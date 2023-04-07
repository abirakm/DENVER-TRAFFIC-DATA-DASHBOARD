library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(leaflet)
library(plotly)
library(lubridate)
library(shinyEffects)
library(shinyWidgets)
library(rintrojs)

#Folder <- 'C:/Abir/Shiny App Collection/Denver Dashboard'
#setwd(Folder)

TrafficData  <- read_csv("Traffic Data.csv") %>%
  mutate(Date = mdy(Date))

Stations <- read_csv("TrafficStations.csv") %>%
  mutate(LocationID = STATIONID) %>%
  select(LocationID, LOCATION, LAT, LONG)

Weather <- read_csv("Denver Weather Data.csv")

Dates <- unique(TrafficData$Date)

Months <- factor(month.name, levels = month.name)


TrafficMonth <- TrafficData %>%
  mutate(
    Month = month.name[month(Date)],
    Day = wday(Date, label = T, abbr = F),
    Total = `NB/EB` + `SB/WB`
  ) %>%
  group_by(LocationID, Date, Day, Month) %>%
  summarise(Total = sum(Total, na.rm = T)) %>%
  group_by(LocationID, Day, Month) %>%
  summarise(Traffic = mean(Total, na.rm = T))

Weekday <- TrafficMonth %>%
  filter(!Day  %in% c('Saturday', 'Sunday')) %>%
  group_by(LocationID, Month) %>%
  summarise(Traffic = round(mean(Traffic, na.rm = T), 0))



ui <- dashboardPagePlus(
  title = 'Sample Traffic Dashboard',
  sidebar_fullCollapse = T,
  dashboardHeaderPlus(
    titleWidth = 0,
    title = 'Sample Traffic Dashboard',
    left_menu = tagList(
      actionBttn(
        'btnTitile',
        strong('Denver Traffic Dashboard'),
        style = 'bordered',
        block = T,
        no_outline = T
      ),
      actionBttn(
        inputId = 'DailyBTN',
        label = 'Daily Trend',
        style = 'stretch',
        no_outline = T
      ),
      actionBttn(
        inputId = 'MonthlyBTN',
        label = 'Monthly Trend',
        style = 'stretch',
        no_outline = T
      )
    ),
    userOutput('AppTitle'),
    userOutput('USERINFO')
  ),
  dashboardSidebar(
    introjsUI(),
    disable = T,
    collapsed = T,
    sidebarMenu(
      id = 'tabs',
      menuItem(text = 'Daily', tabName = 'Daily'),
      menuItem(text = 'Monthly', tabName = 'Monthly')
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "introjs-modern.css")
    ),
    tabItems(
      tabItem(
        tabName = 'Daily',
        boxPlus(
          id = 'DailyMAPBOX',
          status = 'info',
          collapsible = F,
          div(
            id = 'DATESLIDER',
            sliderInput(
              inputId = 'Date',
              min = min(Dates),
              max = max(Dates),
              label = 'Select a Date',
              value = Dates[150],
              width = '100%'
            )
          ),
          leafletOutput(
            outputId = 'MapDaily',
            width = '100%',
            height = 550
          )
        ),
        
        boxPlus(
          id = 'DailyGraphBOX',
          status = 'primary',
          collapsible = F,
          enable_dropdown = T,
          fluidRow(
            valueBoxOutput('VTemparatureMax', width = 3),
            valueBoxOutput('VTemparatureMin', width = 3),
            valueBoxOutput('VPrecipitation', width = 3),
            valueBoxOutput('VCondition', width = 3)
          ),
          fluidRow(
            plotlyOutput('HourlyTRaffic', height = 250, width = '100%'),
            plotlyOutput('HourlyTemperature', height = 250, width = '100%')
          )
        )
      ),
      tabItem(
        tabName = 'Monthly',
        
        boxPlus(
          id = 'MonthlyMAPBOX',
          status = 'info',
          collapsible = F,
          leafletOutput(
            outputId = 'MapMonthly',
            width = '100%',
            height = 550
          )
        ),
        fluidRow(
          boxPlus(
            id = 'MonthlyGraphhBOX',
            status = 'success',
            collapsible = F,
            column(
              2,
              
              radioButtons(
                inputId = 'MonthInput',
                label = 'Month',
                choices = month.name,
                inline = F
              )
              
            ),
            column(10,
                   plotlyOutput('DayPlot', height = 350))
          ),
          boxPlus(
            status = 'danger',
            collapsible = F,
            plotlyOutput('MonthlyPlot', height = 250, width = '100%')
          )
          
        )
      )
    )
  )
)

server <- function(input, output, session) {
  RVAL <- reactiveValues(
    SelectedSensor = 2,
    Date = "",
    Count = "",
    Weather = "",
    MinTemp = "",
    MaxTemp = "",
    Precipitation = "",
    Condition = ""
  )
  
  output$MapDaily <- renderLeaflet({
    isolate({
      InitialSelected <- Stations %>%
        filter(LocationID == RVAL$SelectedSensor)
      
      leaflet() %>% addTiles() %>%
        addCircleMarkers(data = Stations,
                         radius = 5,
                         layerId = ~ LocationID) %>%
        addCircleMarkers(
          group = "Selected",
          lng = InitialSelected$LONG,
          lat = InitialSelected$LAT,
          color = 'yellow',
          radius = 5,
          opacity = 1
        ) %>%
        addControl(html = InitialSelected$LOCATION[1], position = 'topright')
    })
  })
  
  
  output$MapMonthly <- renderLeaflet({
    isolate({
      InitialSelected <- Stations %>%
        filter(LocationID == RVAL$SelectedSensor)
      
      leaflet() %>% addTiles() %>%
        addCircleMarkers(data = Stations,
                         radius = 5,
                         layerId = ~ LocationID) %>%
        addCircleMarkers(
          group = "Selected",
          lng = InitialSelected$LONG,
          lat = InitialSelected$LAT,
          color = 'yellow',
          radius = 5,
          opacity = 1
        ) %>%
        addControl(html = InitialSelected$LOCATION[1], position = 'topright')
    })
  })
  
  
  observeEvent(input$MapDaily_marker_click , {
    event <- input$MapDaily_marker_click
    
    RVAL$SelectedSensor <- event$id
    
    InitialSelected <- Stations %>%
      filter(LocationID == event$id)
    
    ProxyMap <- leafletProxy('MapDaily')
    
    ProxyMap %>%
      clearGroup("Selected") %>%
      addCircleMarkers(
        group = "Selected",
        lng = event$lng,
        lat = event$lat,
        color = 'yellow',
        radius = 5,
        opacity = 1
      ) %>%
      clearControls() %>%
      addControl(html = InitialSelected$LOCATION[1], position = 'topright')
    
    
    ProxyMap1 <- leafletProxy('MapMonthly')
    
    ProxyMap1 %>%
      clearGroup("Selected") %>%
      addCircleMarkers(
        group = "Selected",
        lng = event$lng,
        lat = event$lat,
        color = 'yellow',
        radius = 5,
        opacity = 1
      ) %>%
      clearControls() %>%
      addControl(html = InitialSelected$LOCATION[1], position = 'topright')
    
    
  })
  
  
  observeEvent(input$MapMonthly_marker_click , {
    event <- input$MapMonthly_marker_click
    
    RVAL$SelectedSensor <- event$id
    
    ProxyMap <- leafletProxy('MapDaily')
    
    ProxyMap %>%
      clearGroup("Selected") %>%
      addCircleMarkers(
        group = "Selected",
        lng = event$lng,
        lat = event$lat,
        color = 'yellow',
        radius = 5,
        opacity = 1
      )
    
    ProxyMap1 <- leafletProxy('MapMonthly')
    
    ProxyMap1 %>%
      clearGroup("Selected") %>%
      addCircleMarkers(
        group = "Selected",
        lng = event$lng,
        lat = event$lat,
        color = 'yellow',
        radius = 5,
        opacity = 1
      )
    
    
  })
  
  
  
  
  
  observe({
    try({
      print(input$Date)
      RVAL$Count <- TrafficData %>%
        filter(Date == input$Date &
                 LocationID == RVAL$SelectedSensor)
      
      
      RVAL$Weather <- Weather %>%
        filter(Date == as_date(input$Date))
      
      
    })
    
  })
  
  
  output$HourlyTRaffic <- renderPlotly({
    try({
      MaxCount <- max(RVAL$Count$Count, na.rm = T) + 100
      
      RVAL$Count %>%
        gather(Direction, Count, `NB/EB`, `SB/WB`) %>%
        group_by(Direction) %>%
        plot_ly() %>%
        add_lines(
          x = ~ Hour,
          y = ~ Count,
          color = ~ Direction,
          colors = c('red', 'blue')
        ) %>%
        layout(yaxis = list(range = c( MaxCount,0)),
               xaxis = list(zeroline = TRUE),
               legend = list(x = 0.5, y = 1.2, orientation  ='h'))
    })
    
  })
  
  
  output$HourlyTemperature <- renderPlotly({
    try({
      MinTemp <- min(min(RVAL$Weather$Temperature, na.rm = T), 0)
      MaxTemp <- max(RVAL$Weather$Temperature, na.rm = T) + 5
      
      
      RVAL$Weather %>%
        group_by(Date) %>%
        plot_ly() %>%
        add_lines(
          x = ~ Hour,
          y = ~ Temperature,
          color = ~ Date,
          colors = c('black')
        ) %>%
        layout(yaxis = list(range = c(MinTemp, MaxTemp)))
    })
    
  })
  
  
  output$VTemparatureMax <- renderValueBox({
    valueBox(
      subtitle = 'Max Temperature',
      value = HTML(max(RVAL$Weather$Temperature, na.rm = T), "&deg;", "F"),
      color = 'red',
      icon = icon('sun'),
      width = 2
    )
  })
  
  output$VTemparatureMin <- renderValueBox({
    valueBox(
      subtitle = 'Min Temperature',
      value = HTML(min(RVAL$Weather$Temperature, na.rm = T), "&deg;", "F"),
      color = 'teal',
      icon = icon('moon'),
      width = 2
    )
    
  })
  
  output$VPrecipitation <- renderValueBox({
    valueBox(
      subtitle = 'Max Precepitation',
      value = HTML(max(RVAL$Weather$Precipitation, na.rm = T), 'inch'),
      color = 'aqua',
      icon = icon('tint'),
      width = 2
    )
    
  })
  
  
  output$VCondition <- renderValueBox({
    Condition <- RVAL$Weather %>%
      count(Condition) %>%
      arrange(desc(n))
    
    valueBox(
      subtitle = str_replace(Condition$Condition[1], " ", "\n"),
      color = 'yellow',
      icon = icon('cloud-sun-rain'),
      width = 2,
      value = '_'
    )
    
  })
  
  
  output$DayPlot <- renderPlotly({
    try({
      TempData <<- TrafficMonth %>%
        # filter(LocationID == 2 & Month == 'February')
        filter(LocationID == RVAL$SelectedSensor &
                 Month == input$MonthInput)
      
      TempData %>%
        plot_ly(
          r = ~ Traffic,
          theta = ~ Day,
          type = 'scatterpolar',
          fill = 'toself',
          mode = 'markers'
        )
      
      
      
    })
  })
  
  output$MonthlyPlot <- renderPlotly({
    Weekday %>%
      #filter(LocationID == 2) %>%
      mutate(Month = factor(Month, levels = month.name)) %>%
      filter(LocationID == RVAL$SelectedSensor) %>%
      plot_ly() %>%
      add_bars(x = ~ Month, y = ~ Traffic)
    
  })
  
  output$AppTitle <- renderUser({
    tagList(actionBttn(
      'Help',
      label = 'Help',
      style = 'jelly',
      icon = icon('question')
    ),
    dropdownDivider())
    
  })
  output$USERINFO <-  renderUser({
    dashboardUser(
      name = "A K M Abir",
      src = "https://s3-us-west-2.amazonaws.com/abirprofile/profilepic/MyPhoto.jpg",
      title = '\n Transportation Planner/Modeler',
      footer = tags$a('Persoanl Webpage', target = "_blank", href = 'http://www.akmabir.com')
    )
  })
  
  observeEvent(input$DailyBTN, {
    showTab(inputId = "tabs",
            target = "Daily",
            select = T)
    
  })
  
  observeEvent(input$MonthlyBTN, {
    showTab(inputId = "tabs",
            target = "Monthly",
            select = T)
    
  })
  
  observeEvent(input$Help, {
    if (input$tabs == "Daily") {
      rintrojs::introjs(session, options = list(steps = data.frame(
        element = c(
          NA,
          '#MapDaily',
          '#DATESLIDER',
          '#DailyGraphBOX',
          "#DailyBTN",
          '#MonthlyBTN',
          '#USERINFO'
        ),
        intro = c(
          HTML(
            "This application was developed using Shiny, a R based data visualization framework. This application demonstrates an interaction between map element and graphic element. Please press next to learn details about this application. Though this application was develop using traffic and weather data in Denver,Colorado. It can be used to analyze/visualize similar dataset from other location.",
            '<br />',
            "The traffic data was downloaded from <b>http://dtdapps.coloradodot.info/otis/TrafficData</b>",
            '<br />',
            "The weather data was dowloaded from <b>https://www.wunderground.com</b>"
          ),
          'Click on a map marker to select a location.',
          'Use the slider to select a date.',
          'The traffic and weather information will be updated automatically based on the selected location and date!',
          "Click on the 'Daily Trend' button for traffic and weather trend by day.",
          "Click on the 'Monthly Trend' button for traffic trend by month.",
          "Click here to learn about the author."
        )
      )))
    } else if (input$tabs == "Monthly") {
      rintrojs::introjs(session, options = list(steps = data.frame(
        element = c(
          NA,
          '#MapMonthly',
          '#MonthInput',
          '#DayPlot',
          "#DailyBTN",
          '#MonthlyBTN',
          '#USERINFO'
        ),
        intro = c(
          HTML(
            "This application was developed using Shiny, a R based data visualization framework. This application demonstrates an interaction between map element and graphic element. Please press next to learn details about this application. Though this application was develop using traffic and weather data in Denver,Colorado. It can be used to analyze/visualize similar dataset from other location.",
            '<br />',
            "The traffic data was downloaded from <b>http://dtdapps.coloradodot.info/otis/TrafficData</b>",
            '<br />',
            "The weather data was dowloaded from <b>https://www.wunderground.com</b>"
          ),
          'Click on a map marker to select a location.',
          'Select a month from this list of month.',
          'Average traffic by day of week will be updated for the selected month.',
          "Click on the 'Daily Trend' button for traffic and weather trend by day.",
          "Click on the 'Monthly Trend' button for traffic trend by month.",
          "Click here to learn about the author."
        )
      )))
    }
  })

  session$onSessionEnded(function() {
  stopApp()
  })
  
  
}

shinyApp(ui, server)
