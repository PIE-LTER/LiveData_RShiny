library(shiny)
library(dplyr)
library(lubridate)
# library(ggplot2)
library(plotly)

# setwd("C:/Users/pielter/LiveData_RShiny")

#### UI ####
ui <- fluidPage(
  # titlePanel("Live Weather Data"),
  # h4("Please note that this data is provisional and is not checked for quality."),
  h5(textOutput("update_ts")),
  fluidPage(tabsetPanel(
    tabPanel("Marshview Farm",
             h2("Marshview Farm"),
             h4("PIE LTER weather station at Marshview Farm Field Station, Newbury, MA."),
             dateRangeInput("MVdaterange", "Pick a Time Period",
                            # value = today(),
                            start = now() - days(7),
                            end = now(),
                            min = now() - days(365),
                            max = now()),
             fluidRow(
               column(6,
                      h3("Temperature"),
                      plotlyOutput("tempplot"),
                      h3("Precipitation"),
                      plotlyOutput("precipplot"),
                      h3("Photosynthetically Active Radiation"),
                      plotlyOutput("parplot"),
                      h3("Wind Speed"),
                      plotlyOutput("wsplot")
               ),
               column(6,
                      h3("Relative Humidity"),
                      plotlyOutput("rhplot"),
                      h3("Barometric Pressure"),
                      plotlyOutput("barplot"),
                      h3("Shortwave Radiation"),
                      plotlyOutput("radplot"),
                      h3("Wind Direction"),
                      plotlyOutput("wdplot", width = "100%")
               )
             )),
    tabPanel("Ipswich Bay Yacht Club",
             h2("Ipswich Bay Yacht Club"),
             h4("PIE LTER weather station on the Ipswich Bay Yacht Club pier, Ipswich, MA."),
             dateRangeInput("IBdaterange", "Pick a Time Period",
                            # value = today(),
                            start = now() - days(7),
                            end = now(),
                            min = now() - days(365),
                            max = now()),
             h3("Water Level"),
             plotlyOutput("radarplot"),
             fluidRow(
               column(6,
                      h3("Wind Speed"),
                      h4("The wind sensor is currently down for repairs. We apologize for the inconvenience.")
                      # plotlyOutput("windplot")
               ),
               column(6,
                      h3("Wind Direction"),
                      h4("The wind sensor is currently down for repairs. We apologize for the inconvenience.")
                      # plotlyOutput("dirplot", width = "100%")
               ),
             ))
  )
  )
)

#### Server ####
server <- function(input, output, session) {
  # read files
  IBYC_raw <- read.delim("CR1000XSeries_IBYC_raw.dat", sep = ",", header = F, skip = 4)
  names(IBYC_raw) <- c("Timestamp",
                       "Record",
                       "Battery",
                       "Temp",
                       "Wind",
                       "WindDir",
                       "Wind_Max",
                       "T_Wind_Max",
                       "Water_Level",
                       "Water_Level_NAVD88",
                       "Radar_Distance",
                       "Radar_Status")
  IBYC_raw$Timestamp <- ymd_hms(IBYC_raw$Timestamp)
  IBYC_raw$Water_Level <- as.numeric(IBYC_raw$Water_Level)
  IBYC_raw$Water_Level[IBYC_raw$Water_Level <= 0] <- NA
  
  MFM_raw <- read.delim("CR1000XSeries_MFM_raw.dat", sep = ",", header = F, skip = 4)
  names(MFM_raw) <- c("Timestamp",
                      "Record",
                      "Temp",
                      "RH",
                      "Wind",
                      "Wind_Max",
                      "T_Wind_Max",
                      "WindDir",
                      "Pyranometer",
                      "Pyr_TOT",
                      "DewPt",
                      "PAR",
                      "BAR",
                      "Precip",
                      "Battery",
                      "PTemp")
  MFM_raw$Timestamp <- ymd_hms(MFM_raw$Timestamp)
  MFM_raw$DewPt <- as.numeric(MFM_raw$DewPt)
  
  IBYC <- reactive({
    IBYC_raw %>% 
      filter(Timestamp >= input$IBdaterange[1] & Timestamp <= (input$IBdaterange[2] + hours(23) + minutes(59)))
  })  
  MFM <- reactive({
    MFM_raw %>% 
      filter(Timestamp >= input$MVdaterange[1] & Timestamp <= (input$MVdaterange[2] + hours(23) + minutes(59)))
  })   
  
  # Put wind data into format expected by plotly
  # figure out the wind direction bins
  dirres = 30
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c("N", "N-NE", "E-NE", "E", "E-SE", "S-SE", "S", "S-SW", "W-SW", "W", "W-NW", "N-NW", "N")
  
  IBYC_wind <- select(IBYC_raw, Timestamp, Wind, WindDir)
  IBYC_wind$nms <- rep(NA)
  # assign wind speed bins
  IBYC_wind$nms[IBYC_wind$Wind >= 0 & IBYC_wind$Wind <= 2.5] <- "0-2.5"
  IBYC_wind$nms[IBYC_wind$Wind > 2.5 & IBYC_wind$Wind <= 5] <- "2.5-5"
  IBYC_wind$nms[IBYC_wind$Wind > 5 & IBYC_wind$Wind <= 7.5] <- "5-7.5"
  IBYC_wind$nms[IBYC_wind$Wind > 7.5 & IBYC_wind$Wind <= 10] <- "7.5-10"
  IBYC_wind$nms[IBYC_wind$Wind > 10] <- "< 10"
  # assign each wind direction to a bin
  dir.binned <- cut(IBYC_wind$WindDir,
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  # assign wind direction bins
  IBYC_wind$t <- dir.binned
  IBYC_wr <- reactive({
    IBYC_wind %>% 
      filter(Timestamp >= input$IBdaterange[1] & Timestamp <= input$IBdaterange[2]) %>% 
      group_by(t, nms) %>% 
      summarise(count = n()) %>% 
      ungroup() %>% 
      mutate(r = 100 * count/sum(count))
  })
  
  MFM_wind <- select(MFM_raw, Timestamp, Wind, WindDir)
  MFM_wind$nms <- rep(NA)
  # assign wind speed bins
  MFM_wind$nms[MFM_wind$Wind >= 0 & MFM_wind$Wind <= 2.5] <- "0-2.5"
  MFM_wind$nms[MFM_wind$Wind > 2.5 & MFM_wind$Wind <= 5] <- "2.5-5"
  MFM_wind$nms[MFM_wind$Wind > 5 & MFM_wind$Wind <= 7.5] <- "5-7.5"
  MFM_wind$nms[MFM_wind$Wind > 7.5 & MFM_wind$Wind <= 10] <- "7.5-10"
  MFM_wind$nms[MFM_wind$Wind > 10] <- "< 10"
  # assign each wind direction to a bin
  dir.binned <- cut(MFM_wind$WindDir,
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  # assign wind direction bins
  MFM_wind$t <- dir.binned
  MFM_wr <- reactive({
    MFM_wind %>% 
      filter(Timestamp >= input$MVdaterange[1] & Timestamp <= input$MVdaterange[2]) %>% 
      group_by(t, nms) %>% 
      summarise(count = n()) %>% 
      ungroup() %>% 
      mutate(r = 100 * count/sum(count)) 
  })   
  
  m <- list(
    l = 0,
    r = 0,
    b = 20,
    t = 20,
    pad = 0
  )
  
  output$tempplot <- renderPlotly({
    plot_ly(MFM(), x = ~Timestamp, y = ~Temp, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "Temperature (C)"))
    # ggplotly(ggplot(MFM(), aes(Timestamp, Temp)) + geom_line() +
    #            labs(x = "Timestamp (EST)",
    #                 y = "Temperature (°C)") +
    #            theme_classic())
  })
  output$rhplot <- renderPlotly({
    plot_ly(MFM(), x = ~Timestamp, y = ~RH, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "Humidity (%)"))
    # ggplotly(ggplot(MFM(), aes(Timestamp, RH)) + geom_line() +
    #            labs(x = "Timestamp (EST)", 
    #                 y = "Humidity (%)") +
    #            theme_classic())
  })
  output$precipplot <- renderPlotly({
    plot_ly(MFM(), x = ~Timestamp, y = ~Precip, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "Precipitation (mm)"))
    # ggplotly(ggplot(MFM(), aes(Timestamp, Precip)) + geom_line() +
    #            labs(x = "Timestamp (EST)", 
    #                 y = "Precipitation (mm)") +
    #            theme_classic())
  })
  output$barplot <- renderPlotly({
    plot_ly(MFM(), x = ~Timestamp, y = ~BAR, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "Pressure (mbar)"))
    # ggplotly(ggplot(MFM(), aes(Timestamp, BAR)) + geom_line() +
    #            labs(x = "Timestamp (EST)", 
    #                 y = "Pressure (mbar)") +
    #            theme_classic())
  })
  output$parplot <- renderPlotly({
    plot_ly(MFM(), x = ~Timestamp, y = ~PAR, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "PAR (umol/m<sup>2</sup>/s)"))
    # ggplotly(ggplot(MFM(), aes(Timestamp, PAR)) + geom_line() +
    #            labs(x = "Timestamp (EST)", 
    #                 y = "PAR (umol/m<sup>2</sup>/s)") +
    #            theme_classic())
  })    
  output$radplot <- renderPlotly({
    plot_ly(MFM(), x = ~Timestamp, y = ~Pyranometer, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "Shortwave Radiation (kW/m<sup>2</sup>)"))
    # ggplotly(ggplot(MFM(), aes(Timestamp, Pyranometer)) + geom_line() +
    #            labs(x = "Timestamp (EST)", 
    #                 y = "Shortwave Radiation (kW/m<sup>2</sup>)") +
    #            theme_classic())
  })
  output$wsplot <- renderPlotly({
    plot_ly(MFM(), x = ~Timestamp, y = ~Wind, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "Wind Speed (m/s))"))
    # ggplotly(ggplot(MFM(), aes(Timestamp, Wind)) + geom_line() +
    #            labs(x = "Timestamp (EST)", 
    #                 y = "Wind Speed (m/s)") +
    #            theme_classic())
  })
  output$wdplot <- renderPlotly({
    plot_ly(MFM_wr(), type = 'barpolar', r = ~r, theta = ~t, color = ~nms) %>%
      # layout(legend = l) %>%
      layout(
        # margin = m,
             legend = list(orientation = 'v', title = list(text='<b>Wind Speed (m/s)</b>')),
             polar = list(angularaxis = list(direction = "clockwise")))
  })
  output$radarplot <- renderPlotly({
    plot_ly(IBYC(), x = ~Timestamp, y = ~Water_Level, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "Water Level (m)"))
    # ggplotly(ggplot(IBYC(), aes(Timestamp, Water_Level)) + geom_line() +
    #            labs(x = "Timestamp (EST)", 
    #                 y = "Water Level (m)") +
    #            theme_classic())
  })
  
  output$windplot <- renderPlotly({
    plot_ly(IBYC(), x = ~Timestamp, y = ~Wind, type = 'scatter', mode = 'lines',
            line = list(color = "black")) %>% 
      layout(xaxis = list(title = "Timestamp"),
             yaxis = list (title = "Wind Speed (m/s)"))
    # ggplotly(ggplot(IBYC(), aes(Timestamp, Wind)) + geom_line() +
    #            labs(x = "Timestamp (EST)", 
    #                 y = "Wind Speed (m/s)") +
    #            theme_classic())
  })
  output$dirplot <- renderPlotly({
    plot_ly(IBYC_wr(), type = 'barpolar', r = ~r, theta = ~t, color = ~nms) %>%
      layout(
        # margin = m,
        legend = list(orientation = 'v', title = list(text = '<b>Wind Speed (m/s)</b>')),
        polar = list(angularaxis = list(direction = "clockwise")))
  })
  output$update_ts <- renderText({
    paste("Last Updated:", last(IBYC_raw$Timestamp), "EST", sep = " ")
  })
}

shinyApp(ui, server)

