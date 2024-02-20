library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(clifro)

# setwd("C:/Users/pielter/LiveData_RShiny")

ui <- fluidPage(
  titlePanel("Live Weather Data"),
  h4("Please note that this data is provisional and is not checked for quality."),
  h5(textOutput("update_ts")),
  fluidPage(tabsetPanel(
    tabPanel("Marshview Farm",
             dateRangeInput("MVdaterange", "Pick a Time Period",
                            # value = today(),
                            start = now() - days(7),
                            end = now(),
                            min = now() - days(365),
                            max = now()),
             fluidRow(
               column(6,
                      h3("Temperature"),
                      plotOutput("tempplot", width = "100%"),
                      h3("Precipitation"),
                      plotOutput("precipplot", width = "100%"),
                      h3("Photosynthetically Active Radiation"),
                      plotOutput("parplot", width = "100%"),
                      h3("Wind Speed"),
                      plotOutput("wsplot", width = "100%")
               ),
               column(6,
                      h3("Relative Humidity"),
                      plotOutput("rhplot", width = "100%"),
                      h3("Barometric Pressure"),
                      plotOutput("barplot", width = "100%"),
                      h3("Shortwave Radiation"),
                      plotOutput("radplot", width = "100%"),
                      h3("Wind Direction"),
                      plotOutput("wdplot", width = "100%")
               )
             )),
    tabPanel("Ipswich Bay Yacht Club",
             dateRangeInput("IBdaterange", "Pick a Time Period",
                            # value = today(),
                            start = now() - days(7),
                            end = now(),
                            min = now() - days(365),
                            max = now()),
             h3("Water Level"),
             plotOutput("radarplot"),
             fluidRow(
               column(6,
                      h3("Wind Speed"),
                      plotOutput("windplot")
               ),
               column(6,
                      h3("Wind Direction"),
                      plotOutput("dirplot", width = "100%")
               ),
             ))
  )
  )
)
  
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
    MFM_raw$DewPt <- as.numeric(MFM_raw$DewPt)
    
    IBYC <- reactive({
      IBYC_raw %>% 
        filter(Timestamp >= input$IBdaterange[1] & Timestamp <= input$IBdaterange[2])
    })  
    MFM <- reactive({
      MFM_raw %>% 
        filter(Timestamp >= input$MVdaterange[1] & Timestamp <= input$MVdaterange[2])
    })   
    
    output$tempplot <- renderPlot({
      ggplot(MFM(), aes(ymd_hms(Timestamp), Temp)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = "Temperature (°C)") +
        theme_classic(base_size = 20)
    })
    output$rhplot <- renderPlot({
      ggplot(MFM(), aes(ymd_hms(Timestamp), RH)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = "Humidity (%)") +
        theme_classic(base_size = 20)
    })
    output$precipplot <- renderPlot({
      ggplot(MFM(), aes(ymd_hms(Timestamp), Precip)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = "Precipitation (mm)") +
        theme_classic(base_size = 20)
    })
    output$barplot <- renderPlot({
      ggplot(MFM(), aes(ymd_hms(Timestamp), BAR)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = "Pressure (mbar)") +
        theme_classic(base_size = 20)
    })
    output$parplot <- renderPlot({
      ggplot(MFM(), aes(ymd_hms(Timestamp), PAR)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = expression(paste("PAR (umol/", m^2, "/s)"))) +
        theme_classic(base_size = 20)
    })    
    output$radplot <- renderPlot({
      ggplot(MFM(), aes(ymd_hms(Timestamp), Pyranometer)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = expression(paste("Shortwave Radiation (kW/", m^2, ")"))) +
        theme_classic(base_size = 20)
    })
    output$wsplot <- renderPlot({
      ggplot(MFM(), aes(ymd_hms(Timestamp), Wind)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = "Wind Speed (m/s)") +
        theme_classic(base_size = 20)
    })
    output$wdplot <- renderPlot({
      with(MFM(), windrose(Wind, WindDir, 
                            speed_cuts = c(2.5, 5, 7.5, 10),
                            col_pal = "Spectral", 
                            legend_title = "Wind Speed (m/s)")) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 20) + 
        theme(legend.position = "bottom", 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 10))
    })
    output$radarplot <- renderPlot({
      ggplot(IBYC(), aes(ymd_hms(Timestamp), Water_Level)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = "Water Level (m)") +
        theme_classic(base_size = 20)
    })
    output$windplot <- renderPlot({
      ggplot(IBYC(), aes(ymd_hms(Timestamp), Wind)) + geom_line() +
        labs(x = "Timestamp (EST)", 
             y = "Wind Speed (m/s)") +
        theme_classic(base_size = 20)
    })
    output$dirplot <- renderPlot({
      with(IBYC(), windrose(Wind, WindDir, 
                            speed_cuts = c(2.5, 5, 7.5, 10),
                            col_pal = "Spectral", 
                            legend_title = "Wind Speed (m/s)")) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 20) + 
        theme(legend.position = "bottom", 
              legend.text = element_text(size = 8),
              legend.title = element_text(size = 10))
    })
    output$update_ts <- renderText({
      paste("Last Updated:", last(IBYC_raw$Timestamp), "EST", sep = " ")
    })
  }
  
  shinyApp(ui, server)
  
  
  