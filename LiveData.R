library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shinyBS)

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
             plotOutput("tempplot", hover = "plot_hover"),
             plotOutput("rhplot"),
             plotOutput("precipplot"),
    ),
    tabPanel("Ipswich Bay Yacht Club",
             dateRangeInput("IBdaterange", "Pick a Time Period",
                            # value = today(),
                            start = now() - days(7),
                            end = now(),
                            min = now() - days(365),
                            max = now()),
             plotOutput("radarplot"),
             plotOutput("windplot"),
             plotOutput("dirplot"),
    )
  ))
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
      labs(title = "Temperature", x = "Timestamp (EST)", y = "Temperature (C)") +
      theme_classic(base_size = 20)
  })
  output$rhplot <- renderPlot({
    ggplot(MFM(), aes(ymd_hms(Timestamp), RH)) + geom_line() +
      labs(title = "Humidity", x = "Timestamp (EST)", y = "Relative Humidity (%)") +
      theme_classic(base_size = 20)
  })
  output$precipplot <- renderPlot({
    ggplot(MFM(), aes(ymd_hms(Timestamp), Precip)) + geom_line() +
      labs(title = "Precipitation", x = "Timestamp (EST)", y = "Precipitation (mm)") +
      theme_classic(base_size = 20)
  })
  output$windplot <- renderPlot({
    ggplot(IBYC(), aes(ymd_hms(Timestamp), Wind)) + geom_line() +
      labs(title = "Wind Speed", x = "Timestamp (EST)", y = "Wind Speed (m/s)") +
      theme_classic(base_size = 20)
  })
  output$radarplot <- renderPlot({
    ggplot(IBYC(), aes(ymd_hms(Timestamp), Water_Level)) + geom_line() +
      labs(title = "Water Level", x = "Timestamp (EST)", y = "Water Level (m)") +
      theme_classic(base_size = 20)
  })
  output$dirplot <- renderPlot({
    # with(IBYC(), windrose(Wind, WindDir))
    ggplot(IBYC(), aes(ymd_hms(Timestamp), WindDir)) + geom_line() +
      labs(title = "Wind Direction", x = "Timestamp (EST)", y = "Wind Direction (degrees)") +
      theme_classic(base_size = 20)
  })
  output$update_ts <- renderText({
    paste("Last Updated:", last(IBYC_raw$Timestamp), "EST", sep = " ")
  })
}

shinyApp(ui, server)


