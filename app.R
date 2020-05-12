# Title     : TODO
# Objective : TODO
# Created by: wahid
# Created on: 2020-05-11

library(dplyr)
library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(sf)

#Load the dataset
canada_coronavirus <- read_csv( "cases_timeseries_hr.csv")
################

## DATA CLEANUP

#Remove the health_region column
canada_coronavirus <- canada_coronavirus %>%
  select(-health_region)
################

#Convert the date_report as a date variable
canada_coronavirus$date_report <- as.Date(canada_coronavirus$date_report, "%d-%m-%Y")
###############

#Remove province = "Repatriated"
canada_coronavirus_filtered <- canada_coronavirus %>%
  filter(!province == "Repatriated")
###############

#Count number of cumulative for on single date for a particular province
canada_coronavirus_final <- canada_coronavirus_filtered %>%
  group_by(province, date_report) %>%
  arrange(province, date_report) %>%
  mutate(cumulative_cases = sum(cumulative_cases), cases = sum(cases)) %>%
  distinct(date_report, .keep_all = TRUE)
################

#Longitude and Latitude info for the provinces
long_lat_info <- data.frame(
  province  = names(table(canada_coronavirus$province))[-11],
  longitude = c(-115.0, -127.647621, -98.813873, -66.159668, -57.660435, -63.0, -114.371788,
                -85.0, -63.0, -70.0, -106.0, -135.000000),
  latitude = c(	55.000000, 53.726669, 53.760860, 46.498390, 53.135509, 45.000000, 62.453972,
                50.000000, 46.250000, 53.000000, 55.000000, 64.000000)
)
##################

#Add longitude and latitude columns to the corona_virus_final dataframe
canada_coronavirus_final <- canada_coronavirus_final %>%
  merge(long_lat_info)
##################

## SHINY

# Minimum and Current date for slider input
min_date <- min(canada_coronavirus_final$date_report)
current_date <- max(canada_coronavirus_final$date_report)
##############

# User interface
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = FALSE,
             "COVID-19 tracker", id = "nav",

             tabPanel("COVID-19 mapper",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("cumulative", width="100%", height="100%"),

                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",

                                        span(tags$i(h4("Coronavirus cases in Canada")), style="color:#045a8d"),
                                        sliderInput("plot_date",
                                                    label = h5("Select mapping date"),
                                                    min = as.Date(min_date,"%Y-%m-%d"),
                                                    max = as.Date(current_date,"%Y-%m-%d"),
                                                    value = as.Date(current_date),
                                                    timeFormat = "%d %b",
                                                    animate=animationOptions(interval = 1000, loop = FALSE))
                          )
                      )
             )
  )
)
###############

# Server side
server <- function(input, output){
  
  #Plot cumulative data on the filtered date
  output$cumulative <- renderLeaflet({
    date_filtered <- canada_coronavirus_final %>%
      filter(date_report == as.Date(input$plot_date))
                      
    date_filtered %>%
        leaflet() %>%
        addProviderTiles(providers$Esri) %>%
        addCircleMarkers(radius = ~(cumulative_cases)^(1/2.3), color = "#cc4c02",
                        label = sprintf("<strong>Province: %s</strong><br/>Cumulative cases: %g", date_filtered$province, date_filtered$cumulative_cases) %>% lapply(htmltools::HTML),
                        labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#cc4c02"),
                        textsize = "15px", direction = "auto"),
                        group = "Cumulative") %>%
      addCircleMarkers(radius = ~(cases)^(1/2), color = "#016c59",
                       label = sprintf("<strong>Province: %s</strong><br/>New Cases: %g", date_filtered$province, date_filtered$cases) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = "#cc4c02"),
                         textsize = "15px", direction = "auto"),
                       group = "New") %>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Cumulative", "New"),
        options = layersControlOptions(collapsed = FALSE))
  })
  ##########################
}
###############

# Run application
shinyApp(ui = ui, server = server)
################