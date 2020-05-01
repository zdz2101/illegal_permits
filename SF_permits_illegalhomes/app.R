#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(leaflet)
library(shiny)
library(RColorBrewer)
library(tidyverse)
library(htmltools)
library(RSocrata)
# figure out way to speed up API or automatic way to update csv
# housing_permits = read.socrata("https://data.sfgov.org/resource/p4e4-a5a7.json")
# housing_permits_old = read.socrata("https://data.sfgov.org/resource/4jpb-z4kk.json")
# 
# residential1 = housing_permits %>%
#   select(street_number, street_name, street_suffix, zipcode, neighborhoods_analysis_boundaries, permit_type_definition, description, existing_use, proposed_use, permit_creation_date, location.latitude, location.longitude) 
# 
# residential2 = housing_permits_old %>%
#   select(street_number, street_name, street_suffix, zipcode,neighborhoods_analysis_boundaries, permit_type_definition, description, existing_use, proposed_use, permit_creation_date, location.latitude, location.longitude) 
# 
# house_locations <- rbind(residential1, residential2) %>%
#   unite("full_address", street_number:zipcode, sep = " ") %>%
#   filter(existing_use %in% c("1 family dwelling","2 family dwelling", "apartments") |
#          proposed_use %in% c("1 family dwelling","2 family dwelling", "apartments")) %>%
#   filter(str_detect(description, "illegal|legalize")) %>%
#   mutate(lat = as.numeric(location.latitude),
#          long = as.numeric(location.longitude),
#          text = paste(full_address, description, sep=" - "),
#          text = paste(substring(permit_creation_date,1,4), text, sep = ": "),
#          text2 = paste("Permit created", text, sep =" "),
#          year = as.numeric(substring(permit_creation_date,1,4))) %>%
#   select(text2, lat, long, year, neighborhoods_analysis_boundaries, hometype)%>%
#   filter(is.na(lat) == FALSE & is.na(long) == FALSE)

a = read_csv("single_fam_homes.csv") %>% mutate(hometype = "Single Family")
b = read_csv("two_fam_homes.csv")  %>% mutate(hometype = "Two Family")
c = read_csv("apartments.csv") %>% mutate(hometype = "Apartments")

house_locations = rbind(a,b,c) %>%
  mutate(lat = as.numeric(location.latitude),
         long = as.numeric(location.longitude),
         text = paste(full_address, description, sep=" - "),
         text = paste(substring(permit_creation_date,1,4), text, sep = ": "),
         text2 = paste("Permit created", text, sep =" "),
         year = as.numeric(substring(permit_creation_date,1,4))) %>%
  select(text2, lat, long, year, neighborhoods_analysis_boundaries, hometype)%>%
  filter(is.na(lat) == FALSE & is.na(long) == FALSE)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Year of Permit Creation", min(house_locations$year), max(house_locations$year),
                            value = range(house_locations$year), step = 1),
                selectInput("zones", "Neighborhood",
                            sort(unique(house_locations$neighborhoods_analysis_boundaries)))
                ,
                checkboxGroupInput("home", "Type of Home", sort(unique(house_locations$hometype)))
                )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    house_locations[house_locations$year >= input$range[1] &
                    house_locations$year <= input$range[2] &
                    house_locations$neighborhoods_analysis_boundaries %in% input$zones &
                    house_locations$hometype %in% input$home, ]
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(house_locations) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map", data = filteredData())  %>%
      clearMarkers() %>%
      addMarkers(~long, ~lat, popup = ~htmlEscape(text2))
  })
}
shinyApp(ui, server)
