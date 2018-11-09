# Libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(formattable)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)

# Get Petrol data
petrol_data <- GET("http://cac.gov.jm/dev/SurveyEnquiry/PetrolPrices.php?Key=e8189538-d0ca-4899-adae-18f454eca9f9") %>%
  content() %>%
  filter(!is.na(Price),
         Price > 0,
         Price < 400) %>%
  mutate(LocLongitude = as.numeric(LocLongitude),
         LocLatitude = as.numeric(LocLatitude))

dates <- petrol_data$StartDate %>%
  unlist() %>%
  unique()

getGroceryData <- GET("https://api.jamnav.com/v1/locations/",
               add_headers(Authorization="Token ffe38843ae62cfd41c964c803f0a800acfa6485d")
)%>%
  content(as="parsed")%>%
  toJSON() %>%
  fromJSON()

grocerystores <- getGroceryData[["features"]] %>%
  flatten() %>%
  mutate(grocery = map(properties.categories, str_detect,
                       pattern = "Grocery Store"),
         grocery = map_lgl(grocery, any),
         lng = map(geometry.coordinates, pluck, 1) %>% 
           unlist(),
         lat = map(geometry.coordinates, pluck, 2) %>% 
           unlist(),
         store = unlist(properties.name)) %>%
  filter(grocery == T)
