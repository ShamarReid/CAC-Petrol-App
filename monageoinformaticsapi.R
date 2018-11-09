library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)
library(htmltools)
library(RMySQL)
library(lubridate)

getData <- GET("https://api.jamnav.com/v1/locations/",
  add_headers(Authorization="Token ffe38843ae62cfd41c964c803f0a800acfa6485d")
)%>%
  content(as="parsed")%>%
  toJSON() %>%
  fromJSON()

grocerystores <- getData[["features"]] %>%
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

leaflet() %>%
  setView(lng = -77.29751,
          lat = 18.10958,
          zoom = 9) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(lng = as.numeric(grocerystores$lng),
                   lat = as.numeric(grocerystores$lat),
                   stroke =T,
                   fillOpacity = 0.8,
                   popup = htmlEscape(paste(grocerystores$properties.name,
                                            grocerystores$properties.address)
                                      )
  )

# Get data from CAC Server
CAC_con <- dbConnect(MySQL(),
                     dbname = "survey",
                     host = "192.168.0.23",
                     port = 3306,
                     user = "research",
                     password = "cacr123-")
grocerydata <- DBI::dbGetQuery(CAC_con, 
                               "SELECT sf.ID, 
                             sf.SurveyTitle,
                            sf.SurveyDate,
                            sf.MerchantID,
                            m.OutletType,
                            sf.MerchantName,
                            sf.MerchantTown, 
                            sf.MerchantParish,
                            pp.ProductDescID, 
                            pp.ProductDesc,
                            br.BrandName,
                            sd.Description, 
                            pp.Price
                            FROM  survey.vSurveyForms sf 
                            JOIN survey.vProdPrices pp ON sf.ID = pp.SurveyFormID
                            JOIN tblMerchant m ON sf.MerchantID = m.ID
                            JOIN tblBrand br ON pp.BrandID = br.ID
                            JOIN tblSizeDesc sd ON pp.SizeDescID = sd.ID
                            WHERE sf.SurveyTypeDesc='Grocery'")
dbDisconnect(CAC_con)

cacStEliz <- grocerydata %>%
  filter(MerchantParish == "St. Elizabeth" & MerchantTown == "BLACK RIVER") %>%
  select(MerchantName) %>%
  unique()