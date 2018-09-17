library(shiny)
library(shinythemes)
library(tidyverse)
library(httr)
library(leaflet)
library(plotly)
library(DT)
library(htmltools)
library(lubridate)
library(formattable)

#Get data
petrol_data <- GET("http://cac.gov.jm/dev/SurveyEnquiry/PetrolPrices.php?Key=e8189538-d0ca-4899-adae-18f454eca9f9") %>%
  content() %>%
  mutate(LocLongitude = as.numeric(LocLongitude),
         LocLatitude = as.numeric(LocLatitude) )

# Define UI for application that displays results of CAC Monthly Petrol Survey
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  # Application title
  title = "CAC Petrol Pries App",
  titlePanel(title = div(img(src="logo.jpg"),
                         "CAC Petrol Survey")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h2(strong("Survey Results")),
      
      selectInput("date",
                "Choose a date",
                petrol_data$StartDate
      ),
      
      br(),
      
      radioButtons(
        inputId = "fuel",
        label = "Select the fuel type",
        choices = c("87" = "E-10 GASOLENE - 87 OCTANE NONE 1 L",
                    "90" = "E-10 GASOLENE - 90 OCTANE NONE 1 L",
                    "Diesel" = "AUTO DIESEL NONE 1 L",
                    "ULSD" = "ULTRA LOW SULPHUR DIESEL(ULSD) NONE 1 L")
      ),
      
      br(),
      
      actionButton("price","View Prices"),
      
      hr()
      ),
    
    # Show a results of data request
    mainPanel(
      
      tabsetPanel(
        type = "tabs",
        tabPanel("Map",
                 fluidRow(leafletOutput("mapPlot"))
                 ),
        
        tabPanel("Trend",
                 h2(strong("Trend in Price")),
                 plotlyOutput("chart"),
                 p("1. Choose your fuel type in the sidebar"),
                 p("2. Select which data points you'd like to visualize in the chart 
                   by clicking on the item in the legend"),
                 p("3. Use the tooltip at above the chart for added features.")
                 ),
        
        tabPanel("Table",
                 dataTableOutput("table"))
        )
      )
)
  )


# Define server logic required to display data output
server <- function(input, output) {
  
  #Prices app
  observeEvent(input$price,{
    #Generate reactive data for mapPlot
    map_data <- reactive({
      map_data <- petrol_data%>%
        filter(year(StartDate) == year(input$date) &
                 month(StartDate) == month(input$date),
               ItemName == input$fuel,
               Price > 0)
      return(map_data)
    })
    
    #Map Creation
    output$mapPlot <- renderLeaflet({
      # set colours for legend
      pal <- colorBin(
        palette = c("#00FF00", "#FFFF00", "#FF0000"),
        domain = map_data()$Price,
        bins = 5,
        pretty = T
      )
      
      #draw map
      leaflet(width = 400) %>%
        setView(lng = -77.29751,
                lat = 18.10958,
                zoom = 9) %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        addCircleMarkers(lng = as.numeric(map_data()$LocLongitude),
                         lat = as.numeric(map_data()$LocLatitude),
                         layerId = map_data()$Price,
                         stroke =T,
                         fillOpacity = 0.8,
                         fillColor = pal(map_data()$Price),
                         popup = htmlEscape(paste(map_data()$MerchantName,
                                                  map_data()$MerchantTown,
                                                  " sold ",
                                                  map_data()$ItemName,
                                                  " for $",
                                                  as.character(map_data()$Price),
                                                  "/L",
                                                  ", on ",
                                                  map_data()$StartDate))
        )%>%
        addLegend("bottomleft",
                  pal = pal,
                  values = map_data()$Price,
                  title = "Observed Price (Ja$/L)")
    })
    
    #Table Tab output
    output$table <- renderDataTable({
      dt <- map_data() %>%
        select(MerchantName, MerchantTown, Parish, Price) %>%
        group_by(MerchantTown) %>%
        mutate(Price_Rank = rank(Price, na.last = "keep")) %>%
        mutate(PriceMax = max(Price_Rank, na.rm = T),
               PriceMin = min(Price_Rank, na.rm = T)) %>%
        ungroup() %>%
        formattable(list(Price = formatter("span",
                                           style = ~style(color = ifelse(Price_Rank == PriceMin,
                                                                         "green",
                                                                         ifelse(Price_Rank == PriceMax,
                                                                                "red",
                                                                                "black")
                                           ))),
                         PriceMax = F,
                         PriceMin = F,
                         Price_Rank = F)) %>%
        as.datatable()
    })
  })

  
  #Chart analysis
  output$chart <- renderPlotly({
    
    petrol_data%>%
      filter(StartDate <= input$date,
             ItemName == input$fuel,
             Price > 20,
             Price < 200) %>%
      group_by(StartDate) %>%
      mutate(`National Average` = mean(Price, na.rm = T),
             `National Max` = max(Price, na.rm=T),
             `National Min` = min(Price, na.rm = T)) %>%
      ungroup() %>%
      plot_ly(x = ~StartDate) %>%
      add_lines(y = ~`National Average`, 
                name = "National Average") %>%
      add_lines(y = ~`National Max`, 
                name = "National Max") %>%
      add_lines(y = ~`National Min`, 
                name = "National Min") %>%
      add_trace(y = ~Price,
                type = "scatter",
                visible = "legendonly",
                color = ~Parish,
                name = "Station Price",
                text = ~paste(MerchantName,
                              MerchantTown,
                              "$",Price, "/l"),
                alpha = 0.5) %>%
      layout(yaxis = list(title = "Price (Ja$/L)"),
             xaxis = list(title = "Date",
                          rangeslider = list(
                            type = "date"
                          )
             )
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
