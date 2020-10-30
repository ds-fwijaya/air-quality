data <- SOTA_2020_VA

library(sf)
library(ggplot2)
library(ggmap)
library(MAP)
library(mapdata)

#virginia <- map_data("usa")

#west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

#library(maps)
#map("state", "Virginia")
#data(us.cities)
#tempdat= us.cities[us.cities$country.etc == "VA", ]
#tempdat$temperature = round(rnorm(nrow(tempdat), mean=75, sd=5))
#text(tempdat$long, tempdat$lat, label=tempdat$temperature)

################################################################################################################

## A good starting point found at:
## https://community.rstudio.com/t/r-shiny-make-a-reactive-map-of-state-by-county/63224/3

library(shiny)
library(leaflet)
library(sf)

# VA counties - downloaded via the awesome tigris package
shape <- tigris::counties(state = "VA", class = "sf")

# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("The Old Dominion"),
  
  # Top panel with county name
  verticalLayout(
    
    wellPanel(textOutput("cnty")),
    
    # the map itself
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic       
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Toner") %>% 
      addPolygons(data = shape, 
                  fillColor = "aliceblue", 
                  color = "grey",
                  layerId = ~COUNTYNS)
  })
  
  # this is the fun part!
  observe({ 
    event <- input$map_shape_click
    output$cnty <- renderText(shape$NAMELSAD[shape$COUNTYNS == event$id])
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

## This map seems to have more counties than we have information for.
################################################################################################################

devtools::install_github("UrbanInstitute/urbnmapr")
#library(tidyverse)
library(urbnmapr)

ggplot() + 
  geom_polygon(data = urbnmapr::states, mapping = aes(x = long, y = lat, group = group),
               fill = "grey", color = "white") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

countydata %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name =="Virginia") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = .25) +
  scale_fill_gradientn(labels = scales::percent,
                       guide = guide_colorbar(title.position = "top")) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") +
  theme_urban_map()
