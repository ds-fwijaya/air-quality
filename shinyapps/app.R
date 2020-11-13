# importing packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(leafpop))
suppressPackageStartupMessages(library(readr))

#########################################################
# data aggregation
#########################################################
shape <- tigris::counties(state = "VA", class = "sf")
County_Transportation_Profiles <- read_csv("data/processed/counties_mapping_data_simplified.rds")

shape$GEOID <- as.numeric(shape$GEOID)

shape <- select(
    shape,
    STATEFP,
    COUNTYFP,
    GEOID,
    NAMELSAD,
    COUNTYNS,
    geometry
)

Transit <- County_Transportation_Profiles %>%
    rename(
        GEOID = `County FIPS`
    )

Transit$GEOID <- as.numeric(Transit$GEOID)

Transit <- select(
    Transit, 
    GEOID, 
    `Number of workers from other counties who commute to work in the county`,
    `Number of resident workers who commute to work in other counties`,
    `Number of resident workers who commute within county`,
    `Number of residents`
)

y <- left_join(shape,Transit, by = "GEOID")

SOTA_data <- sota_fips %>% 
    rename(
        GEOID = County_FIPS
    )

SOTA <- left_join(y, SOTA_data, by = "GEOID")

#nei_virginia_highway$GEOID <- with(nei_virginia_highway, paste0(STATE_FIPS,COUNTY_FIPS))

#nei_virginia_offhighway$GEOID <- with(nei_virginia_offhighway, paste0(STATE_FIPS,COUNTY_FIPS))

full_data <- SOTA %>%
    rename(
        commute_within = `Number of resident workers who commute within county`,
        commute_into = `Number of workers from other counties who commute to work in the county`,
        commute_out = `Number of resident workers who commute to work in other counties`,
        num_residents = `Number of residents`
    )

full_data <- mutate(
    full_data,
    smoker_ratio = `Ever Smoked`/`Total Population`,
    pediatric_asthma_ratio = `Pediatric Asthma sufferers`/`Total Population`,
    adult_asthma_ratio = `Adult Asthma sufferers`/`Total Population`
)


# Define UI 
ui <- fluidPage(
    
    # Application title
    titlePanel("Virginia County Polution Information"),
    
    # Choose data to present
    varSelectInput("Grade", "What grade would you like to View?", tibble("Pollutant Grade" = full_data$`Pollutant Grade`,
                                                                         "Ozone Grade" = full_data$`Ozone Grade`,
                                                                         "Annual Pass/Fail" = full_data$`Annual Pass/Fail`)),
    
    # Top panel with county name
    verticalLayout(
        
        wellPanel(textOutput("cnty")),
        
        # the map itself
        mainPanel(
            leafletOutput("map")
        )
    )
)

server <- function(input, output) {
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addProviderTiles("Stamen.Toner") %>% 
            addPolygons(data = shape,
                        label = ~NAMELSAD,
                        fillColor = "aliceblue", 
                        color = "grey",
                        layerId = ~COUNTYNS,
                        popup = paste("County:", full_data$County,"<br>",
                                      "Selected Grade:", full_data[[input$Grade]] ,"<br>",
                                      "Amount of People Who Commute In:", full_data$commute_into,"<br>",
                                      "Amount of People who Commute Out:", full_data$commute_out,"<br>",
                                      "Amount of People who Commute Within:", full_data$commute_within))
    })
    
    
    observe({ 
        event <- input$map_shape_click
        output$cnty <- renderText(shape$NAMELSAD[shape$COUNTYNS == event$id])
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)