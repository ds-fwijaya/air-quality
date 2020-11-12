data<-read.csv("SOTA-2020_VA.csv")
data$County<-as.character(data$County)

data$County[-4]<-gsub(" City","",data$County[-4])
graphdata<-data[,c("County","Pollutant.Grade","Ozone.Grade")]

library(raster)
library(leaflet)

USA <- getData("GADM", country = "usa", level = 2)
virginia<-USA[USA$NAME_1=="Virginia",]


virginia<-merge(virginia, graphdata, by.x="NAME_2", by.y="County")



virginia$Pollutant.Grade<-ifelse(is.na(virginia$Pollutant.Grade),"",as.character(virginia$Pollutant.Grade))
virginia$Pollutant.Grade<-as.factor(virginia$Pollutant.Grade)
virginia$Ozone.Grade<-ifelse(is.na(virginia$Ozone.Grade),"",as.character(virginia$Ozone.Grade))
virginia$Ozone.Grade<-as.factor(virginia$Ozone.Grade)



factpal <- colorFactor(c("grey",topo.colors(2)), virginia$Pollutant.Grade)


pollutant_map<-leaflet(virginia) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, 
              fillOpacity = 0.7, fillColor = ~factpal(virginia$Pollutant.Grade),
              popup = ~NAME_2) %>%
  addLegend(values = levels(virginia$Pollutant.Grade)[-1],
            pal = factpal,
            opacity = 0.7, title = "Pollutant Grade", position = "topright")



factpal2 <- colorFactor(c("grey",topo.colors(5)), virginia$Ozone.Grade)


ozone_map<-leaflet(virginia) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, 
              fillOpacity = 0.7, fillColor = ~factpal2(virginia$Ozone.Grade),
              popup = ~NAME_2) %>%
  addLegend(values = levels(virginia$Ozone.Grade)[-1],
            pal = factpal2,
            opacity = 0.7, title = "Ozone Grade", position = "topright")







#Shiny App

library(shiny)



ui <- fluidPage( 
  titlePanel("Virginia Map"),
  leafletOutput("mymap"),
  p(),
  radioButtons(inputId = "pol_oz",
               label = "Factor:",
               choices = c("Pollutant","Ozone"),
               selected = "Pollutant")
)

server <- function(input, output, session) {
  
  
  output$mymap <-   renderLeaflet({
    
    if (input$pol_oz=="Pollutant"){
      
      factpal <- colorFactor(c("grey",topo.colors(2)), virginia$Pollutant.Grade)
      
      
      pollutant_map<-leaflet(virginia) %>%
        addTiles() %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, 
                    fillOpacity = 0.7, fillColor = ~factpal(virginia$Pollutant.Grade),
                    popup = ~NAME_2) %>%
        addLegend(values = levels(virginia$Pollutant.Grade)[-1],
                  pal = factpal,
                  opacity = 0.7, title = "Pollutant Grade", position = "topright")
      
      
    } else {
      
      if (input$pol_oz=="Ozone"){
        
        factpal2 <- colorFactor(c("grey",topo.colors(5)), virginia$Ozone.Grade)
        
        
        ozone_map<-leaflet(virginia) %>%
          addTiles() %>%
          addProviderTiles("Esri.WorldGrayCanvas") %>%
          addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, 
                      fillOpacity = 0.7, fillColor = ~factpal2(virginia$Ozone.Grade),
                      popup = ~NAME_2) %>%
          addLegend(values = levels(virginia$Ozone.Grade)[-1],
                    pal = factpal2,
                    opacity = 0.7, title = "Ozone Grade", position = "topright")
        
        
      }  
      
      
      
      
    }
    
  })
  
  
}

shinyApp(ui, server)
