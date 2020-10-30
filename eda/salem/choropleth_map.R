library("readxl")
library(ggplot2)
library(maps)
library(dplyr)
library(tools)



data <- read_excel("G:/Cloud Computing/SOTA_FL.xlsx")


names(data)[names(data) == "Adult Asthma sufferers"] <- "Asthma_sufferers"

data <- data %>% 
  mutate(Adult_Asthma_sufferers = Asthma_sufferers / 1900)

names(data)[names(data) == "Asthma_sufferers"] <- "Adult Asthma sufferers"



all_counties<- map_data("county", 'florida')

states<-aggregate(Adult_Asthma_sufferers~ County, data, mean)

names(states)[names(states) == "County"] <- "subregion"



states$subregion <- toTitleCase(states$subregion)
all_counties$subregion <- toTitleCase(all_counties$subregion)


states$subregion <- gsub("[.]", "", states$subregion)




total<-merge(all_counties,states,y.by="subregion")
total$subregion<-factor(total$subregion)
total <- total[order(total$order),] 


p<-ggplot(total, aes(long, lat, group=group, fill=Adult_Asthma_sufferers)) +
  geom_polygon()
p 


cnames <- aggregate(cbind(long, lat) ~ subregion, data=all_counties, 
                    FUN=function(x)mean(range(x)))





statenames<-data.frame(
  subregion=levels(total$subregion)
) 

# Merge it with centroids
centroids<-merge(statenames,cnames,by="subregion")





p2<-p+
  with(centroids, 
       annotate(geom="text", x = long, y=lat, label = subregion, 
                size = 4,color="white",family="Times")
  )
p2





p3<-p2+
  # change color scheme
  scale_fill_continuous(
    low = "cornflowerblue",high = "darkblue",
    guide=guide_colorbar(barwidth = 2,barheight = 10))+
  # add titles
  labs(fill = "1.9k * Sufferers")+
  ggtitle("Adult Asthma sufferers in each County")+
  # hide ticks on x and y axis
  scale_y_continuous(breaks=c())+ 
  scale_x_continuous(breaks=c()) 






theme_g<-
  theme(panel.background = element_rect(fill = "grey"),
        plot.background = element_rect(fill = "grey"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        panel.border =  element_blank(),
        plot.title = element_text(
          size = 15, hjust = 0.5, family = "Times",colour = "white"),
        legend.title= element_text(
          hjust = 0.4 ,vjust=0.3, size=10,family = "Times"),
        legend.text = element_text(
          hjust = 0.4 ,vjust=2, size=8,family = "Times")
  )



p3+
  theme_g
