library(readxl)
library(ggplot2)
library(maps)
library(dplyr)
library(tools)



data <- SOTA_2020_VA

names(data)[names(data) == "Ozone Grade"] <- "Ozone_grade"



all_counties<- map_data("county", 'virginia')


states <- data[, c(1,17)]


names(states)[names(states) == "County"] <- "subregion"



states$subregion <- toTitleCase(states$subregion)
all_counties$subregion <- toTitleCase(all_counties$subregion)






total<-merge(all_counties,states,y.by="subregion")
total$subregion<-factor(total$subregion)
total <- total[order(total$order),] 


p<-ggplot(total, aes(long, lat, group=group, fill=Ozone_grade)) +
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
  labs(fill = 'Grade')+
  ggtitle("Ozone Grade")+
  # hide ticks on x and y axis
  scale_y_continuous(breaks=c())+ 
  scale_x_continuous(breaks=c()) 





