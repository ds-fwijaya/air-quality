

library(tidyverse)
library(dplyr)
library(stats)
library(ggplot2)

library(readr)
VA_airdata <- read_csv("C:/Users/User/Desktop/CDS490/SOTA-2020_VA.csv")
View(VA_airdata)

str(VA_airdata)

summary(VA_airdata)

colSums(is.na(VA_airdata)) #checking which caloums have how many 
#NA values

#removing all the NA values
new_VA_airdata <- na.omit(VA_airdata)

#checking the relation between the total population and pediatric asthma sufferes 

ggplot(data = new_VA_airdata, mapping = aes(x = new_VA_airdata$`Pediatric Asthma sufferers`,
                                            y = new_VA_airdata$`Total Population`)) + 
  geom_point()

#checking which county has highest counts
ggplot(data = new_VA_airdata, mapping = aes(x = new_VA_airdata$County,
                                            y = new_VA_airdata$`Orange Ozone Days`)) + 
  geom_point()

#does highest count county has more asthma patience ? does that mean it gets worse with 
#more pollution.

ggplot(data = new_VA_airdata, mapping = aes(x = new_VA_airdata$County,
                                            y = new_VA_airdata$`Orange Ozone Days`)) + 
  geom_point()

#there is no relation between the points which measures the highest point to asthma patience
ggplot(data = new_VA_airdata, mapping = aes(x = new_VA_airdata$`Maroon  Pollutant Days` ,
                                            y = new_VA_airdata$`Pediatric Asthma sufferers` )) + 
  geom_point()

#there is some relation between them , one is below mean and other is the median(not sure)

boxplot(new_VA_airdata$`Total Population` ~ new_VA_airdata$`Wgt. Avg.  Pollutant Days`)



