library(readxl)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(writexl)

U_S_Chronic_Disease_Indicators_CDI_ <- read_excel("C:/Users/salim/OneDrive/Desktop/CDS-490/U.S._Chronic_Disease_Indicators__CDI_s.xls")
View(U_S_Chronic_Disease_Indicators_CDI_s)


data1 <- U_S_Chronic_Disease_Indicators_CDI_

data_view1 <- U_S_Chronic_Disease_Indicators_CDI_ %>%
  select(Topic) %>%
  unique()

data1_as <- data1 %>%
  filter(Topic == 'Asthma')

ggplot(data=data1_as, aes(x=LocationAbbr,fill=LocationDesc))+
  geom_bar()
  
