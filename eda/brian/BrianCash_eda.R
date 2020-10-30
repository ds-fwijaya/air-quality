library(ggplot2)

manufact_data <- table_export

ggplot(data = manufact_data) + 
  geom_point(mapping = aes(x = `Real-World CO2_City (g/mi)`,y = `Real-World CO2_Hwy (g/mi)`)) + 
  facet_wrap(~ Manufacturer, nrow = 2)
# Positive correlation hints that the two are probably very relatable.

ggplot(data = manufact_data) + 
  geom_point(mapping = aes(x = `Model Year`,y = `Real-World CO2 (g/mi)`)) + 
  facet_wrap(~ Manufacturer, nrow = 2)
# We notice that there are distributions that do not show a positive or negative trend.

ggplot(data = manufact_data) + 
  geom_point(mapping = aes(x = `Model Year`,y = `Real-World CO2_Hwy (g/mi)`)) + 
  facet_wrap(~ Manufacturer, nrow = 2)
# Much closer to a negative correlation.

ggplot(data = manufact_data) + 
  geom_point(mapping = aes(x = `Model Year`,y = `Real-World CO2_City (g/mi)`)) + 
  facet_wrap(~ Manufacturer, nrow = 2)
# No conclusivity on the trend with city CO2, maybe there is a leed here?

ggplot(data = manufact_data) + 
  geom_point(mapping = aes(x = `Weight (lbs)`,y = `Real-World CO2 (g/mi)`)) + 
  facet_wrap(~ Manufacturer, nrow = 2)
# Heavier vehichles tend to produce more CO2

ggplot(data = manufact_data) + 
  geom_point(mapping = aes(x = `Weight (lbs)`,y = `Real-World CO2 (g/mi)`)) + 
  facet_wrap(~ `Regulatory Class`, nrow = 2)
# This graph clearly shows the distribution of high emitters vs low emitters via class type.






manufact_data$`Horsepower (HP)`
manufact_data$`Regulatory Class`