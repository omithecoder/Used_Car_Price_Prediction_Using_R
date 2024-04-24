data<- read.csv("Final_TrainingDataSet.csv")
names(data)

library(ggplot2)
library(tidyverse)

# Extract only the first string from the Name column
data$Name <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 2)], collapse=" "))
data$Brand <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 1)], collapse=" "))

values_to_remove <- c("Datsun Redi", "Fiat Siena", "ISUZU D-MAX", "Mercedes-Benz CLS-Class", "Mini Clubman", "Nissan Evalia", "Porsche Boxster", "Tata Venture", "Volkswagen Beetle")

# Convert required columns to factor
# this column have more than 53 levels or categories 
data$Name <- as.numeric(as.factor(data$Name))

data$Location <- as.factor(data$Location)
data$Fuel_Type <- as.factor(data$Fuel_Type)
data$Transmission <- as.factor(data$Transmission)
data$Owner_Type <- as.factor(data$Owner_Type)
data$Brand<- as.factor(data$Brand)

# year vs price
# 
# ggplot(data, aes(x = Year, y = Price)) +
#   geom_point() +
#   labs(title = "Year vs Price", x = "Year", y = "Price")
# 
# ggplot(data, aes(x = Year, y = Price,color = Brand)) +
#   geom_point(size=3,alpha=0.4) +
#   geom_smooth(method = "lm", se = FALSE) +
#   labs(title = "Year vs Price with Trendline", x = "Year", y = "Price")
# 
# 
# # Age vs Price
# ggplot(data, aes(x = Age, y = Price)) +
#   geom_point() +
#   labs(title = "Age vs Price", x = "Age", y = "Price")
# 
# ggplot(data, aes(x = Age, y = Price,color = Brand)) +
#   geom_point(size=3,alpha=0.4) +
#   geom_smooth(method = "lm", se = FALSE) +
#   facet_wrap(~Brand) +
#   labs(title = "Age vs Price with Trendline", x = "Age", y = "Price")
#   
# 
# 
# #Kilometer Driven Vs Price
# ggplot(data, aes(x = Kilometers_Driven, y = Price, color = Brand)) +
#   geom_point(size=3,alpha=0.4) +
#   geom_line(color = "black",size=1) +
#   scale_x_continuous(limits = c(171,500000)) +
#   facet_wrap(~Brand) +
#   labs(title = "Age vs Price with Trendline", x = "Kilometers_Driven", y = "Price")
# 
# #FuelType Vs Price
# ggplot(data, aes(x = Age, y = Price, color = Brand)) +
#   geom_point(size=3,alpha=0.4) +
#   geom_line(color = "black",size=1) + 
#   facet_wrap(~Seats) +
#   labs(title = "Age vs Price with Trendline", x = "Kilometers_Driven", y = "Price")
# 
#  #Mileage Vs Price
# ggplot(data, aes(x = Mileage, y = Price, color = Brand)) +
#   geom_point(size=3,alpha=0.2) +
#   geom_line(color = "black",size=1) +  # Specify color here
#   scale_x_continuous(limits = c(0,40)) +
#   facet_wrap(~Brand) +
#   labs(title = "Age vs Price with Trendline", x = "Kilometers_Driven", y = "Price")
# 
# # Power Vs Price
# ggplot(data, aes(x = Power, y = Price, color = Brand)) +
#   geom_point(size=3,alpha=0.2) +
#   geom_line(color = "black",size=1) +  # Specify color here
#   scale_x_continuous(limits = c(34,600)) +
#   facet_wrap(~Brand) +
#   labs(title = "Age vs Price with Trendline", x = "Kilometers_Driven", y = "Price")
# 
# # Engine Vs Price
# ggplot(data, aes(x = Engine, y = Price, color = Brand)) +
#   geom_point(size=3,alpha=0.2) +
#   geom_line(color = "black",size=1) +  # Specify color here
#   scale_x_continuous(limits = c(600,6000)) +
#   facet_wrap(~Brand) +
#   labs(title = "Age vs Price with Trendline", x = "Kilometers_Driven", y = "Price")
# 
# #


scatter_plots_Brand <- lapply(c("Age", "Year", "Kilometers_Driven", "Power", "Engine", "Mileage"), function(feature) {
  ggplot(data, aes_string(x = feature, y = "Price",color="Brand")) +
    geom_point() +
    geom_line(color = "black",size=1) + 
    facet_wrap(~Brand) +
    labs(title = paste("Price vs", feature), x = feature, y = "Price")
})

scatter_plots_Fuel <- lapply(c("Age", "Year", "Kilometers_Driven", "Power", "Engine", "Mileage"), function(feature) {
  ggplot(data, aes_string(x = feature, y = "Price",color="Brand")) +
    geom_point() +
    geom_line(color = "black",size=1) + 
    facet_wrap(~Fuel_Type) +
    labs(title = paste("Price vs", feature), x = feature, y = "Price")
})

scatter_plots_Owner <- lapply(c("Age", "Year", "Kilometers_Driven", "Power", "Engine", "Mileage"), function(feature) {
  ggplot(data, aes_string(x = feature, y = "Price",color="Brand")) +
    geom_point() +
    geom_line(color = "black",size=1) + 
    facet_wrap(~Owner_Type) +
    labs(title = paste("Price vs", feature), x = feature, y = "Price")
})

scatter_plots_Trans <- lapply(c("Age", "Year", "Kilometers_Driven", "Power", "Engine", "Mileage"), function(feature) {
  ggplot(data, aes_string(x = feature, y = "Price",color="Brand")) +
    geom_point() +
    geom_line(color = "black",size=1) + 
    facet_wrap(~Transmission) +
    labs(title = paste("Price vs", feature), x = feature, y = "Price")
})

kilo_brand <- ggplot(data, aes(x = Kilometers_Driven, y = Price, color = Brand)) +
    geom_point(size=3,alpha=0.4) +
    geom_line(color = "black",size=1) +
    scale_x_continuous(limits = c(171,500000)) +
    facet_wrap(~Brand) +
    labs(title = "Kilometers_Driven vs Price with Trendline", x = "Kilometers_Driven", y = "Price")

kilo_fuel <- ggplot(data, aes(x = Kilometers_Driven, y = Price, color = Brand)) +
  geom_point(size=3,alpha=0.4) +
  geom_line(color = "black",size=1) +
  scale_x_continuous(limits = c(171,500000)) +
  facet_wrap(~Fuel_Type) +
  labs(title = "Kilometers_Driven vs Price with Trendline", x = "Kilometers_Driven", y = "Price")

kilo_owner <- ggplot(data, aes(x = Kilometers_Driven, y = Price, color = Brand)) +
  geom_point(size=3,alpha=0.4) +
  geom_line(color = "black",size=1) +
  scale_x_continuous(limits = c(171,500000)) +
  facet_wrap(~Owner_Type) +
  labs(title = "Kilometers_Driven vs Price with Trendline", x = "Kilometers_Driven", y = "Price")

kilo_trans <- ggplot(data, aes(x = Kilometers_Driven, y = Price, color = Brand)) +
  geom_point(size=3,alpha=0.4) +
  geom_line(color = "black",size=1) +
  scale_x_continuous(limits = c(171,500000)) +
  facet_wrap(~Transmission) +
  labs(title = "Kilometers_Driven vs Price with Trendline", x = "Kilometers_Driven", y = "Price")


scatter_plots_Brand
scatter_plots_Fuel
scatter_plots_Owner
scatter_plots_Trans
kilo_brand
kilo_fuel
kilo_owner
kilo_trans


library(ggcorrplot)
library(metan)
data <- data[, !(names(data) %in% c("X.1","New_Price", "X","Year","Location","Name","Brand","Fuel_Type","Owner_Type","Transmission","Price"))]

All <- corr_coef(data)
plot(All)

cor(data)
cor = round(cor(data),1)

ggcorrplot(cor,lab=TRUE,ggtheme = theme_update(),colors=c("indianred","white","royalblue"))

           