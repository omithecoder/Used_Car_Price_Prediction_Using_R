library(tidyverse)
library(randomForest)
data <- read.csv("train-data.csv")

NumOfNa <- sum(is.na(data))

l<- names(data)


# B dataset is without Na values
b <-data %>% 
  drop_na(everything()) %>% 
  select(everything()) 

d <-data %>% 
  drop_na(everything()) %>% 
  select(everything()) 
  
DropNaRows<- nrow(data)-nrow(b)  
cat("There are ",DropNaRows,"Rows with Null Values")

library(dplyr)
?dplyr

b<- b %>%
  filter(Mileage != "")



# But still "New Price" column of some cars are empty lets Impute it
# We know the New Car price is depends :
# 1) How Old is Model (Age)
# 2) Power
# 3) Engine
# 4) Seats
# 5) Transmission
# 6) Fuel Type
# 7) Mileage

# The problem with this is that the some of above parameters are stored in character datatype but required datatype is numeric
# So lets convert them into numeric


# First we convert Mileage into Double
mileage_data <- b$Mileage

# Remove units and convert to integer
mileage_values <- as.double(sub(" km/kg| kmpl", "", mileage_data))

# Resulting integer values
mileage_values

b$Mileage<-mileage_values
b$Mileage<-as.double(b$Mileage)


# Now we convert Power
power_data<- b$Power

power_values  <- as.double(sub("bhp","",power_data))

power_values

b$Power<-power_values
b$Power<-as.double(b$Power)


# Now convert Engine
Engine_data<-b$Engine

Engine_values<-as.double(sub("CC","",Engine_data))  

Engine_values

b$Engine<-Engine_values
b$Engine<-as.double(b$Engine)



# Now convert New_Price
New_price_data <- b$New_Price

New_price_value<- as.double(sub("Lakh","",New_price_data))

b$New_Price<-New_price_value
b$New_Price<-as.double(b$New_Price)

# Now we create one new feature which determine the Age of model or How much old is the given model is 
b<-b %>% 
  mutate(Age = 2024-Year) %>% 
  select(X,Year,Age,everything())


# Now lets Impute New_price values using rf_Impute
# Load necessary libraries

# Assuming 'df' is your dataset with relevant columns including 'New_Price'
# Replace missing values in 'New_Price' using rfImpute

sum(is.na(b$New_Price))


?rfImpute
# Convert necessary variables to factors
b$Transmission <- as.factor(b$Transmission)
b$Fuel_Type <- as.factor(b$Fuel_Type)

# Convert all other variables to numeric
b$Mileage <- as.numeric(as.character(b$Mileage))
b$Power <- as.numeric(as.character(b$Power))
b$Engine <- as.numeric(as.character(b$Engine))
b$Seats <- as.numeric(as.character(b$Seats))

# Impute missing values using rfImpute
b.imputed <- rfImpute(Engine~ Mileage + Power + Seats + Transmission + Fuel_Type+New_Price, data = b)
b$New_Price<-b.imputed$New_Price
b$Power<-b.imputed$Power

str(b)
sum(is.na(b))


write.csv(b, file = "Final_TestDataSet.csv")


