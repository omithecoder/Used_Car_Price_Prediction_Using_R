library(caret)
library(rpart)
library(dplyr)

data<-read.csv("Final_TrainingDataSet.csv")

str(data)
# Load required libraries
# Load required libraries
library(caret)


data <- data[, !(names(data) %in% c("New_Price", "X"))]
# Extract only the first string from the Name column
data$Name <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 2)], collapse=" "))

values_to_remove <- c("Datsun Redi", "Fiat Siena", "ISUZU D-MAX", "Mercedes-Benz CLS-Class", "Mini Clubman", "Nissan Evalia", "Porsche Boxster", "Tata Venture", "Volkswagen Beetle")

# Convert required columns to factor
data$Name <- as.factor(data$Name)
data$Location <- as.factor(data$Location)
data$Fuel_Type <- as.factor(data$Fuel_Type)
data$Transmission <- as.factor(data$Transmission)
data$Owner_Type <- as.factor(data$Owner_Type)


# Divide dataset into training and testing (75% train, 25% test)
set.seed(123)  # for reproducibility
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.75,0.25))
train_data <- data[pd==1,]
test_data <- data[pd==2,]

test_data <- test_data %>%
  filter(!Name %in% values_to_remove)


# Remove rows from test data where Name column contains "Bentley"
test_data_filtered <- test_data[test_data$Name != "Bentley", ]
test_data <- test_data[test_data$Name != "Bentley", ]
test_data_filtered <- test_data_filtered[, !(names(test_data_filtered) %in% c("Price"))]


dt_model <- rpart(Price ~ ., data = train_data, method = "anova")

# Predict price using test data
predicted1_price <- predict(dt_model, newdata = test_data_filtered)

library(Metrics)
# Evaluate the model
# You can add evaluation metrics here based on your preference
MAE <- mean(abs(predicted1_price - test_data$Price))
RMSE<- rmse(test_data$Price,predicted1_price)


print(paste("Mean Absolute Error (MAE):", MAE))
print(paste("Root Mean Squared Error (RMSE):", RMSE))
# Calculate mean of actual prices
mean_actual_price <- mean(test_data$Price)

# Calculate total sum of squares (TSS)
TSS <- sum((test_data$Price - mean_actual_price)^2)

# Calculate residual sum of squares (RSS)
RSS <- sum((predicted1_price - test_data$Price)^2)

# Calculate R-squared value
R_squared1 <- 1 - (RSS / TSS)

print(paste("R-squared:", R_squared1))

