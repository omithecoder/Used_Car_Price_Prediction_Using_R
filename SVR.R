data<-read.csv("Final_TrainingDataSet.csv")

str(data)
# Load required libraries
# Load required libraries
library(caret)
library(dplyr)
library(ggplot2)
library(e1071)

data <- data[, !(names(data) %in% c("New_Price", "X"))]
# Extract only the first string from the Name column
data$Name <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 2)], collapse=" "))
data$Brand <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 1)], collapse=" "))





values_to_remove <- c("Datsun Redi", "Fiat Siena", "ISUZU D-MAX", "Mercedes-Benz CLS-Class", "Mini Clubman", "Nissan Evalia", "Porsche Boxster", "Tata Venture", "Volkswagen Beetle")

ggplot(data, aes(x=Brand, y=Price)) +
  geom_point() + # This adds the points to the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotate x labels for better readability
  labs(x='Brand of Car', y='Price of Car', title='Car Brand vs. Price')



# Convert required columns to factor
data$Name <- as.factor(data$Name)
data$Location <- as.factor(data$Location)
data$Fuel_Type <- as.factor(data$Fuel_Type)
data$Transmission <- as.factor(data$Transmission)
data$Owner_Type <- as.factor(data$Owner_Type)
data$Brand<- as.factor(data$Brand)

# Divide dataset into training and testing (75% train, 25% test)
# Divide dataset into training and testing (75% train, 25% test)
set.seed(123)  # for reproducibility
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.75,0.25))
train_data <- data[pd==1,]
test_data <- data[pd==2,]

test_data <- test_data %>%
  filter(!Name %in% values_to_remove)

# Remove rows from test data where Name column contains "Bentley"
test_data_filtered <- test_data[test_data$Name != "Bentley", ]
test_data_filtered <- test_data[test_data$Name != "ISUZU", ]
test_data <- test_data[test_data$Name != "Bentley", ]
test_data <- test_data[test_data$Name != "ISUZU", ]
test_data_filtered <- test_data_filtered[, !(names(test_data_filtered) %in% c("Price"))]


?svm()

library(e1071)
svr_model <- svm(Price ~ ., data = train_data, na.action = na.omit, scale = TRUE,kernel = 'radial')
  

# Predict price using filtered test data
predicted_price_svr <- predict(svr_model, newdata = test_data_filtered)

# Evaluate the model
# You can add evaluation metrics here based on your preference
MAE <- mean(abs(predicted_price_svr - test_data$Price))
RMSE <- sqrt(mean((predicted_price_svr - test_data$Price)^2))


print(paste("Mean Absolute Error (MAE):", MAE))
print(paste("Root Mean Squared Error (RMSE):", RMSE))

mean_actual_price <- mean(test_data$Price)

# Calculate total sum of squares (TSS)
TSS <- sum((test_data$Price - mean_actual_price)^2)

# Calculate residual sum of squares (RSS)
RSS <- sum((predicted_price_svr - test_data$Price)^2)

# Calculate R-squared value
R_squared <- 1 - (RSS / TSS)

print(paste("R-squared-byCalculation:", R_squared))

