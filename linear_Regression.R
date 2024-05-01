
# Load required libraries
# Load required libraries
library(caret)
library(dplyr)
library(ggplot2)

data<-read.csv("Final_TrainingDataSet.csv")
# Remove unnecessary columns
data <- data[, !(names(data) %in% c("New_Price", "X","Year"))]

values_to_remove <- c("Datsun Redi", "Fiat Siena", "ISUZU D-MAX", "Mercedes-Benz CLS-Class", "Mini Clubman", "Nissan Evalia", "Porsche Boxster", "Tata Venture", "Volkswagen Beetle")
data <- data %>%
  filter(!Name %in% values_to_remove)
# Extract only the first string from the Name column
# data$Name <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 2)], collapse=" "))
data$Brand <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 1)], collapse=" "))


data <- data[, !(names(data) %in% c("Name","X.1"))]


# Convert required columns to factor
# this column have more than 53 levels or categories 


data$Location <- as.numeric(as.factor(data$Location))
data$Fuel_Type <- as.numeric( as.factor(data$Fuel_Type))
data$Transmission <- as.numeric(as.factor(data$Transmission))
data$Owner_Type <- as.numeric(as.factor(data$Owner_Type))
# data$Brand<- as.numeric(as.factor(data$Brand))
data$Brand<- as.factor(data$Brand)

# Divide dataset into training and testing (75% train, 25% test)
set.seed(123)  # for reproducibility
pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.75,0.25))
train_data <- data[pd == 1, ]
test_data <- data[pd == 2, ]

test_data <- test_data %>%
  filter(!Brand %in% "ISUZU")

write.csv(train_data, file = "train_data.csv")

Price = test_data$Price
test_data <- test_data[, !(names(data) %in% c("Price"))]

# Convert categorical variables to factors with levels from training data
# test_data$Location <- factor(test_data$Location, levels = levels(train_data$Location))
# test_data$Fuel_Type <- factor(test_data$Fuel_Type, levels = levels(train_data$Fuel_Type))
# test_data$Transmission <- factor(test_data$Transmission, levels = levels(train_data$Transmission))
# test_data$Owner_Type <- factor(test_data$Owner_Type, levels = levels(train_data$Owner_Type))
# test_data$Brand <- factor(test_data$Brand, levels = levels(train_data$Brand))



# Train a linear model for price prediction
lm_model <- lm(Price ~ Age+Kilometers_Driven+Fuel_Type+Transmission+Owner_Type+Mileage+Engine+Power+Seats+Brand, data = train_data)

# Predict price using filtered test data
predicted_price <- predict(lm_model, newdata = test_data)

# Evaluate the model
# You can add evaluation metrics here based on your preference
MAE <- mean(abs(predicted_price - Price))
RMSE <- sqrt(mean((predicted_price - Price)^2))
R_squared <- summary(lm_model)$r.squared

print(paste("Mean Absolute Error (MAE):", MAE))
print(paste("Root Mean Squared Error (RMSE):", RMSE))
print(paste("R-squared-byModel:", R_squared))
mean_actual_price <- mean(Price)

# Calculate total sum of squares (TSS)
TSS <- sum((Price - mean_actual_price)^2)

# Calculate residual sum of squares (RSS)
RSS <- sum((predicted_price - Price)^2)

# Calculate R-squared value
R_squared <- 1 - (RSS / TSS)

print(paste("R-squared-byCalculation:", R_squared))

summary(lm_model)
sum<- data.frame(sum)
print(sum)

write.table(sum, file, append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
save(sum,file="significance.txt")

p_values <- summary(lm_model)$coefficients[, 4]
