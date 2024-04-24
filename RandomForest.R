  library(caret)
  library(randomForest)
  library(dplyr)
  
  data<-read.csv("Final_TrainingDataSet.csv")
  # Remove unnecessary columns
  data <- data[, !(names(data) %in% c("New_Price", "X","Year"))]
  
  # Extract only the first string from the Name column
  # data$Name <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 2)], collapse=" "))
  data$Brand <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 1)], collapse=" "))
  
  data <- data[, !(names(data) %in% c("Name","X.1"))]
  
  
  # Convert required columns to factor
  # this column have more than 53 levels or categories 
  
  
  data$Location <- as.factor(data$Location)
  data$Fuel_Type <- as.factor(data$Fuel_Type)
  data$Transmission <- as.factor(data$Transmission)
  data$Owner_Type <- as.factor(data$Owner_Type)
  data$Brand<- as.factor(data$Brand)
  
  # Divide dataset into training and testing (75% train, 25% test)
  set.seed(123)  # for reproducibility
  pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.75,0.25))
  train_data <- data[pd == 1, ]
  test_data <- data[pd == 2, ]
  
  write.csv(train_data, file = "train_data.csv")
  
  Price = test_data$Price
  test_data <- test_data[, !(names(data) %in% c("Price"))]
  
  # Convert categorical variables to factors with levels from training data
  test_data$Location <- factor(test_data$Location, levels = levels(train_data$Location))
  test_data$Fuel_Type <- factor(test_data$Fuel_Type, levels = levels(train_data$Fuel_Type))
  test_data$Transmission <- factor(test_data$Transmission, levels = levels(train_data$Transmission))
  test_data$Owner_Type <- factor(test_data$Owner_Type, levels = levels(train_data$Owner_Type))
  test_data$Brand <- factor(test_data$Brand, levels = levels(train_data$Brand))
  
  
  # names(test_data)
  # is.numeric(test_data$Year)
  # is.numeric(test_data$Age)
  # is.numeric(test_data$Kilometers_Driven)
  # is.numeric(test_data$Mileage)
  # is.numeric(test_data$Engine)
  # is.numeric(test_data$Power)
  # is.numeric(test_data$Seats)
  # is.factor(test_data$Location)
  # is.factor(test_data$Fuel_Type)
  # is.factor(test_data$Transmission)
  # is.factor(test_data$Owner_Type)
  # is.factor(test_data$Brand)
  
  
  
  
  # Train a Random Forest model for price prediction
  rf_model <- randomForest(Price ~ ., data = train_data,iter=300)
  
  # Predict price using test data 
  predicted_price_rf <- predict(rf_model, newdata = test_data)
  
  # Evaluate the model
  # Calculate Mean Absolute Error (MAE)
  MAE_rf <- mean(abs(predicted_price_rf - Price))
  
  # Calculate Root Mean Squared Error (RMSE)
  RMSE_rf <- sqrt(mean((predicted_price_rf - Price)^2))
  
  # Calculate R-squared value
  R_squared_rf <- cor(predicted_price_rf, Price)^2
  
  print(paste("Mean Absolute Error (MAE) with Random Forest:", MAE_rf))
  print(paste("Root Mean Squared Error (RMSE) with Random Forest:", RMSE_rf))
  
  print(paste("R-squared with Random Forest:", R_squared_rf))
  
  saveRDS(rf_model, file = "random_forest_model.rds")
    