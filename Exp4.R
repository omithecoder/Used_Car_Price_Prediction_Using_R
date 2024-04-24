library(ggplot2)

data <- read.csv("Toy_sales_csv.csv")
cat("********* Apply simple linear model considering response as Unit sales and explanatory variable as Price. *******")
slm_model <- lm(Unitsales~Price,data)
s1<-summary(slm_model)
cat("\n\nSummary of Linear Model")
print(s1)
predicted_unitsales <- predict(slm_model,data)
cat("\n\nPredicted Values => ",predicted_unitsales)
cat("\n\nActual Values => ", data$Unitsales)
error <- data$Unitsales-predicted_unitsales
cat("\n\nError Values => ",error)
cat("\n\nScatter plot for regression")
slm_plot<- ggplot(data,aes(Price,Unitsales))+geom_point()+geom_smooth(method="lm",formula=y~x,col="red",se=F)
print(slm_plot)




cat("\n\n\n\n****** Consider all variables to fit the regression model. ******")
cat("\n Multiple Linear Regression")
mlr_model <- lm(Unitsales~Price+Adexp+Promexp,data)
s2<-summary(mlr_model)
cat("\n\nSummary of Linear Model")
print(s2)
predicted_unitsales1 <- predict(mlr_model,data)
cat("\n\nPredicted Values => ",predicted_unitsales1)
cat("\n\nActual Values => ", data$Unitsales)
error <- data$Unitsales-predicted_unitsales1
cat("\n\nError Values => ",error)


# a. Price=9.1$, Adexp=52,000$, Promexp=61,000$
# b. Price=8.1$, Adexp=50,000$,Promexp=60,000$

testdata <- data.frame(Price=c(9.1,8.1),Adexp=c(52,50),Promexp=c(61,60))

cat("\nTesting dataset")
print(testdata)
prediction <- predict(mlr_model,testdata)
cat("\n Prediction for above data frame : ",prediction)

