data <-read.csv("Final_TrainingDataSet.csv")
unique(data$Price)
min(data$Price)
max(data$Price)

data <- data[, !(names(data) %in% c("New_Price", "X"))]

data$Brand <- sapply(strsplit(data$Name, " "), function(x) paste(x[1:min(length(x), 1)], collapse=" "))

pd <- sample(2, nrow(data), replace = TRUE, prob = c(0.25,0.75))
data <- data[pd==1,]


lower_bound_mid_range = 10
lower_bound_luxury = 25

data <- data %>%
  mutate(Category = case_when(
    Price <= lower_bound_mid_range ~ "Economy",
    Price > lower_bound_mid_range & Price <= lower_bound_luxury ~ "Mid-Range",
    Price > lower_bound_luxury ~ "Luxury"
  ))


Cars <- data.frame(data$Brand,data$Category)
Cars <- table(Cars)

Economy <- sum(Cars$data.Category=="Economy")
Mid_Range <- sum(Cars$data.Category=="Mid-Range")
Luxury <- sum(Cars$data.Category=="Luxury")


library(ggplot2)

category_counts <- data %>%
  group_by(Category) %>%
  summarise(Count = n())

ggplot(category_counts, aes(x=Category, y=Count)) +
  geom_bar(stat='identity', fill='steelblue') +
  theme_minimal() +
  labs(x='Category', y='Count', title='Bar Graph of Car Categories')


# Applying Cia-Square goodness of fit test

# Assuming Null Hypothesis 
# Ho -> The dataset consist all categories of cars equally or uniformly
# Ha -> The dataset consist unequal categories of cars

observed_counts <- table(data$Category)

# Now, perform the Chi-squared test
# If you expect equal frequencies, the expected counts would be the total count divided by the number of categories
expected_counts <- rep(sum(observed_counts) / length(observed_counts), length(observed_counts))

# Perform the Chi-squared test
chi_squared_test <- chisq.test(x = observed_counts, p = expected_counts, rescale.p = TRUE)

# Output the result of the Chi-squared test
print(chi_squared_test)


# here p value is very less which is less than 2.2e-16
# so we reject the 

  
