data<-read.csv("D://Song_Popularity_Prediction/song_data.csv") 
summary(data)
str(data)
head(data)

data<-data[,-1]

#Removing Duplicate Values
library(dplyr)
data<-data %>% distinct()


#checking if there's Null Values present
colSums(is.na(data))


#Standardize Data

data$song_duration_ms<-scale(data$song_duration_ms)
data$loudness<-scale(data$loudness)
data$tempo<-scale(data$tempo)
data$time_signature<-scale(data$time_signature)
data$key<-scale(data$key)

summary(data)

#Checking Correlation
library(corrplot)
corrplot(cor(data),
         method = "square",
)



#Splitting the Dataset
set.seed(1234) 

sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, 
                     prob=c(0.8,0.2))
train  <- data[sample, ] 
test   <- data[!sample, ] 

X_train <- train[, -1]
X_test <- test[, -1]
y_train <- train[,1]
y_test <- test[,1]


#Applying Random Forest Algorithm


install.packages("randomForest")
library(randomForest)
rf_model <- randomForest(song_popularity ~ ., data = data)

predictions <- predict(rf_model, newdata = X_test)



#calculating Evaluation Measures
rmse <- sqrt(mean((predictions - y_test)^2))
mae <- mean(abs(predictions - y_test))
rsquared <- 1 - sum((y_test - predictions)^2) / sum((y_test - mean(y_test))^2)

print(paste("Root Mean Squared Error (RMSE):", round(rmse, 2)))
print(paste("Mean Absolute Error (MAE):", round(mae, 2)))
print(paste("R-squared (R2):", round(rsquared, 2)))


#Plotting the results
install.packages("ggplot2")
library(ggplot2)


comparison_data <- data.frame(Actual = y_test, Predicted = predictions)

ggplot(comparison_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "green", linetype = "dotted") +
  labs(x = "Actual Values", y = "Predicted Values",
       title = "Actual vs. Predicted Values",
       subtitle = "Comparison of model predictions to actual values",
       caption = "Dashed red line: Regression line | Dotted green line: 1:1 line") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.position = "none")








