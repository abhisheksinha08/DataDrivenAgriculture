library(dplyr)
library(caret)
library(ggplot2)
library(randomForest)


data_df <- read.csv("Modeling/Recommendation Models/Final Data/wheat_final.csv", stringsAsFactors = F)
data_df <- select(data_df, -District.Id, -Block.Id, -Year, -month, -District)

#Splitting into train and test, 80:20
set.seed(12321)
index <- createDataPartition(data_df$Yield, p = 0.8, list = F)
train_df <- data_df[index,]
test_df <- data_df[-index,]


y_train <- train_df$Yield
y_test <- test_df$Yield

x_train <- train_df[, 1:16]
x_test <- test_df[, 1:16]

#RMSE Function
rmse <- function(x, y){
    temp <- (x - y)**2
    temp <- sqrt(mean(temp, na.rm = T))
    return(temp)
}


set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

#Learning Curve
bag_data <- learing_curve_dat(dat = train_df, outcome = "Yield",
                              test_prop = 1/4, 
                              ## `train` arguments
                              method = "rf", 
                              trControl = tc,
                              ## `bagging` arguments
                              nbagg = 100)


ggplot(bag_data, aes(x = Training_Size, y = RMSE, color = Data)) + 
    geom_smooth(method = loess, span = .8) + 
    theme_bw()

#RMSE: 0.38 - BEST SO FAR