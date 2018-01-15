library(dplyr)
library(caret)


data_df <- read.csv("Modeling/Recommendation Models/Final Data/wheat_final.csv", stringsAsFactors = F)

str(data_df)

data_df <- select(data_df, -District.Id, -Block.Id, -Year, -month)

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



library(randomForest)
set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
rm2 <- train(y = y_train, x = x_train, method = "rf", trControl = tc)


library(nnet)
set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
nn1 <- train(y = y_train, x = x_train, method = "nnet", trControl = tc, preProcess = c('center', 'scale'), verbose =T)
