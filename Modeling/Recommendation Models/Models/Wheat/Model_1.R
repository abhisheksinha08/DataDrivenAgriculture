library(dplyr)
library(caret)


data_df <- read.csv("Modeling/Recommendation Models/Final Data/wheat_final.csv", stringsAsFactors = F)

str(data_df)

data_df <- select(data_df, -District.Id, -Block.Id, -Year, -month)

#Splitting into train valid test, 80:10:10
set.seed(12321)
index <- createDataPartition(data_df$Yield, p = 0.8, list = F)
train_df <- data_df[index,]
test_df <- data_df[-index,]

index <- createDataPartition(test_df$Yield, p = 0.5, list = F)
valid_df <- test_df[index,]
test_df <- test_df[-index,]


#RMSE Function
rmse <- function(x, y){
    temp <- (x - y)**2
    temp <- sqrt(mean(temp, na.rm = T))
    return(temp)
}


# Decision Tree
library(rpart)
rp1 <- train(Yield ~ ., data = train_df, method = "rpart")
rmse(predict(rp1), y_train)
rmse(predict(rp1, valid_df), y_valid)

y_train <- train_df$Yield
y_valid <- valid_df$Yield
y_test <- test_df$Yield

train_df <- select(train_df,-Yield)
valid_df <- select(valid_df,-Yield)
test_df <- select(test_df,-Yield)


library(randomForest)

set.seed(12321)
rm1 <- randomForest(y = y_train, x = train_df, ntree = 10)
rmse(predict(rm1), y_train)
rmse(predict(rm1, valid_df), y_valid)
