library(dplyr)
library(caret)



data_df <- read.csv("Modeling/Recommendation Models/Final Data/wheat_final.csv", stringsAsFactors = F)

str(data_df)

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

#~+SoilPh+ElectricalConductivity+OrganicCarbon+Phosphorous+Potassium+Sulphur+Zinc+Iron+Magnesium+meanTemp+meanDewPoint+meanStationPressure+meanVisibility+meanWindSpeed+meanPrecipitation+meanDepth+Yield

lm1 <- glm(Yield ~ Sulphur+meanTemp+meanDewPoint+meanVisibility+meanPrecipitation+meanDepth, data = train_df)
summary(lm1)

pca <- prcomp(x = x_train, scale. = T)
expl.var <- round(pca$sdev^2/sum(pca$sdev^2)*100) # percent explained variance

train_pca <- predict(pca, x_train )
test_pca <- predict(pca, x_test)


nsel = 5

x_train <- train_pca[,1:nsel]
x_test <- test_pca[,1:nsel]

library(randomForest)
set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
rm2 <- train(y = y_train, x = x_train[,c(6,10,11,13,15,16)], method = "rf", trControl = tc)
summary(rm2)
rm2


library(xgboost)
set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
xg1 <- train(y = y_train, x = x_train[,c(6,10,11,13,15,16)], method = "xgbTree", trControl = tc, verbose=T)
summary(xg1)
xg1


library(nnet)
set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
nn1 <- train(y = y_train, x = x_train[,c(6,10,11,13,15,16)], method = "nnet", trControl = tc, preProcess = c('center', 'scale'), verbose =T)
summary(nn1)
nn1$results
