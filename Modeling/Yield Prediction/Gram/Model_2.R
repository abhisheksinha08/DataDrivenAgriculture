library(dplyr)
library(caret)
library(ggplot2)
library(randomForest)


soil_df <- read.csv("Modeling/Yield Prediction/Soil_Nutrient_Summarized.csv", stringsAsFactors = F)
weather_df <- read.csv("Modeling/Yield Prediction/SummarizedTidyWeather2012_2017.csv", stringsAsFactors = F)
water_df <- read.csv("Modeling/Yield Prediction/Summarized_Water_Depth.csv", stringsAsFactors = F)

crop_df <- read.csv("Yield/Gram_Raw.csv", stringsAsFactors = F)

str(soil_df)
str(weather_df)
str(water_df)
str(crop_df)

weather_df$year <- weather_df$year + 1
weather_df <- weather_df[weather_df$month==10,]

water_df$Year <- water_df$Year +1
water_df <- water_df[water_df$Month == 'November',]

crop_df$X.1 <- NULL
crop_df$X <- NULL
crop_df$Crop <- NULL

#Data Join
#1. Join Soil and Weather
data_df <- soil_df %>% inner_join(weather_df, by = c("District.Id"="District.Id","Year"="year"))

#2. Join data with water 
data_df <- data_df %>% inner_join(water_df, by = c("District.Id"="District.Id","Year"="Year"))


#3.
data_df <- select(data_df, -month)
weather_df <- read.csv("Modeling/Yield Prediction/SummarizedTidyWeather2012_2017.csv", stringsAsFactors = F)
weather_df$year <- weather_df$year+1
weather_df <- weather_df[weather_df$month==11,]
data_df <- data_df %>% inner_join(weather_df, by = c("District.Id"="District.Id","Year"="year"))

#4. Joi with Crop
data_df <- data_df %>% inner_join(crop_df, by = c("Block.Id"="Block","Year"="Year"))
head(data_df)

data_df$Month <- NULL
data_df$Block <- NULL



data_df <- select(data_df, -District.Id, -Block.Id, -Year, -District)
str(data_df)

#Splitting into train and test, 80:20
set.seed(12321)
index <- createDataPartition(data_df$Yield, p = 0.8, list = F)
train_df <- data_df[index,]
test_df <- data_df[-index,]


y_train <- train_df$Yield
y_test <- test_df$Yield

x_train <- train_df[, 1:(ncol(train_df)-1)]
x_test <- test_df[, 1:(ncol(train_df)-1)]

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



#1. Random Forest
library(randomForest)
set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
rm1 <- randomForest(x_train, y_train, ntree=500, mtry=4)
rmse(y_test, predict(rm1, x_test))
#RMSE: 621


#2. GBM
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), #Num Predictors
                        n.trees = (1:3)*3, 
                        shrinkage = 0.1,
                        n.minobsinnode = 5)

set.seed(825)

gbmFit1<-train(y=y_train, x = x_train, 
               method = "gbm", 
               trControl = tc, 
               verbose = FALSE, 
               ## Now specify the exact models 
               ## to evaluate:
               tuneGrid = gbmGrid)
gbmFit1
summary(gbmFit1)
#RMSE: 459


#Final Model
finalModel <- gbmFit1