library(dplyr)
library(caret)
library(ggplot2)

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


#1. Random Forest
library(randomForest)
set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
rm2 <- train(y = y_train, x = x_train, method = "rf", trControl = tc, ntree=1500)

plot(rm2)
#RMSE: 0.5815


#2. Neural Net
library(nnet)
set.seed(123)
tc <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
nn1 <- train(y = y_train, x = x_train, method = "nnet", trControl = tc, preProcess = c('center', 'scale'), verbose =T)
#RMSE: 0.76



# Add Grid Search
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9, 13, 17), #Num Predictors
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

#3. GBM
set.seed(825)
gbmFit2 <- train(y=y_train, x = x_train, 
                 method = "gbm", 
                 trControl = tc, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2

# Plot RMSE
trellis.par.set(caretTheme())
plot(gbmFit2)
#RMSE: 0.62

#Variable IMportance
varImp(gbmFit2, scale = T)
filterVarImp(x = x_train[, -ncol(x_train)], y = y_train)



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