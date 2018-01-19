library(dplyr)


soil_df <- read.csv("Modeling/Recommendation Models/Soil_Nutrient_Summarized.csv")
weather_df <- read.csv("Modeling/Recommendation Models/SummarizedTidyWeather2012_2017.csv")
water_df <- read.csv("Modeling/Recommendation Models/Summarized_Water_Depth.csv")

crop_df <- read.csv("Modeling/Recommendation Models/Yield/Wheat_Normalized.csv")

str(soil_df)
str(weather_df)
str(water_df)
str(crop_df)

#Wheat Sowing Time is November

# Taking only November Month's weather in consideration
weather_df <- weather_df[weather_df$month==11,]

#Taking Water Depth of November Quarter
water_df <- water_df[water_df$Month == 'November',]

crop_df$X.1 <- NULL
crop_df$X <- NULL
crop_df$Crop <- NULL

#Data Join
#1. Join Soil and Weather
data_df <- soil_df %>% inner_join(weather_df, by = c("District.Id"="District.Id","Year"="year"))

#2. Join data with water 
data_df <- data_df %>% inner_join(water_df, by = c("District.Id"="District.Id","Year"="Year"))

#3. Joi with Crop
data_df <- data_df %>% inner_join(crop_df, by = c("Block.Id"="Block","Year"="Year"))
head(data_df)

data_df$Month <- NULL
data_df$Block <- NULL

str(data_df)

write.csv(data_df, "Modeling/Recommendation Models/Final Data/wheat_final.csv", row.names = F)

rm(list = ls())
