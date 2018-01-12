library(dplyr)

distric_master <- xlsx::read.xlsx("Dsitrict_Block_Masters.xlsx", sheetIndex = 1)
str(distric_master)
weather_df <- read.csv("Weather/tidy_dist_weather.csv", stringsAsFactors = T)


#Join with District Master 
weather_df <-  weather_df %>% left_join(distric_master, by = c("District" = "District.Name")) %>% select(District.Id, everything()) %>% select (-District, -Station)

weather_df$District.Id <- as.character(weather_df$District.Id)
head(weather_df)


library(lubridate)

weather_df$year <- substr(weather_df$YearMonDay,1,4)
weather_df$month <- substr(weather_df$YearMonDay,5,6)
weather_df$day <- substr(weather_df$YearMonDay,7,8)

weather_df$YearMonDay <- NULL

str(weather_df)

#Summarize Data by MOnth, Year and District.Id
summarized_weather <- weather_df %>% group_by(District.Id, year, month) %>% summarize(meanTemp = mean(AvgTemp, na.rm = T), meanDewPoint = mean(AvgDewPoint, na.rm = T),
                                                                                      meanStationPressure = mean(StationPressure, na.rm=T), meanVisibility = mean(Visibility, na.rm=T),
                                                                                      meanWindSpeed = mean(WindSpeed, na.rm = T), 
                                                                                      meanPrecipitation = mean(PrecipitationAmount, na.rm=T)) %>% as.data.frame()

head(summarized_weather)

summarized_weather$year <- as.integer(summarized_weather$year)

summarized_weather <- summarized_weather[summarized_weather$year>=2012,]

write.csv(summarized_weather, file = "Weather/SummarizedTidyWeather2012_2017.csv", row.names = F)
