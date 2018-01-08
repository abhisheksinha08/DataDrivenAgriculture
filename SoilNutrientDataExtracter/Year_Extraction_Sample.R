library(data.table)
library(dplyr)

soil_data <- as.data.frame(fread(file = "haryana_soil_nutrient.csv", header = T, stringsAsFactors = F))

library(stringr)

get_year <- function(x){
    return(strsplit(x,'/')[[1]][2])
}

y<- as.data.frame(lapply(soil_data$SampleNo, get_year))
y<-as.data.frame(t(y))
soil_data$Year_range <- y[,1]
head(soil_data)


soil_data$Year<- substr(soil_data$Year_range,1,4)
soil_data$Year_range <- NULL

head(soil_data)

write.csv(soil_data, "Soil_Nutrient_Year.csv")