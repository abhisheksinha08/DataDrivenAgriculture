##Tidy Weather Data for Haryana
##Author: Abhishek Sinha
##Creation Date: 23/11/2017
###############################################

library(dplyr)

#Read Data File - Taken from the [SOURCE] TODO: Ask Pronod Sir
df <- data.table::fread("CDO7374177468829.txt")


#Mean temperature (.1 Fahrenheit),Mean dew point (.1 Fahrenheit),Mean sea level pressure (.1 mb),Mean station pressure (.1 mb)
#Mean visibility (.1 miles),Mean wind speed (.1 knots),Maximum sustained wind speed (.1 knots),Maximum wind gust (.1 knots)
#Maximum temperature (.1 Fahrenheit),Minimum temperature (.1 Fahrenheit),Precipitation amount (.01 inches),Snow depth (.1 inches)
#Indicator for occurrence of:  Fog,Rain or Drizzle,Snow or Ice Pellets,Hail,Thunder,Tornado/Funnel Cloud

column_names <-
    c(
        "Station",
        "WBAN",
        "YearMonDay",
        "AvgTemp",
        "Temp1",
        "AvgDewPoint",
        "DEWP1",
        "SeaLevelPress",
        "SLP1",
        "StationPressure",
        "STP1",
        "Visibility",
        "VISIB1",
        "WindSpeed",
        "WDSP1",
        "MaxSpeed",
        "WindGust",
        "MaxTemp",
        "MinTemp",
        "PrecipitationAmount",
        "SnowDepth",
        "FRSHTT"
    )

colnames(df) <- column_names


#Remove unnecessary columns
columns_remove <-
    c(
        "WBAN",
        "Temp1",
        "DEWP1",
        "SeaLevelPress",
        "SLP1",
        "STP1",
        "VISIB1",
        "WDSP1",
        "MaxSpeed",
        "WindGust",
        "SnowDepth",
        "FRSHTT"
    )


for (col_rem in columns_remove)
{
    df[, col_rem] <- NULL
}


#Get Indicator from Precipitation. Last Character only.
df$Indicator <-
    substr(
        df$PrecipitationAmount,
        nchar(df$PrecipitationAmount),
        nchar(df$PrecipitationAmount) ##Why have i used nchar, it should be 1.
    )

df$Indicator[df$Indicator == 9] <- ''

df$PrecipitationAmount <-
    as.numeric(substr(
        df$PrecipitationAmount,
        0,
        nchar(df$PrecipitationAmount) - nchar(df$Indicator)
    ))




df$Indicator <- NULL


#Removing * from Temperature
df$MinTemp <- as.numeric(gsub('\\*', "", df$MinTemp))
df$MaxTemp <- as.numeric(gsub('\\*', "", df$MaxTemp))


#Converting to Celcius
fahreinheitToCel <- function(x)
{
    return((x - 32) * 5 / 9)
}

df$AvgTemp <- fahreinheitToCel(df$AvgTemp)
df$MaxTemp <- fahreinheitToCel(df$MaxTemp)
df$MinTemp <- fahreinheitToCel(df$MinTemp)





#Read District Wise Weather Station Data  - Compiled by Pronod B.
dist_wise_weather_station <- as.data.frame(readxl::read_xlsx("Haryana - District Wise WeatherStation Mapping.xlsx", col_names = T))


#Read Selected Weather List - Compiled by Pronod B.
station_list<-as.data.frame(readxl::read_xlsx("Haryana - Selected Weather Station list.xlsx", col_names = T))


#Tidy Up Station Id
getStationId <- function(stationName)
{
    if(is.na(stationName)==T)
    {
        return('')
    }
    return(substring(station_list$STATION_ID[station_list$STATION==stationName],1,6))
}

dist_wise_weather_station$LeftWeatherstationId <- unlist(lapply(dist_wise_weather_station$`Left Weatherstation`, getStationId))
dist_wise_weather_station$RightWeatherstationId <- unlist(lapply(dist_wise_weather_station$`Right Weather Station`, getStationId))
dist_wise_weather_station$NearestWeatherstationId <- unlist(lapply(dist_wise_weather_station$`Nearest Weather Station`, getStationId))
dist_wise_weather_station$`Left Weatherstation` <- NULL
dist_wise_weather_station$`Right Weather Station` <- NULL
dist_wise_weather_station$`Nearest Weather Station` <- NULL


#Get Weather Stations left of District
leftStations <- dist_wise_weather_station$LeftWeatherstationId[dist_wise_weather_station$LeftWeatherstationId!='']
#Get Weather Stations right of District
rightStations <- dist_wise_weather_station$RightWeatherstationId[dist_wise_weather_station$LeftWeatherstationId!='']





## Join Weather Station Data based on Average from two stations
#st1Id - Station 1 ID (E.g. - 421030)
#st2Id - Station 2 ID (E.g. - 421030)
joinWeatherStationData <- function(st1Id, st2Id)
{
    station_name <- paste(st1Id, st2Id, sep = "_")
    st1<-df[df$Station==st1Id,]
    st2<-df[df$Station==st2Id,]
    
    #join stations data
    st1 <- st1 %>% full_join(st2, by = "YearMonDay")
    
    #Get Columns Names for averaging
    col_x <- colnames(st1[,3:10])
    col_y <- colnames(st1[,12:19])
    col_xy <- gsub(".x", "", col_x)
    
    
    i<-1
    for (colx in col_x) {
        #If null, copy from the other station. This will help with averaging
        st1[is.na(st1[col_x[i]])==T, col_x[i]] <- st1[is.na(st1[col_x[i]])==T, col_y[i]]
        st1[is.na(st1[col_y[i]])==T, col_y[i]] <- st1[is.na(st1[col_y[i]])==T, col_x[i]]
        
        st1[,col_xy[i]] <- (st1[,col_x[i]] +st1[,col_y[i]])/2
        
        #Remove Columns
        st1[,col_x[i]]<-NULL
        st1[,col_y[i]]<-NULL
        i<-i+1
    }
    
    #Remove extra columns
    st1[,c(1,3)]<-NULL
    st1$Station <- station_name
    
    #Reorder columns
    st1 <- st1[,c(10,1:9)]
    st2<-NULL
    return(st1)
}

# For all left and right stations generate data
j<-1
st3 <- data.frame()
for (leftStationId in leftStations) {
    if(nrow(st3)==0)
    {
        st3 <- joinWeatherStationData(leftStations[j], rightStations[j])
    }
    else
    {
        stationName <-paste(leftStations[j], rightStations[j], sep = "_")
        if(nrow(st3[st3[,1]==stationName,])==0)
        {
          st3 <- rbind(st3, joinWeatherStationData(leftStations[j], rightStations[j]))
        }
    }
    j<-j+1
}

#Copy Column Name from df
colnames(st3) <- colnames(df)
# Combine st3 with 421030, 421810, 421310
for(n in names(table(dist_wise_weather_station$NearestWeatherstationId)))
{
    if(n !='')
    {
        st3 <- rbind(st3, df[df$Station==n,])
    }
}

df<- NULL

dist_wise_weather_station$NearestWeatherstationId[dist_wise_weather_station$NearestWeatherstationId==''] <- paste(dist_wise_weather_station$LeftWeatherstationId[dist_wise_weather_station$NearestWeatherstationId==''], dist_wise_weather_station$RightWeatherstationId[dist_wise_weather_station$NearestWeatherstationId==''], sep = "_")

dist_wise_weather_station$LeftWeatherstationId<-NULL
dist_wise_weather_station$RightWeatherstationId<-NULL
dist_wise_weather_station$S.No <-NULL

colnames(dist_wise_weather_station) <- c("District","Station")

#Finally Join the weather data with District Tabler
dist_weather <- dist_wise_weather_station %>% inner_join(st3, by = "Station")

#Write to CSV
write.csv(dist_weather, file = "tidy_dist_weather.csv", row.names = F)

#Remove all objects from Enviroment
rm(list=ls())
