library(data.table)


soil_df <- fread("Soil_Nutrient_Year.csv", header = T, stringsAsFactors = F, showProgress = T)

str(soil_df)

soil_df$V1 <- NULL


table(soil_df$Year)


soil_df$Year <- as.integer(soil_df$Year)
################################
# Fixing Data on the basis of EDA - Soil_Nutrient_EDA1
################################

###Soil PH

table(is.na(soil_df$SoilPh)) #6319 is missing (<1%)
table(soil_df$SoilPh>14) # 1652 records have incorrect data (<1%)
# Fix PH assigning mean to any value that is above 14 or is null
soil_df$SoilPh[soil_df$SoilPh>14] <- mean(soil_df$SoilPh[soil_df$SoilPh<=14], na.rm = T)
soil_df$SoilPh[is.na(soil_df$SoilPh)] <- mean(soil_df$SoilPh[soil_df$SoilPh<=14], na.rm = T)



###Electrical Conductivty