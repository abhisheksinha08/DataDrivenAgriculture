library(data.table)
library(dplyr)


soil_df <- fread("Soil_Nutrient_Year.csv", header = T, stringsAsFactors = F, showProgress = T)

str(soil_df)

soil_df$V1 <- NULL


soil_df$Year <- as.integer(soil_df$Year)
################################
# Fixing Data on the basis of EDA - Soil_Nutrient_EDA1 (http://rpubs.com/abhishek08/348055)
################################

# Fix PH assigning mean to any value that is above 14 or is null
soil_df$SoilPh[soil_df$SoilPh>14] <- mean(soil_df$SoilPh[soil_df$SoilPh<=14], na.rm = T)

cols_to_drop <- c("Nitrogen","Copper","Boron")
soil_df_selected <- soil_df %>% select(-one_of(cols_to_drop))
soil_df_selected <- as.data.frame(soil_df_selected)


# Insert mean value in missing data
mean_rm_na <- function(x)
{
    x[is.na(x)] <- mean(x, na.rm = T)
    return(x)
}

soil_df_selected[,5:14] <- as.data.frame(sapply(soil_df_selected[,5:14], mean_rm_na))

str(soil_df_selected)

soil_summarized <- soil_df_selected %>% group_by(DistrictId, DistrictName, BlockId, BlockName, Year) %>% 
    summarize(SoilPh = mean(as.numeric(SoilPh), na.rm = T),
              ElectricalConductivity = mean(ElectricalConductivity), OrganicCarbon=mean(OrganicCarbon),
                  Phosphorous = mean(Phosphorous), Potassium = mean(Potassium), Sulphur = mean(Sulphur), Zinc = mean(Zinc),
                  Iron = mean(Iron), Magnesium = mean(Magnesium))
soil_summarized <- as.data.frame(soil_summarized)

str(soil_summarized)

write.csv(soil_summarized, "Soil_Nutrient_Summarized.csv", row.names = F, col.names = T)
