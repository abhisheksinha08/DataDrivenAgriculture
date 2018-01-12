library(data.table)
library(dplyr)


soil_df <- fread("SoilNutrientDataExtracter/Soil_Nutrient_Year.csv", header = T, stringsAsFactors = F, showProgress = T)

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



soil_summarized$BlockName[soil_summarized$BlockName=='Ambala-Ii'] <-'Ambala-II'
soil_summarized$BlockName[soil_summarized$BlockName=='Shahzadpur'] <-'Sahazadpur'
soil_summarized$BlockName[soil_summarized$BlockName=='Jagadhri'] <-'Jagadhari'
soil_summarized$BlockName[soil_summarized$BlockName=='Sadaura (Part)'] <-'Sadaura'
soil_summarized$BlockName[soil_summarized$BlockName=='Shahbad'] <-'Shahabad'
soil_summarized$BlockName[soil_summarized$BlockName=='Guhla'] <-'Gulha'
soil_summarized$BlockName[soil_summarized$BlockName=='Gharaunda (Part)'] <-'Gharaunda'
soil_summarized$BlockName[soil_summarized$BlockName=='Nissing At Chirao'] <-'Nissing'

soil_summarized$BlockName[soil_summarized$BlockName=='Madlauda'] <-'Matlauda'
soil_summarized$BlockName[soil_summarized$BlockName=='Gannaur'] <-'Ganaur'
soil_summarized$BlockName[soil_summarized$BlockName=='Sonipat'] <-'Sonepat'
soil_summarized$BlockName[soil_summarized$BlockName=='Bhattu Kalan'] <-'Bhattukalan'
soil_summarized$BlockName[soil_summarized$BlockName=='Nathusari Chopta'] <-'Nathusari'
soil_summarized$BlockName[soil_summarized$BlockName=='Hansi-I'] <-'Hansi I'
soil_summarized$BlockName[soil_summarized$BlockName=='Hansi-Ii'] <-'Hansi-II'
soil_summarized$BlockName[soil_summarized$BlockName=='Hisar-I'] <-'Hisar I'
soil_summarized$BlockName[soil_summarized$BlockName=='Hisar-Ii'] <-'Hisar II'
soil_summarized$BlockName[soil_summarized$BlockName=='Maham'] <-'Meham'
soil_summarized$BlockName[soil_summarized$BlockName=='Matannail'] <-'Matanhail'
soil_summarized$BlockName[soil_summarized$BlockName=='Ateli Nangal'] <-'Ateli'
soil_summarized$BlockName[soil_summarized$BlockName=='Mahendragarh'] <-'Mahendergarh'
soil_summarized$BlockName[soil_summarized$BlockName=='Nangal Chaudhry'] <-'Nangal Chaudhary'
soil_summarized$BlockName[soil_summarized$BlockName=='Nizampur'] <-'Nijampur'
soil_summarized$BlockName[soil_summarized$BlockName=='Khol At Rewari'] <-'Khol'
soil_summarized$BlockName[soil_summarized$BlockName=='Ferozepur Jhirka'] <-'Ferozpur Jhirka'
soil_summarized$BlockName[soil_summarized$BlockName=='Punahana'] <-'Punhana'
soil_summarized$BlockName[soil_summarized$BlockName=='Taoru'] <-'Tauru'



# Get District and Block Ids from Master
distric_master <- xlsx::read.xlsx("Dsitrict_Block_Masters.xlsx", sheetIndex = 1)
block_master <- xlsx::read.xlsx("Dsitrict_Block_Masters.xlsx", sheetIndex = 2)
str(distric_master)
str(block_master)

#Join with District Master 
temp_df <-  soil_summarized %>% left_join(block_master, by = c("BlockName" = "Block.Name"))
temp_df <-  temp_df %>% left_join(distric_master, by = c("DistrictName" = "District.Name"))



soil_summarized <- temp_df %>% select(District.Id, Block.Id, everything()) %>% select(-DistrictId, -DistrictName, -BlockId, -BlockName)
head(soil_summarized)

write.csv(soil_summarized, "Soil_Nutrient_Summarized.csv", row.names = F, col.names = T)


