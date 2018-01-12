
#Read Tidy Crop Data
yield_all <- read.csv("Yield/Tidy_Data_final.csv", stringsAsFactors = F)

str(yield_all)

yield_all$Yield <- as.numeric(yield_all$Yield)

yield_all <- yield_all[is.na(yield_all$Yield)==F,]


yield_all$Block[yield_all$Block=='B/garh'] <-'Ballabgarh'

yield_all$District[yield_all$District=='Sonepat'] <-'Sonipat'
yield_all$District[yield_all$District=='Yamuna Nagar'] <-'Yamunanagar'
yield_all$District[yield_all$District=='Mahendergarh'] <-'Mahendragarh'

# Get District and Block Ids from Master
distric_master <- xlsx::read.xlsx("Dsitrict_Block_Masters.xlsx", sheetIndex = 1)
block_master <- xlsx::read.xlsx("Dsitrict_Block_Masters.xlsx", sheetIndex = 2)
str(distric_master)
str(block_master)


#Joining with District and Block Master
temp_df <-  yield_all %>% left_join(block_master, by = c("Block" = "Block.Name"))
temp_df <-  temp_df %>% left_join(distric_master, by = c("District" = "District.Name"))
str(temp_df)
yield_all <- temp_df %>% select(Year, District = District.Id, Block = Block.Id, Crop, Yield)

yield_all$Block <- as.character(yield_all$Block)
yield_all$District <- as.character(yield_all$District)


#Get All Crop Names
crop_names <- unique(yield_all$Crop)


# Function for Z Score Normalization
z_score_nrom <- function(x)
{
    mean_x = mean(x, na.rm = T)
    sd_x = sd(x, na.rm = T)
    norm_x <- (x - mean_x)/sd_x
    return(norm_x)
}

#Get CSV for each crop
for (crop in crop_names) {
    data_crop <- subset(yield_all, Crop==crop)
    write.csv(data_crop, paste0("Yield/",crop,"_Normalized",".csv"))
    data_crop <- read.csv(paste0("Yield/",crop,"_Normalized",".csv"), stringsAsFactors = F)
    data_crop$Yield <- z_score_nrom(data_crop$Yield)
    write.csv(data_crop, paste0("Yield/",crop,"_Normalized",".csv"))
}


