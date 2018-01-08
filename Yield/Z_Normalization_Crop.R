
#Read Tidy Crop Data
yield_all <- read.csv("Tidy_Data_final.csv", stringsAsFactors = F)

str(yield_all)

yield_all$Yield <- as.numeric(yield_all$Yield)

yield_all <- yield_all[is.na(yield_all$Yield)==F,]

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
    write.csv(data_crop, paste0(crop,"_Normalized",".csv"))
    data_crop <- read.csv(paste0(crop,"_Normalized",".csv"), stringsAsFactors = F)
    data_crop$Yield <- z_score_nrom(data_crop$Yield)
    write.csv(data_crop, paste0(crop,"_Normalized",".csv"))
}


