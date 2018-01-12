library(stringr)
library(dplyr)
library(xlsx)

w_df <- read.csv("WaterTableDataExtracter/DepthQuarterly/water_table_depth_quarterly_2013_2014.csv", stringsAsFactors = F)
w_df <- rbind(w_df, read.csv("WaterTableDataExtracter/DepthQuarterly/water_table_depth_quarterly_2014_2015.csv", stringsAsFactors = F))
w_df <- rbind(w_df, read.csv("WaterTableDataExtracter/DepthQuarterly/water_table_depth_quarterly_2015_2016.csv", stringsAsFactors = F))
w_df$X <- NULL

str(w_df)


#Generate Quarter Year
w_df$Month <- substr(w_df$Quarter,1, str_length(w_df$Quarter)-5)
w_df$Year <- substr(w_df$Quarter,str_length(w_df$Quarter)-3, str_length(w_df$Quarter))
w_df$Quarter <- NULL


# Get District and Block Ids from Master
distric_master <- read.csv("Dsitrict_Masters.csv", stringsAsFactors = T)
block_master <- read.csv("Block_Masters.csv", stringsAsFactors = T)
str(distric_master)
str(block_master)

#Join with District Master 
temp_df <-  w_df %>% left_join(distric_master, by = c("District" = "District.Name"))


unique(temp_df[is.na(temp_df$District.Id),"District"])
#None of the missing districts looks correct

temp_df <- temp_df[is.na(temp_df$District.Id)==F,]
str(temp_df)

temp_df$District <- NULL
temp_df$Location <- NULL


summarized_data <- temp_df %>% group_by(District.Id, Year, Month) %>% summarize(meanDepth = mean(Depth, na.rm = T)) %>% as.data.frame()

summarized_data <- summarized_data[is.na(summarized_data$meanDepth)==F,]
str(summarized_data)

write.csv(summarized_data, "WaterTableDataExtracter/Summarized_Water_Depth.csv", row.names = F)
