
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

setwd('./WaterTableDataExtracter/DepthQuarterly')

# Describe the input CSV
raw.input.csv.file.name <- "water_table_depth_quarterly_2014_2015_untidy.csv"
tidy.output.csv.file.name <- "water_table_depth_quarterly_2014_2015.csv"

quarterly.column.names <- c('District',  'Location', "March 2014", "July 2014", "November 2014" , "January 2015")

input.col_types = cols(
    `March 2014` = col_double(),
    `July 2014` = col_double(),   
    `November 2014` = col_double(),
    `January 2015` = col_double(),
    `Sl. No.` = col_skip())
# End of input CSV description. No more changes should be required in script below.

# START PROCESSING

# Read dataset that was extracted from PDF. TODO: Investigate the warnings due to parsing failures and number of columns mismatch.
water_table_depth_quarterly <- read_csv(raw.input.csv.file.name, col_types = input.col_types)


water.table.df <- as.data.frame(water_table_depth_quarterly)
head(water.table.df)


# First, Tidy up the composite column named [District:]Location
#  In the PDF, District value is in a vertically merged cell, to be read for multiple location rows.
#  Therefore, in the CSV extracted from the PDF, the district name is only available for the first location row.
#  Hence the result of text extraction is that only the first Location in each district contains the District value


# Create a logical vector with all rows that appear in the dataset first in the district
has.district.names.flag <- as.vector( sapply(water.table.df['[District:]Location'] ,   str_detect, regex(":") ) )
head(has.district.names.flag, 15)

# Print summary. Note that Total number of TRUE values = the number of districts = 18
summary(has.district.names.flag)

# Find and flag all the first occurring Locations of each district
water.table.df <- cbind(has.district.names.flag, water.table.df)

total.rows <- nrow(water.table.df)
current.district.name <- NA
current.location.name <- NA
district.name <- vector(mode = "character", length = total.rows )
location.name <- vector(mode = "character", length = total.rows)

# Assign a district to each row. 
for(i in 1:total.rows) {
  row <- water.table.df[i,]
  
  district.location <- row[['[District:]Location']]
  current.location.name <- district.location
  if(row['has.district.names.flag'] == TRUE){
    colon.index <- str_locate(district.location , regex(":"))
    current.district.name <- substr ( district.location , 1 , (colon.index - 1) )
    current.location.name <- substr ( district.location , (colon.index + 1) , str_length(district.location))
  }
  # print(paste(current.district.name, '  :  ' , current.location.name)) 
  district.name[i] <- current.district.name 
  location.name[i] <- current.location.name 
}

water.table.df.0 <- cbind(water.table.df, district.name, location.name)
names(water.table.df.0)[names(water.table.df.0) == 'district.name'] <- quarterly.column.names[1]
names(water.table.df.0)[names(water.table.df.0) == 'location.name'] <- quarterly.column.names[2]
# End of district value population for all locations.


# There is a column for each quarter dimension i.e. 4 columns. This we need to tidyfy so that there is only one column specifying the quarter
# - thus normalizing 1 row into 4 rows with each row containing only one fact column.
water.table.df.1 <-select(water.table.df.0, quarterly.column.names)
head(water.table.df.1, 15)

# Tidy up the multiple columns for each quarter using gather
water.table.df.2 <- gather(water.table.df.1,  key="Quarter", value="Depth", 3:6)
head(water.table.df.2, 15)

# Summary of tidy dataset
summary(water.table.df.2)

# END PROCESSING

# Write as CSV to output file
write.csv(water.table.df.2, file = tidy.output.csv.file.name)

