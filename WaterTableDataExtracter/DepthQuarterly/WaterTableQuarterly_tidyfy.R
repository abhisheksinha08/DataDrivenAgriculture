
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

setwd('./WaterTableDataExtracter/DepthQuarterly')

# Describe the input CSV
raw.input.csv.file.name <- "water_table_depth_quarterly_2014_2015_untidy.csv"
tidy.output.csv.file.name <- "water_table_depth_quarterly_2014_2015.csv"

quarterly.column.names <- c('District',  'Location', "May 2014", "August 2014", "November 2014" , "January 2015")

input.col_types = cols(
    `May 2014` = col_double(),
    `August 2014` = col_double(),   
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
has.district.names.flag <- as.vector(! sapply(water.table.df['District'] ,  is.na ) )
head(has.district.names.flag, 15)

# Print summary. Note that Total number of TRUE values = the number of districts = 18
summary(has.district.names.flag)

# Find and flag all the first occurring Locations of each district
total.rows <- nrow(water.table.df)
current.district.name <- NA
water.table.df.0 <- cbind(has.district.names.flag, water.table.df)

# Assign a district to each row. 
for(i in 1:total.rows) {
  row <- water.table.df.0[i,]
  
  if(row['has.district.names.flag'] == TRUE){
    district.name.allcaps <-    row[['District']]
    district.name.first.char <- substr (district.name.allcaps, 1, 1)
    district.name.tail <- substr( district.name.allcaps, 2, str_length(district.name.allcaps))
    current.district.name <- paste( district.name.first.char,str_to_lower(district.name.tail), sep = '')
  }
  water.table.df.0[i,][['District']] <- current.district.name
}
# End of district value population for all locations.
head(water.table.df.0, 15)

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

