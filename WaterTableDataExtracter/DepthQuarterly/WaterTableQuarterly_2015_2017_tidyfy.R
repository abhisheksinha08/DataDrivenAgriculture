
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

setwd('./WaterTableDataExtracter/DepthQuarterly')

# Describe the input CSV
raw.input.csv.file.name <- "water_table_depth_quarterly_2015_2016_untidy.csv"
tidy.output.csv.file.name <- "water_table_depth_quarterly_2015_2016.csv"

quarterly.column.names <- c('District',  'Location', "May 2015", "August 2015", "November 2015" , "January 2016")

input.col_types = cols(
    `May 2015` = col_double(),
    `August 2015` = col_double(),   
    `November 2015` = col_double(),
    `January 2016` = col_double(),
    `Sl. No.` = col_skip())
# End of input CSV description. No more changes should be required in script below.

# START PROCESSING

# Read dataset that was extracted from PDF. TODO: Investigate the warnings due to parsing failures and number of columns mismatch.
water_table_depth_quarterly <- read_csv(raw.input.csv.file.name, col_types = input.col_types)


water.table.df <- as.data.frame(water_table_depth_quarterly)
head(water.table.df)




# There is a column for each quarter dimension i.e. 4 columns. This we need to tidyfy so that there is only one column specifying the quarter
# - thus normalizing 1 row into 4 rows with each row containing only one fact column.
water.table.df.1 <-select(water.table.df, quarterly.column.names)
head(water.table.df.1, 15)

# Tidy up the multiple columns for each quarter using gather
water.table.df.2 <- gather(water.table.df.1,  key="Quarter", value="Depth", 3:6)
head(water.table.df.2, 15)

# Summary of tidy dataset
summary(water.table.df.2)

# END PROCESSING

# Write as CSV to output file
write.csv(water.table.df.2, file = tidy.output.csv.file.name)

