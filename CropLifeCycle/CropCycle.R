library(readr)

# Read the CSV with crop cycle months data. This CSV has been created after domain research.
CropLifeCycleMonths <- read_csv("CropLifeCycle/CropLifeCycleMonths.csv", 
                                col_types = cols(MedianDuration = col_double()))

head(CropLifeCycleMonths, n = 8)
# Note that each numeric value is the number of months starting from new year midnight as 0
#
#   Crop       Season SowStart SowEnd HarvestStart HarvestEnd MedianDuration
#   -----     ------  -------- ------ ------------ ---------- --------------
#   Wheat      Rabi      10.0   11.0         15.5        16.0           6.00
#
# SowStart = 10.0 means "Start sowing on or after 1st October", Sow End = 11.0 means "End sowing before 1st November"
# HarvestStart = 15.5 means "Start harvesting on or after 15 April next year
# HarvestEnd = 16.0 means "End harvesting before 1st May next year" 
#