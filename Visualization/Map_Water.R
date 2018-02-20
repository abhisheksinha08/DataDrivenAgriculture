
# ################  Maps for Haryana ####################


# SETUP to be done using super user Terminal i.e. outside R / RStudio:

# Install prerequisite packages on your OS using terminal
#      e.g. Ubuntu-Linux: 
#           sudo apt-get update && sudo apt-get install libgeos-dev libgdal-dev libproj-dev


######## Packages ########
checkAndDownload<-function(packageNames) {
    for(packageName in packageNames) {
        if(!isInstalled(packageName)) {
            install.packages(packageName,repos="http://lib.stat.cmu.edu/R/CRAN") 
        } 
        library(packageName,character.only=TRUE,quietly=TRUE,verbose=FALSE)
    }
}

isInstalled <- function(mypkg){
    is.element(mypkg, installed.packages()[,1])
}


library(dplyr)
library(ggplot2)
library(readr)

packages <- c("sp","ggplot2","plyr","rgeos","maptools","sqldf","RColorBrewer","raster","rgeos","mapproj")
checkAndDownload(packages)
# 'sp'  a package for spatial data
# 'plyr' required for fortify which converts 'sp' data to 
#polygons data to be used with ggplot2
# 'rgeos' required for maptools
# 'maptools' required for fortify - region



# Get India map. Or download from  URL 'http://biogeo.ucdavis.edu/data/gadm2.8/rds/IND_adm2.rds'
POLYGONS_INDIA_DF <- getData("GADM", country = "India", level = 2)  

# Column NAME_1 is for state name
polygons.haryana.df <- subset(POLYGONS_INDIA_DF, NAME_1 == "Haryana")
# Column NAME_2 is for district name
districts.haryana.df <- data.frame(id = 166:186, 
                                   district = polygons.haryana.df@data$NAME_2);  

regions.haryana.df <- fortify(polygons.haryana.df);  # To define Regions for Polygons
regions.haryana.df$id <- as.integer(regions.haryana.df$id);  

map.df <- inner_join(regions.haryana.df, districts.haryana.df, by = "id");  
district.centers.df <- data.frame(gCentroid(polygons.haryana.df, byid = TRUE));
# Hack to show district label at center of district
district.centers.df$district <- districts.haryana.df$district;  


# Join map data with Water Table tidy data using our district id D1,D2 etc

Water_Depth_df <- read.csv("WaterTableDataExtracter/Summarized_Water_Depth.csv", stringsAsFactors = T) %>%
    filter(Year == '2016', Month == 'January') %>% select(District.Id, meanDepth)
head(Water_Depth_df, n = 21)

# Join with District_Masters CSV file to get our district id D1,D2 etc
district.masters.df <- read.csv('Dsitrict_Masters.csv', stringsAsFactors = T)
head(district.masters.df)

district.water.df <- left_join(Water_Depth_df, district.masters.df, by = "District.Id")

map.w.df <- left_join(map.df, district.water.df, by = c("district" = "District.Name"))
head(map.w.df)

# Show map with the water depth depicted by the color of district
ggplot(data = map.w.df) +
    geom_map(map = map.w.df,
             aes(map_id = id, x = long, y = lat, group = group, fill = meanDepth),
             color = "#ffffff", size = 0.25) +
    geom_text(data = district.centers.df, aes(label = district, x = x, y = y), size = 4 ) +
    coord_map() + labs(x = "", y = "", title = "Districts of Haryana") 

# TODO Display yield per hectare on the map and a few of the the input variables


# TODO Display plots of "What-If?" scenarios of yield wrt an input variable



# ######  Using Google Maps Satellite Terrain image
#
# checkAndDownload(c('ggmap'))
# Google Maps API Terms of Service: http://developers.google.com/maps/terms.
# Please cite ggmap if you use it: see citation('ggmap') for details.
#
# Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=Haryana&zoom=8&size=640x640&scale=2&maptype=hybrid&language=en-EN&sensor=false
#googlemap <- qmap('Haryana', zoom = 8, maptype = 'hybrid')
#googlemap


