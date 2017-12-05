library(ggplot2)
library(leaflet)
library(rgdal)
library(dplyr)

setwd("C:\\Users\\Aditya\\Desktop\\Soil Data")
Haryana <- read.csv("haryana_soil_nutrient.csv")


Final1 <- 
  Haryana %>% filter(!is.na(SoilPh)) %>%
  group_by(DistrictName) %>%
  summarise(SoilPhCount = n(),
            SoilPhMean = mean(SoilPh, rm.na=TRUE),
            SoilPhMedian = median(SoilPh),
            SoilPhSD = sd(SoilPh))


Final2 <- 
  Haryana %>% filter(!is.na(ElectricalConductivity)) %>%
  group_by(DistrictName) %>%
  summarise(ElectricalConductivityCount = n(),
            ElectricalConductivityMean = mean(ElectricalConductivity, rm.na=TRUE),
            ElectricalConductivityMedian = median(ElectricalConductivity),
            ElectricalConductivitySD = sd(ElectricalConductivity))


Final3 <- 
  Haryana %>% filter(!is.na(OrganicCarbon)) %>%
  group_by(DistrictName) %>%
  summarise(OrganicCarbonCount = n(),
            OrganicCarbonMean = mean(OrganicCarbon, rm.na=TRUE),
            OrganicCarbonMedian = median(OrganicCarbon),
            OrganicCarbonSD = sd(OrganicCarbon))


Final4 <- 
  Haryana %>% filter(!is.na(Nitrogen)) %>%
  group_by(DistrictName) %>%
  summarise(NitrogenCount = n(),
            NitrogenMean = mean(Nitrogen, rm.na=TRUE),
            NitrogenMedian = median(Nitrogen),
            NitrogenSDn = sd(Nitrogen))


Final5 <- 
  Haryana %>% filter(!is.na(Phosphorous)) %>%
  group_by(DistrictName) %>%
  summarise(PhosphorousCount = n(),
            PhosphorousMean = mean(Phosphorous, rm.na=TRUE),
            PhosphorousMedian = median(Phosphorous),
            PhosphorousSD = sd(Phosphorous))


Final6 <- 
  Haryana %>% filter(!is.na(Potassium)) %>%
  group_by(DistrictName) %>%
  summarise(PotassiumCount = n(),
            PotassiumMean = mean(Potassium, rm.na=TRUE),
            PotassiumMedian = median(Potassium),
            PotassiumSD = sd(Potassium))


Final7 <- 
  Haryana %>% filter(!is.na(Sulphur)) %>%
  group_by(DistrictName) %>%
  summarise(SulphurCount = n(),
            SulphurMean = mean(Sulphur, rm.na=TRUE),
            SulphurMedian = median(Sulphur),
            SulphurSD = sd(Sulphur))



Final8 <- 
  Haryana %>% filter(!is.na(Zinc)) %>%
  group_by(DistrictName) %>%
  summarise(ZincCount = n(),
            ZincMean = mean(Zinc, rm.na=TRUE),
            ZincMedian = median(Zinc),
            ZincSD = sd(Zinc))



Final9 <- 
  Haryana %>% filter(!is.na(Iron)) %>%
  group_by(DistrictName) %>%
  summarise(IronCount = n(),
            IronMean = mean(Iron, rm.na=TRUE),
            IronMedian = median(Iron),
            IronSD = sd(Iron))



Final10 <- 
  Haryana %>% filter(!is.na(Copper)) %>%
  group_by(DistrictName) %>%
  summarise(CopperCount = n(),
            CopperMean = mean(Copper, rm.na=TRUE),
            CopperMedian = median(Copper),
            CopperSD = sd(Copper))


Final11 <- 
  Haryana %>% filter(!is.na(Magnesium)) %>%
  group_by(DistrictName) %>%
  summarise(MagnesiumCount = n(),
            MagnesiumMean = mean(Magnesium, rm.na=TRUE),
            MagnesiumMedian = median(Magnesium),
            MagnesiumSD = sd(Magnesium))


Final12 <- 
  Haryana %>% filter(!is.na(Boron)) %>%
  group_by(DistrictName) %>%
  summarise(BoronCount = n(),
            BoronMean = mean(Boron, rm.na=TRUE),
            BoronMedian = median(Boron),
            BoronSD = sd(Boron))

Final <- Final1 %>% left_join(Final2) %>%  left_join(Final3) %>%  
  left_join(Final4) %>%  left_join(Final5) %>%  left_join(Final6) %>%  
  left_join(Final7) %>%  left_join(Final8) %>%  left_join(Final9) %>%  
  left_join(Final10) %>%  left_join(Final11) %>%  left_join(Final12)




#Reading a Shape Files and Filtering the districts of Haryana

tmp <- "C://Users//Aditya//Desktop//Soil Data//IND_adm_shp"
TAL <- readOGR(dsn = tmp, layer = "IND_adm2", encoding = "UTF-8")
Haryana.map <- TAL[TAL$NAME_1 == "Haryana",]

Haryana.map$DistrictName <- as.character(Haryana.map$NAME_2)
Final$DistrictName = as.character(Final$DistrictName)
df1 <- sp::merge(x= Haryana.map, y=Final)

pal <- colorQuantile("YlGn", NULL, n = 9)

state_popup <- paste0("<strong>District : </strong>", 
                      df1$DistrictName, 
                      "<br><strong>SoilPhMean </strong>", 
                      df1$SoilPhMean)


df1 %>% leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView( 76.5, 30, 7 ) %>% addPolygons(fillColor = ~pal(SoilPhMean),
                                         fillOpacity = 0.8, 
                                         color = "#BDBDC3", 
                                         weight = 1,
                                         popup = state_popup)


state_popup <- paste0("<strong>District : </strong>", 
                      df1$DistrictName, 
                      <br><strong>SoilPhMedian</strong>,
                      df1$SoilPhMedian)


df1 %>% leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView( 76.5, 30, 7 ) %>% addPolygons(fillColor = ~pal(SoilPhMedian),
                                         fillOpacity = 0.8, 
                                         color = "#BDBDC3", 
                                         weight = 1,
                                         popup = state_popup)

state_popup <- paste0("<strong>District : </strong>", 
                      df1$DistrictName, 
                      "<br><strong>SoilPhSD</strong>"
                      ,df1$SoilPhSD)


df1 %>% leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView( 76.5, 30, 7 ) %>% addPolygons(fillColor = ~pal(SoilPhSD),
                                         fillOpacity = 0.8, 
                                         color = "#BDBDC3", 
                                         weight = 1,
                                         popup = state_popup)







































Final_Coord <- read.csv(s)
Final_Coord <- left_join(Final,Block_Coord)

Final_Coord %>% filter(!is.na(lon)) %>% 
  leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView( 76.5, 30, 7 ) %>% addPolygons()
addHeatmap(lng = ~lon, lat = ~lat, intensity = ~SoilPhMean
           ,blur = 20, max = 0.05, radius =10 )

m <- leaflet(blocks) %>%
  setView( 76.5, 30, 7 ) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

