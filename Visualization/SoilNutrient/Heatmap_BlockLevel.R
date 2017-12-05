library(leaflet.extras)
library(ggplot2)
library(RgoogleMaps)
library(ggmap)

setwd("C:\\Users\\Aditya\\Desktop\\Soil Data")
Haryana <- read.csv("haryana_soil_nutrient.csv")
attach(Haryana)

Final1 <- 
  Haryana %>% filter(!is.na(SoilPh)) %>%
  group_by(BlockName) %>%
  summarise(SoilPhCount = n(),
  SoilPhMean = mean(SoilPh, rm.na=TRUE),
  SoilPhMedian = median(SoilPh))


Final2 <- 
  Haryana %>% filter(!is.na(ElectricalConductivity)) %>%
  group_by(BlockName) %>%
  summarise(ElectricalConductivityCount = n(),
            ElectricalConductivityMean = mean(ElectricalConductivity, rm.na=TRUE),
            ElectricalConductivityMedian = median(ElectricalConductivity))


Final3 <- 
  Haryana %>% filter(!is.na(OrganicCarbon)) %>%
  group_by(BlockName) %>%
  summarise(OrganicCarbonCount = n(),
            OrganicCarbonMean = mean(OrganicCarbon, rm.na=TRUE),
            OrganicCarbonMedian = median(OrganicCarbon))


Final4 <- 
  Haryana %>% filter(!is.na(Nitrogen)) %>%
  group_by(BlockName) %>%
  summarise(NitrogenCount = n(),
            NitrogenMean = mean(Nitrogen, rm.na=TRUE),
            NitrogenMedian = median(Nitrogen))


Final5 <- 
  Haryana %>% filter(!is.na(Phosphorous)) %>%
  group_by(BlockName) %>%
  summarise(PhosphorousCount = n(),
            PhosphorousMean = mean(Phosphorous, rm.na=TRUE),
            PhosphorousMedian = median(Phosphorous))


Final6 <- 
  Haryana %>% filter(!is.na(Potassium)) %>%
  group_by(BlockName) %>%
  summarise(PotassiumCount = n(),
            PotassiumMean = mean(Potassium, rm.na=TRUE),
            PotassiumMedian = median(Potassium))


Final7 <- 
  Haryana %>% filter(!is.na(Sulphur)) %>%
  group_by(BlockName) %>%
  summarise(SulphurCount = n(),
            SulphurMean = mean(Sulphur, rm.na=TRUE),
            SulphurMedian = median(Sulphur))



Final8 <- 
  Haryana %>% filter(!is.na(Zinc)) %>%
  group_by(BlockName) %>%
  summarise(ZincCount = n(),
            ZincMean = mean(Zinc, rm.na=TRUE),
            ZincMedian = median(Zinc))



Final9 <- 
  Haryana %>% filter(!is.na(Iron)) %>%
  group_by(BlockName) %>%
  summarise(IronCount = n(),
            IronMean = mean(Iron, rm.na=TRUE),
            IronMedian = median(Iron))



Final10 <- 
  Haryana %>% filter(!is.na(Copper)) %>%
  group_by(BlockName) %>%
  summarise(CopperCount = n(),
            CopperMean = mean(Copper, rm.na=TRUE),
            CopperMedian = median(Copper))


Final11 <- 
  Haryana %>% filter(!is.na(Magnesium)) %>%
  group_by(BlockName) %>%
  summarise(MagnesiumCount = n(),
            MagnesiumMean = mean(Magnesium, rm.na=TRUE),
            MagnesiumMedian = median(Magnesium))


Final12 <- 
  Haryana %>% filter(!is.na(Boron)) %>%
  group_by(BlockName) %>%
  summarise(BoronCount = n(),
            BoronMean = mean(Boron, rm.na=TRUE),
            BoronMedian = median(Boron))

Final <- Final1 %>% left_join(Final2) %>%  left_join(Final3) %>%  
        left_join(Final4) %>%  left_join(Final5) %>%  left_join(Final6) %>%  
        left_join(Final7) %>%  left_join(Final8) %>%  left_join(Final9) %>%  
        left_join(Final10) %>%  left_join(Final11) %>%  left_join(Final12)


Blocks <- unique(Final$BlockName)
BlockName <- as.character(Blocks)
BlockName$Block_Coord <- geocode(BlockName)
Block_Coord<-as.data.frame(cbind(as.character(BlockName[1:120]),BlockName$Block_Coord))

names(Block_Coord) <- c("BlockName","lon","lat")

Final_Coord <- left_join(Final,Block_Coord)


Base <- Final_Coord %>% filter(!is.na(lon))

leaflet(Base) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( 76.5, 30, 7 ) %>%
  addHeatmap(lng = ~lon, lat = ~lat, intensity = ~SoilPhMean,
             blur = 20, max = .15, radius = 10)


leaflet(Base) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( 76.5, 30, 7 ) %>%
  addHeatmap(lng = ~lon, lat = ~lat, intensity = ~SoilPhMedian,
             blur = 20, max = .15, radius = 10)


leaflet(Base) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView( 76.5, 30, 7 ) %>%
  addHeatmap(lng = ~lon, lat = ~lat, intensity = ~SoilPhCount,
             blur = 20, max = .15, radius = 10)