library("dplyr")
library("ggplot2")
library("data.table")

#--------------------------
#Load Datasets
#--------------------------

#Load Yield Data
yield_db <- as.data.frame(readxl::read_xlsx("Yield//Tidy_Data_1.xlsx", sheet = 1))

table(yield_db$Crop)

yield_db$Crop[yield_db$Crop=='Rice'] <-'Paddy'
yield_db$Crop[yield_db$Crop=='Rabi oil seed'] <-'Rabi Oil Seed'

#Remove Rabi Oil Seed
yield_db <- yield_db[yield_db$Crop!='Rabi Oil Seed',]

head(yield_db)


table(yield_db$Block)[table(yield_db$Block)<35]

## Fix Block Names

incorrect_names <- c("B/Garh","B/khera","Badhara","Bapouli","Dadri- I","F.P./Zhorka","F/Nagar","Gharounda","Hodel","L/Majra","Ladawa","Madlouda","Nathusiri","Patoudi","Rojound","Sahalawas","Sewan","Taoru","N/Chaudhary","Hasanpur")
correct_names <-c("Bahadurgarh","Bawani Khera","Badhra","Bapoli","Dadri-I","Ferozpur Jhirka","Farrukh Nagar","Gharaunda","Hodal","Lakhan Majra","Ladwa","Matlauda","Nathusari","Pataudi","Rajound","Salhawas","Siwan","Tauru","Nangal Chaudhary","Hassanpur")

i<-1
for (inc_name in incorrect_names) {
    yield_db$Block[yield_db$Block==inc_name] <- correct_names[i]
    i<-i+1
}


write.csv(yield_db, "Tidy_Data_final.csv", row.names = F)
