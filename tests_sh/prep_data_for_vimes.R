## This is sourced from the try_vimes script

#rm(list=ls())

# Import data and prepare for analysis
SE_Tanz <- read.csv(here::here("tests_sh/animal_cases.csv"))

SE_Tanz$Date_started <- as.Date(SE_Tanz$Symptoms.started, format = "%Y-%m-%d")
#head(SE_Tanz)

#SE_Tanz$Symptoms.started[1:5]
#SE_Tanz$Date_started[1:5]

# we want only complete cases for symptoms started and location data
SE_Tanz <-SE_Tanz[complete.cases(SE_Tanz$Symptoms.started),]
SE_Tanz <- SE_Tanz[complete.cases(SE_Tanz$Latitude),]
SE_Tanz <- SE_Tanz[complete.cases(SE_Tanz$Longitude),]

SE_Tanz$Species <- as.factor(SE_Tanz$Species)

levels(SE_Tanz$Species) <- list (Dog = c("Domestic dog"), 
                                 Wildlife = c("Wildlife: Baboon","Wildlife: Banded mongoose","Wildlife: Bat eared fox",
                                              "Wildlife: Civet", "Wildlife: Dwarf mongoose", "Wildlife: Genet", 
                                              "Wildlife: Honey badger","Wildlife: Hyena", "Wildlife: Jackal", "Wildlife: Leopard",
                                              "Wildlife: Lion", "Wildlife: Monkey", "Wildlife: Serval","Wildlife: Striped hyena" ,
                                              "Wildlife: Sykes monkey", "Wildlife: White tailed mongoose", "Wildlife: Wild cat", "Wildlife: Wildebeest"),
                                 Livestock = c("Livestock: Cow", "Livestock: Donkey", "Livestock: Goat",
                                               "Livestock: Pig", "Livestock: Sheep" ),
                                 Cat = c("Cat"),
                                 Unknown = ("Unknown"), Other = ("Other"))

#levels(SE_Tanz$Species)

## Need to add a time difference as a numeric on to the data set for use within the function

start <- as.Date("2011-01-01")  

SE_Tanz$Time_diff <- SE_Tanz$Date_started - start
SE_Tanz$Time_diff <- as.numeric(SE_Tanz$Time_diff)

