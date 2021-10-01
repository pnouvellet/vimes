#####################################################################
# Clean contact tracing data to create files ready for analysis:
# Private repo has identifying information - remove before sharing
# 
#####################################################################

rm(list=ls())

library("lubridate")
#source("R/elapsed_months.R")

study_regs <- c("Lindi", "Mtwara") # Set study regions as Lindi and Mtwara Regions only

# Format as date
start_date <- as.Date("2011-01-01") # Set start date for data
end_date <- as.Date("2020-12-31") # end date

########################################################################

# Import and process Animal contact tracing data
# animals <- read.csv("data/Tanzania_Animal_Contact_Tracing_2020.csv", stringsAsFactors = FALSE)
animals <- read.csv("tests_sh/animal_cases.csv", stringsAsFactors = FALSE) # data pulled on 07/05/2021

animalstz <- animals[which(animals$Region %in% study_regs),] # Subset data for only study regions
nrow(animalstz) # 612

# Format dates
animalstz$Symptoms.started <- as.Date(animalstz$Symptoms.started, format="%Y-%m-%d")
animalstz$Date.bitten <- as.Date(animalstz$Date.bitten, format="%Y-%m-%d")

head(animalstz)

# Setup a date proxy for the year when cases happened
animalstz$Date_proxy <- as.Date(ifelse(!is.na(animalstz$Date.bitten), animalstz$Date.bitten,
                                       ifelse(!is.na(animalstz$Symptoms.started), animalstz$Symptoms.started, NA)),
                                origin="1970-01-01")
animalstz <- animalstz[which(animalstz$Date_proxy >= start_date),] # Subset data from 2011 to present
nrow(animalstz) # 549

# Create a column for year
animalstz$Year <- year(animalstz$Date_proxy)
table(animalstz$Year, animalstz$Suspect, useNA="always")

# Focus on probable rabid animals
table(animalstz$Suspect, animalstz$Uncertainty, useNA = "always")

# Remove unknown animal from the list of species

animalstz <- subset(animalstz, animalstz$Species == "Wildlife: Honey badger" |animalstz$Species == "Wildlife: Hyena" |
                      animalstz$Species == "Wildlife: Jackal" |animalstz$Species == "Wildlife: Leopard" |
                      animalstz$Species =="Domestic dog"|animalstz$Species=="Cat")
animalstz <- animalstz[which(animalstz$Suspect=="Yes"),]; nrow(animalstz) # 549

animalstz$Time_diff <- animalstz$Date_proxy - start_date
animalstz$Time_diff <- as.numeric(animalstz$Time_diff)


#write.csv(animalstz, "tests_sh/animals_ungrouped_districts.csv", row.names = FALSE)

