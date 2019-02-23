# Binding data from 2 years (2016 -2018) folders for Greater Manchester Area" 
# Download crime data from "data.police.uk" and unzip the folders & place all of the CSV files into a single folder called 'uk data'

#install.packages(c("tidyverse", "lubridate", "stringr"))
library(tidyverse) ; library(lubridate) ; library(stringr)

# Combine all CSV files form all folders and merge files in one dataframe.
textfiles <- list.files(recursive=T, pattern="*.csv", full.names=T) 
readDatFile <- function(f) { dat.fl <- readLines(f) } 
textdata <- sapply(textfiles, readDatFile) 
files <- names(textdata)
datacrimes<- do.call(rbind, lapply(files , read.csv))
glimpse(datacrimes)

# Drop column not needed , like Context  that has all NAs values, Reported and falls within are same "Greater Manchester Police", hence remove.
#Retain only police recorded crimes by excluding 'Anti-social behaviour' (no outcom specified for such listings)

crimes<- select (datacrimes,-c(Context,Reported.by, Falls.within, Crime.ID))
crimes <- crimes %>% filter(Crime.type != "Anti-social behaviour")
head(crimes)

# Split LSOA.name and retain just the town name
crimes <- separate(crimes, `LSOA.name`, into = c("town", "x"), sep = -5)
crimes<- select (crimes,-c(x))  
crimes$town <- as.factor(crimes$town)
levels(crimes$town)[levels(crimes$town)=="Blackburn with Darwen"] <- "Darwen"
levels(crimes$town)[levels(crimes$town)=="St. Helens"] <- "St.Helens"
levels(crimes$town)
names(crimes)

# Select and rename some parameters 
crimes <- crimes %>% select(month = Month,
                            location = Location,
                            town = town,
                            lsoa = `LSOA.code`,
                            category = `Crime.type`,
                            long = Longitude,
                            lat = Latitude,
                            outcome=Last.outcome.category)

dim(crimes)
sum(is.na(crimes$lsoa))  # check for any Na value 


#Write all selected data to one file csv file for further use 
write_csv(crimes, "crimedata.csv")

