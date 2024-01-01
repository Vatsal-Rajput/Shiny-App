#Installing packages which be helping further in merging tables
install.packages("data.table")

library(data.table)
library(dplyr)
library(tidyverse)
library(rvest)

#Extracting the webpage
webpage <- read_html("https://www.totaltraininfo.com/")

type_of_trains <- html_table(webpage)

#Extracting links from the anchor tags of a particular class
type_of_trains_links <- webpage %>% html_elements(".dropdown.offset300 a")

type_of_trains_links <- type_of_trains_links %>% html_attr("href")

#Taking out train_names from the links
train_names <- substr(type_of_trains_links,26,nchar(type_of_trains_links)-4)

list_of_tables <- list()

#Going to webpage of each train and extracting data_table of each type of train in a list 
for(i in 1:29){
  train_webpage <- read_html(paste0("https:",type_of_trains_links[i]))
  train_table <- train_webpage %>% html_table()
  list_of_tables[[i]] <- train_table[[3]]
  
}

# Saving the list_of_tables as an R data file
file <- save(list_of_tables, file = "list_of_tables.Rdata")

#Loading previous data
raw_data <- load("list_of_tables.Rdata")

list_train <- list()

#Cleaning Data
for(i in 1:29){
  list_train[[i]] <- list_of_tables[[i]][1:11]
  
  list_train[[i]] <- list_train[[i]]%>%
    filter(row_number() %% 2 != 0)
  
  list_train[[i]] <- list_train[[i]] %>% mutate(Train_Type = train_names[i])
}

#Merging Tables of each type of train
all_trains_table <- rbindlist(list_train)

#Taking out train nos which will be used for scraping from a different site
train_nos <- all_trains_table %>% select(`Train#`)

file <- save(train_nos, file = "train_nos.Rdata")
write.csv(train_nos,file = "train_nos.csv")

#Extraction of avg train delays from another site

numbers <- read.csv("train_nos.csv")
numbers <- numbers$Train#
len <- length(numbers)
delays <- numeric(length=len)

# extracting the average delay for the last one year for each train
for(i in 1:len)
{
  train.links[i] =  paste0("https://runningstatus.in/history/",numbers[i], "/thisyear")
  
  html <- read_html(train.links[i])
  
  data <- html %>% html_elements(".text-danger") %>% html_text() 
  
  res <- as.numeric(gsub(" Mins", "", data[4]))
  
  delays[i] <- res
  
}

# converting the data into a csv file
data <- data.frame(TrainNo = numbers, AvgDelay = delays)
write.csv(data, file="final_delays.csv")

train_delays <- read_csv("final_delays.csv")

#Merging the train_delays with the original table
all_trains_table <- all_trains_table %>% mutate(train_delays[3])

file <- save(all_trains_table, file = "all_trains_table.Rdata")

load("all_trains_table.Rdata")
train_station_code <- all_trains_table$Dest

travelkhana_webpage <- read_html("https://www.travelkhana.com/rail-infoindian-railway-station-list-with-station-code/")

StationNamesTable <- travelkhana_webpage %>% html_table()

StationNamesTable <- StationNamesTable[[2]]

Destination_Full_Name <- list(length = length(train_station_code))

for (i in 1:length(train_station_code)) {
  temp <-StationNamesTable %>% filter(StationNamesTable$X3==train_station_code[i])
  Destination_Full_Name[i] <- temp[2]
}

all_trains_table <- all_trains_table %>% mutate(Destination_Full_Name)

all_trains_table$Dist. <- sapply(strsplit(all_trains_table$Dist., " "), `[`, 1)

all_trains_table$`Avg Speed` <- sapply(strsplit(all_trains_table$`Avg Speed`, " "), `[`, 1)

all_trains_table$`Avg Speed` <- as.numeric(all_trains_table$`Avg Speed`)

names(all_trains_table)[names(all_trains_table) == "Avg_Speed.1"] <- "Avg_Speed"

lat_lon <-read_csv("lat_lon.csv")

all_trains_table$Source_Lat <- 0
all_trains_table$Source_Lon <- 0
all_trains_table$Dest_Lat <- 0
all_trains_table$Dest_Lon <- 0
lat_lon <- unique(lat_lon)



for(i in 1:dim(all_trains_table)[1]){
  all_trains_table$Source_Lat[i] <- lat_lon[which(all_trains_table$Source[i]==(lat_lon)$...2),3] 
  all_trains_table$Source_Lon[i] <- lat_lon[which(all_trains_table$Source[i]==(lat_lon)$...2),4] 
  all_trains_table$Dest_Lat[i] <- lat_lon[which(all_trains_table$Dest[i]==(lat_lon)$...2),3] 
  all_trains_table$Dest_Lon[i] <- lat_lon[which(all_trains_table$Dest[i]==(lat_lon)$...2),4] 
}


all_trains_table$Source_Lat <- as.numeric(as.character(all_trains_table$Source_Lat))
all_trains_table$Source_Lon <- as.numeric(as.character(all_trains_table$Source_Lon))

file <- save(all_trains_table, file = "merged.Rdata")


