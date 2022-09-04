library(tidyverse)
library(dplyr)
library("anytime")

#load in data into a dataframe and parse columns
polls <- read_csv("Data/congress-generic-ballot/generic_topline.csv", 
                  col_types = cols(.default = col_character())
                  )
#View df
polls

#check how data was loaded
class(polls)
#check if any columns in dataframe have na
polls %>% summarise_all(~ sum(is.na(.)))

#FORMATTING DATE COLUMN
#Set locale to avoid return NA values for dates
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
polls$modeldate <- anydate(polls$modeldate)

#Remove timestamp and subgroup column
polls = subset(polls, select = -c(timestamp,subgroup))

#Rename columns
polls <- polls %>%
  rename(ModelDate = modeldate,
         PercDemEst = dem_estimate,
         PercDemHigh = dem_hi,
         PercDemLow = dem_lo,
         PercRepEst = rep_estimate,
         PercRepHigh = rep_hi,
         PercRepLow = rep_lo,
         )


#Format all columns holding a percent value to 2 decimal places
cols.perc <- c("PercDemEst","PercDemHigh","PercDemLow","PercRepEst","PercRepHigh","PercRepLow")
polls[cols.perc] <- sapply(polls[cols.perc],as.numeric)

polls <- polls %>% 
  mutate_if(is.numeric, round, digits = 2)


#Subset data to the year 2017 and 2018
polls_2017 <- polls %>% 
  filter(ModelDate < '2018-01-01')

polls_2018 <- polls %>% 
  filter(ModelDate >= '2018-01-01')