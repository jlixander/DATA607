library(tidyverse)
library(RColorBrewer)


#Set color palette
hue <- brewer.pal(3, "Pastel2")

#load in data into a dataframe
df1 <- read_csv("data.csv", 
                  col_types = cols(.default = col_character())
                  )

#Remove all whitespaces from column names
names(df1) <- gsub(" ", "_", names(df1))

View(df1)

#Give column 1 and 2 names
colnames(df1)[1] <- "Airline"
colnames(df1)[2] <- "OnTime_Delayed"

#Remove empty row
df1 <- df1[-c(3),]

#Fill empty values for Airline
df1 <- df1 %>% 
  fill(Airline)

df_longer <- df1 %>%
  pivot_longer(cols = c(Los_Angeles,Phoenix, San_Diego, San_Francisco, Seattle), 
               names_to = "Airport", 
               values_to = "Flt_cnt"
               )
#Check data type of dataframe
sapply(df_longer, class)

#Convert count to int
df_longer$Flt_cnt <- as.numeric(as.character(df_longer$Flt_cnt))

#Subset dataset to ontime and delayed
ot_df <- filter(df_longer, OnTime_Delayed == "on time")
delay_df <- filter(df_longer, OnTime_Delayed == "delayed")

#Create a Line graph showing how on time flights compare for airlines
ot_df |>
  ggplot(aes(x=Airport, y=Flt_cnt, group=Airline, color=Airline))+
  geom_line() +
  labs(
    title = "On Time Flight Count by Airline and Airport",
    x = "Departing Airport",
    y = "Count of On Time Flights",
  )

#Create a Line graph showing how delayed flights compare for airlines
delay_df |>
  ggplot(aes(x=Airport, y=Flt_cnt, group=Airline, color=Airline))+
  geom_line() +
  labs(
    title = "Delayed Flight Count by Airline and Airport",
    x = "Departing Airport",
    y = "Count of Delayed Flights",
  )

#For each airport find the proportion of On Time vs Delayed flights.
perc <- df_longer |>
  group_by(Airport, OnTime_Delayed) |>
  summarise(Flt_cnt = sum(Flt_cnt))
ggplot(perc, aes(fill=OnTime_Delayed, y=Flt_cnt, x=Airport)) + 
  geom_bar(position="fill", stat="identity")
         
