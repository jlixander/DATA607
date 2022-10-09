#Load Libraries
library("tidyverse")

#Load data
df <- read_csv("C:/Users/jlixa/Desktop/CUNY/Fall22/DATA607_Rep/Project2/Tidy2/brain_stroke.csv")

#Set column data types
df$age<-round(as.numeric(df$age), 0)

#Give each observation a unique ID - helps keep data group after a pivot_longer
df$ID <- seq.int(nrow(df))

#Remove encoded values from columns
df <- df |>
  janitor::clean_names() |>
  mutate(hypertension = recode(hypertension, "0" = "Negative", "1" = "Positive"),
         stroke = recode(stroke, "0" = "Negative", "1" = "Positive"),
         heart_disease = recode(heart_disease, "0" = "Negative", "1" = "Positive")
         )

#Check for na values in each column
df %>%
  summarise_all(funs(sum(is.na(.))))

#Consolidate all condition columns
df_longer <- df |>
  pivot_longer(cols = hypertension:heart_disease, 
               names_to = "condition", 
               values_to = "condition_status")
