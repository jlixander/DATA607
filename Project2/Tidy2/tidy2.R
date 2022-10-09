#Load Libraries
library("tidyverse")


#load in data into a dataframe
urlfile <- 'https://raw.githubusercontent.com/jlixander/DATA607/main/Project2/Tidy2/brain_stroke.csv'
df <- read_csv(url(urlfile))

#Give each observation a unique ID - helps keep data group after a pivot_longer
df$ID <- seq.int(nrow(df))

#####Data Wrangling/tidying
#Remove encoded values from columns
df <- df |>
  janitor::clean_names() |>
  mutate(hypertension = recode(hypertension, "0" = "Negative", "1" = "Positive"),
         stroke = recode(stroke, "0" = "Negative", "1" = "Positive"),
         heart_disease = recode(heart_disease, "0" = "Negative", "1" = "Positive")
         )

#Set column data types
df$age<-round(as.numeric(df$age), 0)

#Check for na values in each column
df %>%
  summarise_all(funs(sum(is.na(.))))

#Consolidate all condition columns
df_longer <- df |>
  pivot_longer(cols = hypertension:heart_disease, 
               names_to = "condition", 
               values_to = "condition_status")
