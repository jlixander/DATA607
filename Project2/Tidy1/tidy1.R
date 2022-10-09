#Load Libraries
library("tidyverse")

#load in data into a dataframe
urlfile <- 'https://raw.githubusercontent.com/jlixander/DATA607/main/Project2/Tidy1/Section8-FY22.csv'
df <- read_csv(url(urlfile))

#Get column names
colnames(df)

#Give each observation a unique ID - helps keep data group after a pivot_longer
df$ID <- seq.int(nrow(df))

#####Data Wrangling/tidying
df_longer <- df |>
  janitor::clean_names() |>
  pivot_longer(cols = l50_1:l80_8, 
               names_to = "income_level", 
               values_to = "income_threshold") |>
  mutate(household_size = case_when(
    endsWith(income_level, "1") ~ "1",
    endsWith(income_level, "2") ~ "2",
    endsWith(income_level, "3") ~ "3",
    endsWith(income_level, "4") ~ "4",
    endsWith(income_level, "5") ~ "5",
    endsWith(income_level, "6") ~ "6",
    endsWith(income_level, "7") ~ "7",
    endsWith(income_level, "8") ~ "8"
    )) |>
  mutate(program = case_when(
    startsWith(income_level, "eli") ~ "extra_low_income",
    startsWith(income_level, "l50") ~ "very_low_income",
    startsWith(income_level, "l80") ~ "low_income"
    )) |>
  mutate(metro = recode(metro, "0" = "non_metro_area", "1" = "metro_area")
           )

df_clean <- df_longer |>
  select(-c(county,income_level)) |>
  mutate(county_name = str_remove_all(county_name, " County"))

#####Data Analysis
