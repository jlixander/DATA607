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

#Label US Territories
us_ter <- c("GU", "MP", "AS", "VI", "PR")

df_clean <- df_clean |>
  mutate(territory = case_when(
    state_alpha %in% us_ter ~ "us_territory",
    !(state_alpha %in% us_ter) ~ "non_us_territory"
    ))


# #####Data Analysis
# #1 Which States have the lowest and highest income limit average across programs?
# The top 5 states with the lowest average income limits are:
#   1) PR
#   2) MP
#   3) AS
#   4) MS
#   5) AR
# 
# 
# #2 Which states have the highest income limits average across programs?
# The top 5 states with the highest average income limits are:
#   1) DC
#   2) MA
#   3) HI
#   4) CT
#   5) NJ
# 
# #3 Do metro areas tend to have higher income limits?
#  As expected, it was found that metropolitan areas have higher income thresholds across all programs.
# 
# #4 Do US territories, on average, have higher income limits compared to the continental US?
# Most US territory limits fall on the bottom half of all states.

df_clean |>
  ggplot(aes(x= reorder(factor(state_alpha), -income_threshold), y = income_threshold, fill = program)) +
  stat_summary(fun = "mean", geom = "bar") +
  coord_flip()

df_clean |>
  ggplot(aes(x= reorder(factor(state_alpha), -income_threshold), y = income_threshold, fill = program)) +
  stat_summary(fun = "mean", geom = "bar") +
  coord_flip() +
  facet_wrap(~metro)

df_clean |>
  ggplot(aes(x= reorder(factor(state_alpha), -income_threshold), y = income_threshold, fill = program)) +
  stat_summary(fun = "mean", geom = "bar") +
  coord_flip() +
  facet_wrap(~territory)
