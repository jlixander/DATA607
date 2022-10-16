#Load Libraries
library("tidyverse")

#load in data into a dataframe
urlfile <- 'https://raw.githubusercontent.com/jlixander/DATA607/main/Project2/Tidy3/nba_player_stats.csv'
df <- read_csv(url(urlfile))

#Drop Columns
df2 <- df |>
  select(-c(DIST_FEET,GP,DIST_MILES))

#turn df into long format
df_longer <- df2 |>
  pivot_longer(cols = W:L, 
               names_to = "GAME_OUTCOME", 
               values_to = "GAME_CNT") |>
  pivot_longer(cols = AVG_SPEED:AVG_SPEED_DEF,
               names_to = "PLAYER_SPEED",
               values_to = "SPEED")

#####Data Analysis
ggplot(df, aes(x=DIST_MILES, y=AVG_SPEED)) +
  geom_point()

ggplot(df, aes(x=DIST_MILES, y=W)) +
  geom_point()

ggplot(df, aes(x=DIST_MILES_DEF, y=L)) +
  geom_point()

df |>
  ggplot(aes(x=DIST_MILES, y = W, colour= factor(TEAM))) +
  stat_summary(fun = "mean", geom = "point")
