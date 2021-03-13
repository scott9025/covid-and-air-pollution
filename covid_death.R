library(dplyr)
library(lubridate)
library(skimr)
library(tidyverse)

covid_confirmed <- read_csv("data/covid19/City_Confirmed_20200115_20210114.csv")
covid_death <- read_csv("data/covid19/City_Death_20200115_20210114.csv")

covid <- covid_confirmed %>% 
  left_join(., covid_death)

covid_death_plot <- covid %>% 
  select(City_EN, T_C_210114, T_D_210114) %>% 
  mutate(death_rate = T_D_210114 / T_C_210114) %>% 
  arrange(desc(death_rate)) %>% 
  filter(death_rate > 0.07) %>% 
  ggplot(aes(x = City_EN,
             y = death_rate)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Chinese Cities with Highest COVID-19 Death Rate",
       x = "Cities",
       y = "Death Rates")

saveRDS(covid_death_plot, "milestone3/covid_death_plot.RDS")
