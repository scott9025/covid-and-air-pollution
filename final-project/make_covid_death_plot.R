library(dplyr)
library(lubridate)
library(skimr)
library(tidyverse)

covid_aq <- read_csv("clean/covid_aq.csv",
                     col_types =
                       cols(
                         City_EN = col_character(),
                         Prov_EN = col_character(),
                         death_rate = col_double(),
                         T_C_210114 = col_double(),
                         T_D_210114 = col_double(),
                         avg_co = col_double(),
                         avg_no2 = col_double(),
                         avg_o3 = col_double(),
                         avg_pm10 = col_double(),
                         avg_pm2.5 = col_double(),
                         avg_so2 = col_double()
                       ))

# COVID-19 death rate plot (city-level)

# Here, I want to make a bar graph that shows which of the cities in China have
# high COVID-19 death rates. By filling with total cases at the end of the
# collection period, I try to show that cities with many total cnfirmed cases
# don't necessarily have high death rates.

covid_aq %>% 
  arrange(desc(death_rate)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = City_EN,
             y = death_rate,
             fill = T_C_210114)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Chinese Cities with Highest COVID-19 Death Rate",
       subtitle = "Having many total cases doesn't necessarily mean having high death rates!",
       x = "Cities",
       y = "Death Rates",
       fill = "Total Cases",
       caption = "Source: Harvard Dataverse") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic()

# COVID-19 death rate plot (province-level)

# I do the same, but this time with provinces instead of cities. It might be
# more intuitive if I could show this using the map of China, but for now let's
# settle on the bar plots.

covid_aq %>% 
  arrange(desc(death_rate)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = Prov_EN,
             y = death_rate,
             fill = T_C_210114)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Chinese Provinces with Highest COVID-19 Death Rate",
       subtitle = "Having many total cases doesn't necessarily mean having high death rate!",
       x = "Provinces",
       y = "Death Rates",
       fill = "Total Cases",
       caption = "Source: Harvard Dataverse") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic()
