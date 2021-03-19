library(dplyr)
library(lubridate)
library(skimr)
library(tidyverse)

covid_confirmed <- read_csv("data/covid19/City_Confirmed_20200115_20210114.csv",
                            col_types = 
                              cols(
                                .default = col_double(),
                                City_CH = col_character(),
                                City_EN = col_character(),
                                Prov_CH = col_character(),
                                Prov_EN = col_character()
                              ))
covid_death <- read_csv("data/covid19/City_Death_20200115_20210114.csv",
                        col_types = 
                          cols(
                            .default = col_double(),
                            City_CH = col_character(),
                            City_EN = col_character(),
                            Prov_CH = col_character(),
                            Prov_EN = col_character()
                          ))

covid <- covid_confirmed %>% 
  left_join(., covid_death)

covid_death_plot <- covid %>% 
  select(City_EN, T_C_210114, T_D_210114) %>% 
  mutate(death_rate = T_D_210114 / T_C_210114) %>% 
  arrange(desc(death_rate)) %>% 
  filter(death_rate > 0.07) %>% 
  ggplot(aes(x = City_EN,
             y = death_rate,
             fill = T_C_210114)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 Chinese Cities with Highest COVID-19 Death Rate",
       subtitle = "Having many total cases doesn't necessarily mean having high death rate!",
       x = "Cities",
       y = "Death Rates",
       fill = "Total Cases",
       caption = "Source: Harvard Dataverse") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic()
