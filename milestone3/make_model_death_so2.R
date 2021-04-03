library(dplyr)
library(lubridate)
library(skimr)
library(tidyverse)
library(rstanarm)
library(xtable)
library(broom)
library(gtsummary)
library(gt)

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

# For the fitted model, I decided to remove all rows with death rate = 0, since
# I wanted to focus on cities with at least one death case. Also, I multiplied
# death rates by 100, since running stan_glm with regular death_rate returned
# really small coefficients (marked 0.0 with the model only showing up to the
# nearest 10th).

covid_aq_no_zero <- covid_aq %>% 
  filter(death_rate != 0) %>% 
  mutate(death_rate_100 = death_rate * 100)

# Death rate vs Average SO2

fit_so2 <- stan_glm(data = covid_aq_no_zero,
         formula = death_rate_100 ~ avg_so2, 
         seed = 13,
         refresh = 0)

summary_fit_so2 <- summary(fit_so2) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  gt()
  



# From Stack Overflow: If you call launch_shinystan on the object and go to the
# Estimate tab, there is a link to Generate LaTeX table that gives you a bunch
# of options to check on the left and it outputs the syntax on the right.
