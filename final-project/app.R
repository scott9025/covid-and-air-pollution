#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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

ui <- navbarPage(
    "Final Project",
    tabPanel("Data 1",
             fluidPage(
               titlePanel("COVID-19 Death Rate"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     "var_plot",
                     "Choose a Response Category",
                     choices = c("City" = "city",
                                 "Province" = "province")
                   ),
                   width = 300),
                 plotOutput("death_plot")
                 )
               )
             ),
    tabPanel("Data 2",
             fluidPage(
               titlePanel("Death Rate vs Air Pollutant"),
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     "var_plot2",
                     "Choose a Response Category",
                     choices = c("CO" = "co",
                                 "NO2" = "no2",
                                 "O3" = "o3",
                                 "PM10" = "pm10",
                                 "PM2.5" = "pm2.5",
                                 "SO2" = "so2")
                   ),
                   width = 300),
                 plotOutput("aq_plot")
               )
             )
           ),
    tabPanel("Model",
             titlePanel("Fitted Model: Regressing COVID-19 Death Rate on Air Pollutants, City Population, and City Size"),
             br(),
             htmlOutput("model"),
             br(),
             p("Interpretation")
             ),
    tabPanel("Discussion",
             titlePanel("Data Sources"),
             p("I found three datasets in Harvard Dataverse that relate to my project topic: 1) Air Quality Data, 2) COVID-19 Data, 3) Census Data"),
             p("All three datasets contain data collected in various cities in China."),
             p("You can access each of the datasets below."),
             a("China Air Quality Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XETLSS"),
             a("China COVID-19 Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/MR5IJN"),
             a("China Census Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GLKQME")),
    tabPanel("About", 
             titlePanel("About"),
             p("For milestone #6, I decided to make an interative graphs of top 10 Chinese cities/provinces with highest COVID-19 deah rate. I filled with total COVID-19 cases to see whether cities with many total cases also have high death rates. Looking at the graphs, it does not seem like it. Also, using stan_glm, I made a fitted model between COVID-19 death rate and SO2 level in China. See the Model panel for details."),
             h3("Url"),
             a("Milestone #3", href = "https://github.com/scott9025/milestone3.git"),
             p("Note that I'm using the previous repo for milestone #6")))

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  output$death_plot <- renderPlot({
    
    # Here, since I'm using top ten cities or provinces, I cannot just define z
    # to be covid_aq$City_EN or covid_aq$Prov_En. I have to define z to be the
    # sliced version of them so that it does not cause a "different length"
    # error when I call on z inside ggplot.
    
    ifelse(input$var_plot == "city",
           z <- covid_aq %>% 
             arrange(desc(death_rate)) %>% 
             slice(1:10) %>% 
             .$City_EN,
           z <- covid_aq %>% 
             arrange(desc(death_rate)) %>% 
             slice(1:10) %>% 
             .$Prov_EN)
    
    ifelse(input$var_plot == "city",
           name <- "Cities",
           name <- "Provinces")
    
    covid_aq %>%
      arrange(desc(death_rate)) %>%
      slice(1:10) %>% 
      ggplot(aes(x = z,
                 y = death_rate,
                 fill = T_C_210114)) +
      geom_col() +
      coord_flip() +
      
      # One way to add strings is using the paste() function. Notices paste()
      # provides white spaces by default. If I don't want white spaces, I can
      # use paste0().
      
      labs(title = paste("Top 10 Chinese", name, "with Highest COVID-19 Death Rate"),
           subtitle = "Having many total cases doesn't necessarily mean having high death rates!",
           x = name,
           y = "Death Rates",
           fill = "Total Cases",
           caption = "Source: Harvard Dataverse") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_classic()
   
    })
  
  output$aq_plot <- renderPlot({
    
    # Since we have 6 possible options, we want to use if, else if, and else,
    # rather than ifelse, which is only useful for 2 options.
    
    if(input$var_plot2 == "co")
    {z <- covid_aq %>%
      filter(death_rate != 0) %>% 
      .$avg_co}
    else if(input$var_plot2 == "no2")
    {z = covid_aq %>% 
      filter(death_rate != 0) %>% 
      .$avg_no2}
    else if(input$var_plot2 == "o3")
    {z = covid_aq %>% 
      filter(death_rate != 0) %>% 
      .$avg_o3}
    else if(input$var_plot2 == "pm10")
    {z = covid_aq %>% 
      filter(death_rate != 0) %>% 
      .$avg_pm10}
    else if(input$var_plot2 == "pm2.5")
    {z = covid_aq %>% 
      filter(death_rate != 0) %>% 
      .$avg_pm2.5}
    else
    {z = covid_aq %>% 
      filter(death_rate != 0) %>% 
      .$avg_so2}
    
    # Defining pollutant would allow us to do 2 things at once: 1) modify the
    # name of the x-axis, and 2) modify the title.
    
    if(input$var_plot2 == "co")
    {pollutant <- "CO"}
    else if(input$var_plot2 == "no2")
    {pollutant <- "NO2"}
    else if(input$var_plot2 == "o3")
    {pollutant <- "O3"}
    else if(input$var_plot2 == "pm10")
    {pollutant <- "PM10"}
    else if(input$var_plot2 == "pm2.5")
    {pollutant <- "PM2.5"}
    else
    {pollutant <- "SO2"}
    
    # Since only SO2 shows a relatively strong correlation against death rate,
    # let's make our subtitle accordingly.
    
    if(input$var_plot2 %in% c("co", "no2", "o3", "pm10", "pm2.5"))
    {strength <- "weak"}
    else
    {strength <- "strong"}
    
    covid_aq %>% 
      filter(death_rate != 0) %>% 
      ggplot(aes(x = z,
                 y = death_rate)) +
      geom_point(na.rm = TRUE) +
      geom_smooth(method = "lm",
                  formula = y ~ x,
                  na.rm = TRUE) +
      labs(title = paste("Correlation between COVID-19 death rate and", 
                         pollutant),
           subtitle = paste("Seems like there is a", 
                            strength, 
                            "correlation between the two"),
           x = paste("Average",
                     pollutant),
           y = "Death Rates",
           caption = "Source: Harvard Dataverse") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_bw()
    
  })
  
  output$model <- renderUI({
    
    print(table_1)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
