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

# source("make_covid_death_plot.R")

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
    "Final Project Milestone #5",
    tabPanel("Data",
             fluidPage(
               titlePanel("COVID-19 Death Rate Plot"),
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
    tabPanel("Discussion",
             titlePanel("Data Sources"),
             p("I found two datasets in Harvard Dataverse that relate to my project topic: 1) Air Quality Data and 2) COVID-19 Data."),
             p("Both datasets contain data collected in various cities in China."),
             p("You can access each of the datasets below."),
             a("China Air Quality Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XETLSS"),
             a("China COVID-19 Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/MR5IJN")),
    tabPanel("About", 
             titlePanel("About"),
             p("For milestone #5, I decided to graph top 10 Chinese cities with highest COVID-19 deah rate. I filled with total COVID-19 cases to see whether cities with many total cases also have high death rates. Looking at the graph, it does not seem like it."),
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
}

# Run the application 
shinyApp(ui = ui, server = server)
