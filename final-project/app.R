library(shiny)
library(shinythemes)
library(tidyverse)

# It is a good idea to load packages that are absolutely necessary to run the
# shiny app. Otherwise, running the app could take a long time.

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

# Here, I am calling the dataset that has been cleaned up in gather.Rmd. Also,
# by setting col_types as above, I can avoid an unnecessary warning message.

ui <- navbarPage(
    "COVID-19 Death Rate and Air Pollution Level in China",
    
    # A rather vague title, but interesting enough to catch attention.
    
    theme = shinytheme("sandstone"),
    
    # The shinytheme package has a range of wonderful themes to choose from.
    
    tabPanel("Background Information",
             
    # Let's begin by talking about some background information on the topic.
    
    titlePanel("Background Information"),
             h3("15% of COVID-19 deaths worldwide could be attributed to air pollution"),
             p("Both short- and long-term exposure to high levels of air pollution can cause a whole variety of adverse health outcomes. 
               It could mean that residents - without knowing it - have pre-damaged lungs or blood vessels, a higher risk for a severe course of the disease."),
             p("Air pollution refers to the release of pollutants into the air that are detrimental to human health and the health of other animals. 
               The WHO estimates that around 7 million people die every year from exposure to polluted air. 
               Ambient air pollution alone caused some 4.2 million deaths in 2016, while household air pollution, e.g. from cooking with pollution fuels indoors caused an estimated 3.8 million deaths in the same period."),
             p("Many different substances are summarised under the term air pollution. 
               The most important ones are particulate matter (PM), ground-level ozone, nitrogen oxides, sulfur oxides, volatile organic compounds and polycyclic aromatic hydrocarbon. 
               A large fraction of air pollutants is man-made. 
               Most air pollution comes from energy use and production. 
               This includes traffic and fuel combustion during industrial production processes."),
    
    # Here, I explain why air pollution might be related to COVID-19 death rate.
             
             h3("Overview of COVID-19 Death Rate in China"),
                       fluidPage(
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
                         ),
             p("Initially, I expected cities/provinces with higher number of total confirmed COVID-19 cases to have higher COVID-19 death rates.
               However, as seen in the plot above, my expectation turned out to be incorrect.
               Wuhan, a city that had the highest number of total confirmed cases, was ranked 9th in terms of COVID-19 death rate.
               Similarly, Hubei, a province that had the highest number of total confirmed cases, was also ranked 9th.")
             ),
    
    # This plot is pretty much self-explanatory. However, it's always good to
    # have some additional explanation.
    
    tabPanel("Data Exploration",
             
    # Next, I decided to play around with the data to see if there are any
    # meaningful correlations between COVID-19 death rate and each of the major
    # pollutants.
    
    titlePanel("Data Exploration"),
            h3("Correlations between COVID-19 death rate and each of the major pollutants"),
                       fluidPage(
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
                       ),
            p("Since various studies have found a positive relationship between COVID-19 death rate and air pollution,
              I expected strong positive correlations between COVID-19 death rate and each of the major pollutants.
              Surprisingly, only SO2 showed a relatively strong positive correlation with COVID-19 death rate.
              Some pollutants, such as NO2 and PM2.5, even showed slightly negative correlations.
              One thing to note here is that I created scatterplots using cities that have at least one COVID-19 death case,
              significantly decreasing the total number of cities used from 339 cities to 74 cities.")
    ),
    
    # It's always a good idea to explain any extra assumption made to create the
    # plot.

    tabPanel("Model",
             
    # Here, I explore further the relationship between SO2 and COVID-19 death
    # rate.
    
    titlePanel("Model"),
             h3("Regressing COVID-19 death rate on SO2"),
             p("Given that SO2 had a strongest correlation with COVID-19 death rate, 
               I decided to regress SO2 on COVID-19 death rate to explore further the relationship between the two.
               It would have been ideal if I could control for all other variables that are correlated to both SO2 and COVID-19 death rate;
               however, due to limitation on data availability, I could only control for populations and sizes of cities."),
             br(),
             img(src = "table_1.png", height = "100%", width = "100%"),
    
             # For some reason, shiny app recognizes an image only if it's
             # inside a folder called www.
    
             br(),
             p("y_i"),
             p("Interpretation")
             ),
    tabPanel("Technical Details",
             titlePanel("Technical Details"),
             p("Compare different fit models using loo_compare.
               To be done after the presentation.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Data Sources"),
             p("I found three datasets in Harvard Dataverse that relate to my project topic: 
               1) Air Quality Data, 2) COVID-19 Data, 3) Census Data"),
             p("All three datasets contain data collected in various cities in China."),
             p("You can access each of the datasets below."),
             a("China Air Quality Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XETLSS"),
             br(),
             a("China COVID-19 Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/MR5IJN"),
             br(),
             a("China Census Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GLKQME"),
             h3("About Me"),
             p("My name is Scott (Seung Woo) Bek and I am an MPP student at the Harvard Kennedy School."), 
             p("You can reach me at: sbek@hks.harvard.edu"),
             a("Github for this project", href = "https://github.com/scott9025/final-project")))

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
      ggplot(aes(x = fct_reorder(z,
                                 death_rate),
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
    
    # Since there are 6 possible options, I use if, else if, and else, rather
    # than ifelse, which is only useful for 2 options.
    
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
  
}

shinyApp(ui = ui, server = server)
