library(shiny)
library(shinythemes)
library(tidyverse)

# It is a good idea to load packages that are absolutely necessary to run the
# shiny app. Otherwise, running the app could take a long time or even cause an
# error.

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
             p("One study found that 15% of COVID-19 deaths are related to air pollution. Both short- and long-term exposure to high levels of air pollution can cause a whole variety of adverse health outcomes. 
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

    tabPanel("Data Analysis",
             
    # Here, I explore further the relationship between SO2 and COVID-19 death
    # rate.
    
    titlePanel("Data Analysis"),
             h3("Simple model: Regressing COVID-19 death rate on SO2"),
             p("Given that SO2 had a strongest correlation with COVID-19 death rate, 
               I decided to regress SO2 on COVID-19 death rate to explore further the relationship between the two.
               It would have been ideal if I could control for all other variables that are correlated to both SO2 and COVID-19 death rate;
               however, due to limitation on data availability, I could only control for populations and sizes of cities."),
             img(src = "table_1.png", height = "50%", width = "50%",
                 style = "display: block; margin-left: auto; margin-right: auto;"),
    
             # For some reason, shiny app recognizes an image only if it's
             # inside a folder called www. Also, using style is one way to
             # center the image.
    
             br(),
             p("According to the regression results, COVID-19 death rate of a city with 0 micrograms per cubic meter of SO2, 
               and population and size of 0 is likely to be about 3.8%. 
               I am 95% confident that this value is between 1.3% and 6.2%.
               Now, in reality, there is no such city, and thus the value of the (Intercept) is not particularly meaningful in this model.
               The cofficient of population and that of size tell me that on average, 
               one unit increase in population/size is associated with -0.00000041%/0.205% increase in COVID-19 death rate. 
               Given that the average value of population/size is 5,525,020 people/1.45 GIS, respectively, these two coefficients, again, are not particularly meaningful in this model."),
             p("The coefficient of SO2, however, is more meaningful; 
               on average, every 1 microgram per cubic meter increase is associated with about 0.2% increase in COVID-19 death rate, controlling for city population and city size.
               I am 95% confident that this value is between 0.0068% and 0.039%.
               Note that this is not a causal claim.
               In order for it to show a causal relationship between the two, the regression must control for all confounding variables that are correlated to both COVID-19 death rate and SO2.
               Obviously, my simple model does not control for all confounding variables."),
             p("Still, the model provides an useful general relationship between COVID-19 death rate and SO2.
               For example, given that each of any two cities has 5,525,020 people (mean population) and 1.45 GIS (mean size), 
               city with average SO2 level of 10 micrograms per cubic meter (below the permissible level of 20 micrograms per cubic meter) is likely to have COVID-19 death rate of about 3.8%,
               whereas city with average SO2 level of 30 micrograms per cubic meter (above the permissible level) is likely to have COVID-19 death rate of about 7.7%.
               That is, the latter city is likely to have COVID-19 death rate that is more than double that of the former city."),
             h3("Different COVID-19 death rates for cities with different SO2 level"),
             plotOutput("posterior"),
             p("To explore further, I decided to calculate the expected values of the COVID-19 death rate of 5 cities with differnt SO2 level.
               These expected values are based on the simple model above, and without making many bold assumptions, the results are not generalizable to other countries.
               Before interpreting the distributions, remember that the permissible level of SO2 defined by WHO is 20 micrograms per cubic meter.
               Therefore, the expected values come from 5 cities, 2 of which have the SO2 level below, 1 of which at, and 2 other of which above the permissible level."),
             p("As expected, a city with SO2 of 40 micrograms per cubic meter had the highest expected COVID-19 death rate, centered at around 10%.
               Because it is a distribution, not a fixed value, this value can range anywhere from 3% to 18%. 
               It is, however, very unlikely that the expected value of COVID-19 death rate for this city is below 3% or above 18%.
               Similar interpretations can apply to the remaining four cities.
               Generally, the city with SO2 level of 0 had the lowest mean, and the city with SO2 level of 40 had the highest mean.
               In terms of spread of the distribution, the city with SO2 level of 10 had the narrowest, and the city with SO2 level of 40 had the widest.
               Given that most cities used to fit the simple model had SO2 level around 10 micrograms per cubic meter, 
               it makes sense that the spread is the narrowest for the distribution of the city with SO2 level of 10 and that it gets wider as cities diverge from that SO2 level.")
             ),
    tabPanel("About", 
             
    # Here, I explain about my datasets and include contact information.         
             
             titlePanel("About"),
             p("For this project, I attempt to explore the relationship between COVID-19 death rate and air quality level in China."),
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
      
      # One way to add strings is using the paste() function. Notice how paste()
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
      
      # To focus on cities with at least one death case, I filter for cities
      # with non-zero death cases.
      
      ggplot(aes(x = z,
                 y = death_rate)) +
      geom_point(na.rm = TRUE) +
      geom_smooth(method = "lm",
                  formula = y ~ x,
                  na.rm = TRUE) +
      
      # By including formula and na.rm, we can avoid getting unwanted messages.
      # Messages and/or warnings are sometimes useful, but oftentimes annoying.
      
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
      
      # By setting accuracy equal to 1, I can get rid of any decimal points.
      
      theme_bw()
    
  })
  
  output$posterior <- renderPlot({
    
    pe <- readRDS("pe.rds")
    
    # First step here is to store the previously created rds in pe.
    
    pe %>% 
      ggplot(aes(x = outcome,
                 fill = groups)) +
      geom_histogram(aes(y = after_stat(count/sum(count))),
                     alpha = 0.5,
                     bins = 100,
                     position = "identity") +
      
      # Just as a reminder, this is our usual histogram arguments. Since there
      # are some overlaps between the five distributions, I want to set alpha to
      # some value less than 1.
      
      scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                         breaks = seq(-0.05, 0.2, 0.01)) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      
      # I want both our x- and y-axis in percent format. Also, by setting
      # accuracy equal to 1, I can get rid of any decimal points.
      
      labs(title = "Posterior Distributions of the Expected Value of the COVID-19 Death Rate",
           subtitle = "Cities with SO2 level above the permissible level (20) are likely to have COVID-19 death rates \nmore than twice as high COVID-19 death rates as cities with SO2 level below the permissible level",
           x = "COVID-19 Death Rate",
           y = NULL,
           fill = "SO2 Level \n(Micrograms per Cubic Meter)",
           caption = "Source: Harvard Dataverse") +
      
      # Using \n helps us avoid the texts running over the page since \n means
      # new line.
      
      theme_bw() +
      theme(legend.position = "bottom")
    
  })
  
}

shinyApp(ui = ui, server = server)
