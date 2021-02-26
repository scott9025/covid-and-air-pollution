#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- navbarPage(
    "Final Project Milestone #3",
    tabPanel("Discussion",
             titlePanel("Data Sources"),
             p("I found two datasets in Harvard Dataverse that relate to my project topic: 1) Air Quality Data and 2) COVID-19 Data."),
             p("Both datasets contain data collected in various cities in China."),
             p("You can access each of the datasets below."),
             a("China Air Quality Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XETLSS"),
             a("China COVID-19 Data", href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/MR5IJN")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Url"),
             a("Milestone #3", href = "https://github.com/scott9025/milestone3.git")))

# Define server logic required to draw a histogram
server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
