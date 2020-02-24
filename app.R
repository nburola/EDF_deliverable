#Names: Nathaniel Burola, Siya Qui, Mauricio Collado 
#Subject: ESM 244: Advanced Data Analysis Shiny App 
#Date: 2/23/2020 

#Installing the packages to construct a Shiny App (commented out in order to avoid long processing)
#install.packages("shiny", repos = "https://cran.rstudio.com", dependencies = TRUE)
#install.packages("shinydashboard", repos = "https://cran.rstudio.com", dependencies = TRUE)
#install.packages("tidyverse", repos = "https://cran.rstudio.com", dependencies = TRUE)
#install.packages("here", repos = "https://cran.rstudio.com", dependencies = TRUE)
#install.packages("janitor", repos = "https://cran.rstudio.com", dependencies = TRUE)
#install.packages("shinythemes", repos = "https://cran.rstudio.com", dependencies = TRUE)

#Attaching the packages to construct a Shiny App 
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(here)
library(janitor)

#Reading in the data with the here function 
alldata <- read_csv(here("raw_data", "alldata_combined.csv")) %>%  
  clean_names()

#Constructing the basic user interface (UI) with the shinydashboard package 
ui <- navbarPage("Navigation Bar", 
                 theme = shinytheme("yeti"), 
                 tabPanel("Introduction", 
                          h1("Some giant text"), 
                          p("Here is some regular text")), 
                 tabPanel("Mapping Vulnerabilities", 
                          h1("Some giant text"), 
                          p("Here is some regular text")), 
                 tabPanel("Comparing Trade-Offs", 
                          h1("Some giant text"), 
                          p("Here is some regular text")), 
                 tabPanel("Biomass Over Time", 
                          h1("Some giant text"), 
                          p("Here is some regular text")),
                mainPanel("Main Panel Text"))

#Writing the server function line of code that connects inputs to outputs 
server <- function(input, output) ({})

#Writing the line of code that connects the UI to the server 
shinyApp(ui = ui, server = server)


