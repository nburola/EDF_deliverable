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

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# read in data
fishe <- read_csv(here::here("raw_data", "alldata_combined.csv"))

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Evaluating Adaptive Management Strategies for Climate Resilient Fisheries"),
  tabsetPanel(
    tabPanel("Introduction",
             strong(h3("About the Project (Background)", style = "color:steelblue; font-family:Helvetica")),
             em(strong(h4("What is the Enviromental Defense Fund (EDF) and what are they doing with data-limited fisheries management?", style = "color:skyblue"))), 
             tags$img(src = "EDF.jpeg", height = "100px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
             p("EDF is a nonprofit environmental advocacy group known for working on issues such as global warming, ecosystem restoration, and oceans. Despite the immense value of fisheries, the majority of global fisheries lack the appropriate data to utilize conventional scientific stock assessment methods. With these data-limited fisheries, EDF has developed the Framework for Integrated Stock and Habitat Evaluation (FISHE). The goal of this framework is to provide scientific guidance to fisheries managers with minimal resources in 11 steps. Each step of the framework is designed to promote the sustainable management of data-limited fisheries"),
             br(),
             
             em(strong(h4("How is climate change accounted for in the FISHE framework?", style = "color:skyblue"))),
             tags$img(src = "FISHE.png", height = "300px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
             p("FISHE is designed to capture the inherently dynamic nature of fisheries. It was not designed to address expected 
environmental changes stemming from global climate change. As climate change has already started to impact fish stocks worldwide, it is imperative that FISHE be robust to climate-induced variations. In addition, the framework also needs to minimize environmental and economic risk"),
             br(),
             
             em(strong(h4("Which parts of FISHE are most likely going to be affected by climate change?", style = "color:skyblue"))),
             p("content"),
             hr(),
             
             strong(h3("How was data created?", style = "color:steelblue; font-family:Helvetica")),
             em(strong(h4("Was the data collected or was it all simulated by model runs?", style = "color:skyblue"))),
             p("content"),
             br(),
             
             em(strong(h4("What type of model are you running (generic that can be applied to all vs specific that can only be applied to a few?)", style = "color:skyblue"))),
             "content",
             hr(),
             strong(h3("Variable Glossary (defined variables)", style = "color:steelblue; font-family:Helvetica")),
             em(strong(h4("What are the variables that are going to be defined and utilized in this Shiny App?", style = "color:skyblue"))),
             p("- Growth rates (slow, medium, and fast)"),
             p("- Harvest Control Rules (HCRs)"),
             p("- Error reduction"),
             p("- Assessment intervals"),
             p("- Climate severity (moderate -> severe)"),
             hr(),
             strong(h3("About FISHE Framework (What is the FISHE Framework?)", style = "color:steelblue; font-family:Helvetica")),
             em(strong(h4("What is the Frameworkfor Intergrated Stock and Habitat Evaluation (FISHE)", style = "color:skyblue"))),
             p("contents"),
             br(),
             
             em(strong(h4("What 11 steps are there in this circular process?", style = "color:skyblue"))),
             p("contents"),
             br(),
             
             em(strong(h4("How is FISHE utilized throughout the world?", style = "color:skyblue"))),
             p("contents"),
             br(),
             
             em(strong(h4("What case studies can we utilize to show the efficiency of FISHE in other countries?", style = "color:skyblue"))),
             p("contents")
    ),
    tabPanel("Mapping Vulnerabilities", "content"),
    tabPanel("Comparing Trade-Offs", "content"),
    tabPanel("Biomass Over Time", "content")
  )
)

server <- function(input, output){}

shinyApp(ui = ui, server = server)