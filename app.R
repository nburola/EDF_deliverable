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
             tags$img(src = "vulnerable.jpg", height = "300px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
             p("As the FISHE framework contains multiple steps within steps, it is important to first determine whether FISHE will continue to perform as expected in the face of climate change. This was done by establishing a baseline without climate change and with climate change in the Group Project (GP) model. It was necessary to identify the aspects of FISHE that were most vulnerable to climate change:"), 
             br(), 
             p("Step 7: Reference Points: theoretical values that allow you to determine the status of the fishery. If you are close to your fishing target, then you could be underfishing. However, if you are close to your limit,  there could be a case of severe overfishing."), 
             br(), 
             p("Step 8: Harvest Control Rules (HCR): simple rules that direct the action to be taken from a resulting target. It could be simple enough such as fish more, reduce by x amount of percentage, or even stop fishing altogether."), 
             br(), 
             p("Step 9: Detailed Assessments: detailed assessments are needed to estimate actual fishery values. Estimated sampling error is a potential factor which may affect results with Tier 1 sampling error representing 10%, Tier 2 representing 30%, and Tier 3 representing 50%"), 
             br(), 
             p("Step 11: Implementation and Adaptation: implementation involves choosing Harvest Control Measures (HCM) which describe how the HCR of Step 8 will be implemented. Adaptation involves the process of re-visiting fishery goals and re-evaluating the state of the fishery."),
             br(),
             
             strong(h3("How was the data created?", style = "color:steelblue; font-family:Helvetica")),
             em(strong(h4("Was the data collected or was it all simulated by model runs?", style = "color:skyblue"))),
             tags$img(src = "parameters.png", height = "300px", width = "500px", style="display: block; margin-left: auto; margin-right: auto"),
             p("With the baseline model, the following terms were used to represent variables such as growth (r), carrying capacity (k), inital biomass (b0), shape parameter (p), and time. Adding climate into the baseline model was accomplished by using a change in growth (Δr) and a change in carrying capacity (Δk). To simulate management decisions, fishing pressure, harvest rules, assessment intervals, and sampling error was added to the model. All these variables combined allowed 100,000 + simulations to be computed with the model."),
             br(),
             
             em(strong(h4("What type of model are you running (generic that can be applied to all vs specific that can only be applied to a few?)", style = "color:skyblue"))),
             p("It is important to note that this model is a generic model that can be adapated to a wide range of situations. This is not meant to be applied to specific case studies as every fishery is in a different state prior to being assessed."),
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
             tags$img(src = "FISHE.png", height = "300px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
             p("EDF designed FISHE to assist with the management of data-limited fisheries. FISHE is an 11 step adaptive management framework designed to help fisheires managers conduct simplified stock assessments and evaluate potential management options with limited inputs. FISHE can be used and adapted for any type of stock in any geographic location. FISHE is being used to guide management reforms around the world, including Baja California, the Philippines, and Belize."), 
             br(), 
             
             em(strong(h4("What 11 steps are there in this circular process?", style = "color:skyblue"))),
             p("FISHE is composed of the following steps:"),
             br(),
             p("Step 1: Goal Setting"), 
             p("Step 2: Ecosystem Assessment"), 
             p("Step 3: Vulnerability Assessment"),
             p("Step 4: Initial Stock Assessment"), 
             p("Step 5: Prioritization"), 
             p("Step 6: Performance Indicators"), 
             p("Step 7: Reference Points"), 
             p("Step 8: Harvest Control Rules (HCR)"), 
             p("Step 9: Detailed Assessments"), 
             p("Step 10: Interpretation"), 
             p("Step 11: Implementation and Adaptation"), 
             br(), 
             p("FISHE is an inclusive process which considers all stakeholders that are involved. Steps 1-8 of the framework are completed before any data is analyzed or assessed. Steps 9-11 involves the actual stock assessment after the management framework is set."), 
             br(), 
             
             em(strong(h4("How is FISHE utilized throughout the world?", style = "color:skyblue"))),
             p("As an adaptable management framework, FISHE has been developed to be applicable to fisheries in any type of system. Data collected through the 11 step framework will be equally applicable to these methods. FISHE is utilized in several countries including Cuba to protect the near endangered lane snapper (Lutjanus synagris) which is a great example of how FISHE can work from inside a country to build management at the same time from the outside. Within Indonesia, FISHE is being implemented in its beginning phases to protect the blue swimming crab (Portunus armatus). The Indonesian government is looking at performance indicators and working on gathering data to be able to make management decisions. Other locations where FISHE is being used includes Myanmar, the Philippines, Belize, Chile, Peru, Mexico, and all FishForever sites where FISHE is referred to as an Adaptive Fisheries Management (AFM) framework."),
             br(),
             
             em(strong(h4("What case studies can we utilize to show the efficiency of FISHE in other countries?", style = "color:skyblue"))),
             p("To assist with implementing FISHE, EDF has a 
hypothetical case study concerning a nearshore tropical reef fishery in the Carribbean. Fishing is important to this Carribbean island country both economically and culturally. There is serious concern about the depletion of finfish in its nearshore multi-species tropical reef fish fishery. Outside fishing pressure from nearby islands and other countries has placed additional unaccounted-for-stress on finfish populations. Data has proved unreliable for conducting sound fishery management assessments and thus FISHE has been called in to come up with a solid plan"), 
             br(), 
             p("Step 1: Goal Setting: the goals for this fishery include producing good but not necessarily maximum yields from the main target fisheries, providing good dive tourism and recreational fishing experiences, and maintaining the coral reef ecosystem that supports fishing and tourism by rebuilding depleted stocks and maintaining total fish biomass at appropriate levels. "),
             br(), 
             p("Step 2: Ecosystem Assessment: the ecosystem seems relatively healthy, receiving a score > 0.5 but < 1.0 (score between 0.5-1.0 indicates that coral cover, fish species richness, and the presence of grazers and invertivores are at healthy levels)"), 
             br(), 
             p("Step 3: Vulnerability Assessment: a Productivity Susceptibility Analysis (PSA) was conducted on 29 fish species targeted by the finish fishery to assess the vulnerability of each species."), 
             br(), 
             p("Step 4: Inital Stock Assessment: assessing the status of individual fish targets with a focus on highly vulnerable to overfishing targets: Goliath grouper, Nassau grouper, and red snapper."), 
             br(), 
             p("Step 5: Priortization: After organizing the priority table for all three highly vulnerable species, it was found that the red snapper, goliath grouper, and nassau grouper were all deemed high priority targets for management decisions due to a combination of high vulnerability and high depletion rates"), 
             br(), 
      
    ),
    tabPanel("Mapping Vulnerabilities", "content"),
    tabPanel("Comparing Trade-Offs", "content"),
    tabPanel("Biomass Over Time", "content")
  )
)

server <- function(input, output){}

shinyApp(ui = ui, server = server)