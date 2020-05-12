#Names: Nathaniel Burola, Chase Brewster, Gracie White, and Sara Orofino
#Subject: Somefin' FISHE Shiny App 
#Date: 5/11/2020

#Installing the packages to construct a Shiny App (commented out in order to avoid long processing)
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("here")
#install.packages("janitor")
#install.packages("shinythemes")
#install.packages("purrr")

##############Load Packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(here)
library(janitor)
library(plotly)
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(lubridate)
library(purrr)

# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.

# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

##############Read in Data

######Old Model

#No Climate Response
fishe_1 <- read_csv(here::here("raw_data", "alldata_combined.csv"))

#Perfect Climate Response
fishe_2 <- read_csv(here::here("raw_data", "fmsy_u_all.csv"))

#Precautionary Climate Response
fishe_3 <- read_csv(here::here("raw_data", "proxy_old_all_publics.csv"))

######New Model

#No Climate Response
fishe_4 <- read_csv(here::here("raw_data", "noclose_all_c.csv"))

#Perfect Climate Response
fishe_5 <- read_csv(here::here("raw_data", "noclose_all_u.csv"))

#Precautionary Climate Response
fishe_6 <- read_csv(here::here("raw_data", "proxy_new_all_publics.csv"))

##############Wrangle Data

######HCR

###Prop Tables

#No Climate Response
hcr_none <- fishe_1 %>% 
  filter(error != 0.5) %>% 
  filter(r_s < 0) %>% 
  group_by(hcr) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate(simulations = sum(closed + over + good)) %>% 
  mutate(prop_good = good / simulations)

#Perfect Climate Response
hcr_perf <- fishe_2 %>% 
  filter(error != 0.5) %>% 
  filter(r_s < 0) %>% 
  group_by(hcr) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(simulations = sum(closed + over + good)) %>% 
  mutate(prop_good = good / simulations)

#Precautionary Climate Response
hcr_proxy <- fishe_3 %>% 
  filter(error != 0.5) %>% 
  filter(ai != 0) %>% #filter out no repeat (none of the other data has this ai)
  filter(r_s < 0) %>% 
  group_by(hcr) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(simulations = sum(closed + over + good)) %>% 
  mutate(prop_good = good / simulations)

###Graph-Ready Data

#No Climate Response
hcr_none_g <- data.frame(hcr = hcr_none$hcr, response = rep("none", 10),
                          prop_good = hcr_none$prop_good)

#Perfect Climate Response
hcr_perf_g <- data.frame(hcr = hcr_perf$hcr, response = rep("ideal", 10),
                          prop_good = hcr_perf$prop_good)

#Precautionary Climate Response
hcr_proxy_g <- data.frame(hcr = hcr_proxy$hcr, response = rep("precautionary", 10),
                            prop_good = hcr_proxy$prop_good)

#No Climate Response & Perfect Climate Response
hcr_none_perf_g <- rbind(hcr_none_g, hcr_perf_g)

#No Climate Response & Precautionary Climate Response
hcr_none_proxy_g <- rbind(hcr_none_g, hcr_proxy_g)

#Perfect Climate Response & Precautionary Climate Response
hcr_perf_proxy_g <- rbind(hcr_perf_g, hcr_proxy_g)

#All Climate Responses
hcr_all_g <- rbind(hcr_none_g, hcr_perf_g, hcr_proxy_g)

######Error

###Prop Tables

#No Climate Response
err_none <- fishe_1 %>% 
  filter(r_s < 0) %>% 
  group_by(error) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(simulations = sum(closed + over + good)) %>% 
  mutate(prop_good = good / simulations)

err_none$error <- as.factor(err_none$error)

#Perfect Climate Response
err_perf <- fishe_2 %>%
  filter(r_s < 0) %>% 
  group_by(error) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate(simulations = sum(closed + over + good)) %>% 
  mutate(prop_good = good / simulations)

err_perf$error <- as.factor(err_perf$error)

#Precautionary Climate Response
err_proxy <- fishe_3 %>% 
  filter(ai != 0) %>% #filter out no repeat (none of the other data has this ai)
  filter(r_s < 0) %>% 
  group_by(error) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate(simulations = sum(closed + over + good)) %>% 
  mutate(prop_good = good / simulations)

err_proxy$error <- as.factor(err_proxy$error)

###Graph-Ready Data

#No Climate Response
err_none_g <- data.frame(error = err_none$error, response = rep("none", 3),
                          prop_good = err_none$prop_good)

#Perfect Climate Response
err_perf_g <- data.frame(error = err_perf$error, response = rep("ideal", 3),
                          prop_good = err_perf$prop_good)

#Precautionary Climate Response
err_proxy_g <- data.frame(error = err_proxy$error, response = rep("precautionary", 3),
                            prop_good = err_proxy$prop_good)

#No Climate Response & Perfect Climate Response
err_none_perf_g <- rbind(err_none_g, err_perf_g)

#No Climate Response & Precautionary Climate Response
err_none_proxy_g <- rbind(err_none_g, err_proxy_g)

#Perfect Climate Response & Precautionary Climate Response
err_perf_proxy_g <- rbind(err_perf_g, err_proxy_g)

#All Climate Responses
err_all_g <- rbind(err_none_g, err_perf_g, err_proxy_g)

######Assessment Interval

###Prop Tables

#No Climate Response
ai_intervals <- c(20,15,10,5,1) #new intervals in correct order

fishe_4[is.na(fishe_4)] <- "misc" #turn status NA into misc

ai_none <- fishe_4 %>% 
  mutate(assess_ints = factor(assess, levels = ai_intervals)) %>%
  filter(r_s < 0) %>% 
  group_by(assess_ints) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(simulations = sum(over + good + misc)) %>% 
  mutate(prop_good = good / simulations)

#Perfect Climate Response
ai_intervals <- c(20,15,10,5,1) #new intervals in correct order

fishe_5[is.na(fishe_5)] <- "misc" #turn status NA into misc

ai_perf <- fishe_5 %>% 
  mutate(assess_ints = factor(assess, levels = ai_intervals)) %>%
  filter(r_s < 0) %>% 
  group_by(assess_ints) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(simulations = sum(over + good + misc)) %>% 
  mutate(prop_good = good / simulations)

#Precautionary Climate Response
ai_intervals <- c(20,15,10,5,1) #new intervals in correct order

fishe_6[is.na(fishe_6)] <- "misc" #turn status NA into misc

# Calculate proportions for graphing:
ai_proxy <- fishe_6 %>% 
  mutate(assess_ints = factor(ai, levels = ai_intervals)) %>%
  filter(ai != 0) %>% 
  filter(r_s < 0) %>% 
  group_by(assess_ints) %>%
  count(status) %>% 
  spread(key = "status", value = "n") %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
  mutate(simulations = sum(over + good + misc)) %>% 
  mutate(prop_good = good / simulations)

###Graph-Ready Data

#No Climate Response
ai_none_g <- data.frame(assessment_interval = ai_none$assess_ints, response = rep("none", 5),
                         prop_good = ai_none$prop_good)

#Perfect Climate Response
ai_perf_g <- data.frame(assessment_interval = ai_perf$assess_ints, response = rep("ideal", 5),
                         prop_good = ai_perf$prop_good)

#Precautionary Climate Response
ai_proxy_g <- data.frame(assessment_interval = ai_proxy$assess_ints, response = rep("precautionary", 5),
                           prop_good = ai_proxy$prop_good)

#No Climate Response & Perfect Climate Response
ai_none_perf_g <- rbind(ai_none_g, ai_perf_g)

#No Climate Response & Precautionary Climate Response
ai_none_proxy_g <- rbind(ai_none_g, ai_proxy_g)

#Perfect Climate Response & Precautionary Climate Response
ai_perf_proxy_g <- rbind(ai_perf_g, ai_proxy_g)

#All Climate Responses
ai_all_g <- rbind(ai_none_g, ai_perf_g, ai_proxy_g)

##############Source Functions

source("sim_closure.R")
source("sim_fishery.R")

##############Begin App

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(h1("Evaluating Adaptive Management Strategies for Climate-Resilient Fisheries")),
  
  tabsetPanel(
    
    
    tabPanel("Introduction",
             br(),
             h2("Welcome!"),
             img(src='S.Fishie copy.png', align = "right", height = 300, width = 300),
             br(),
             p("This tool was developed to provide an interface for further exploration and visualization of the model and results produced by our research team for our Group Thesis Master's Project."),
             br(),
             h3("Results"),
             p("The 'Results' tab contains the results of more than 200,000 fishery simulations over a 100 year time frame, incorporating FISHE management decisions and climate change effects. To explore the results, select a management action to compare fishery outcomes across that given action. Then, select climate responses to view fishery outcomes under the selected responses."),
             br(),
             h3("Model"),
             p("The 'Model' tab contains the model developed by our team to evaluate fisheries over time using FISHE management in the face of climate change. To run the model, use the provided inputs to select parameter values. Click the 'Set Parameters' button to set the model parameters, and click 'Run Model' to run the model. Then, check the 'graph' box to view the simulation results. To re-run, follow the same steps, and un-check and re-check the 'graph' box."),
             br(),
             h3("Model Parameter Descriptions"),
             strong("Assessment Interval:"), 
             p("How often the fishery will be re-assessed and a new management decision implemented (ex. 20 = every 20 years)"),
             strong("Initial Biomass:"), 
             p("The biomass of the fishery in year 1 (carrying capacity = 10,000)"),
             strong("Growth Rate:"), 
             p("The intrinsic growth rate of the fish species"),
             strong("Climate Change Severity:"), 
             p("The true impact of climate change on the fish stock (ex. 0 = no climate impacts, -0.02 = severe climate impacts)"),
             strong("Precautionary Climate Assumption:"), 
             p("The assumped impact of climate change on the fish stock (ex. 0 = no climate impacts, -0.02 = severe climate impacts)"),
             strong("Error:"), 
             p("The level of sampling error in the stock assessment (ex. 0.5 = high error, 0.1 = low error)"),
             strong("Harvest Control Rule:"), 
             p("The reduction in fishing pressure should the fishery be performing below its target value (ex. 0.95 = 5% reduction in fishing pressure, 0.6 = 40% reduction in fishing pressure)"),
             br(),
             p("For more information about the project:"),
             tags$a(href="http://bren.ucsb.edu/research/documents/SomefinFISHEFinalReport.pdf", "Project Report"),
             br(),
             tags$a(href="http://github.com/saraorofino/FISHE", "Project Repository"),
             br(),
             tags$a(href="http://somefinfishe.com", "Project Website"),
             br(),
             tags$a(href="http://bren.ucsb.edu/research/documents/SomefinFISHEBrief.pdf", "Project Brief"),
             br(),
             tags$a(href="http://bren.ucsb.edu/research/images/SomefinFISHEPoster.png", "Project Poster"),
             br(),
             br(),
             p("Evaluating Adaptive Management Strategies for Climate-Resilient Fisheries is a Master's Thesis project with the Bren School of Environmental Science & Management on behalf of the Environmental Defense Fund (EDF)."),
             br(),
             p("Project Team: Chase Brewster, Nathaniel Burola, Sara Orofino, Gracie White"),
             hr()
             
    ),
    
    
    tabPanel("Results", 
             sidebarLayout(
              sidebarPanel(
                radioButtons(inputId = "metric",
                             label = h4("Management Action"),
                             choices = c("Harvest Control Rule" = "hcr", 
                                         "Sampling Error" = "err",
                                         "Assessment Interval" = "ai")),
                
                checkboxGroupInput(inputId = "climate",
                          label = h4("Climate Response"),
                          choices = c("No Response" = "none",
                                      "Perfect Response" = "perf",
                                      "Precautionary Response" = "proxy"))),
              
             mainPanel(plotOutput(outputId = "results"))
      )
    ),
    
    
    tabPanel("Model",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("ai",
                             label = h5("Assessment Interval"),
                             choices = c("20", "15", "10", "5", "1")),
                 
                 sliderInput("bio",
                             label = h5("Initial Biomass"),
                             min = 1000,
                             max = 9000,
                             value = 9000,
                             step = 500),
                 
                 sliderInput("growth",
                             label = h5("Growth Rate"),
                             min = 0.1,
                             max = 1.0,
                             value = 1.0,
                             step = 0.05),
                 
                 sliderInput("clim",
                             label = h5("Climate Change Severity"),
                             min = -0.02,
                             max = 0,
                             value = 0.0,
                             step = 0.001),
                 
                 sliderInput("clim_assump",
                             label = h5("Precautionary Climate Assumption"),
                             min = -0.02,
                             max = 0,
                             value = 0.0,
                             step = 0.001),
                 
                 sliderInput("error",
                             label = h5("Sampling Error"),
                             min = 0,
                             max = 0.5,
                             value = 0.5,
                             step = 0.05),
                 
                 sliderInput("hcr",
                             label = h5("Harvest Control Rule"),
                             min = 0.5,
                             max = 0.95,
                             value = 0.95,
                             step = 0.05),
                 
                 actionButton(inputId = "param",
                              label = h5("Set Parameters")),
                 
                 br(),
                 br(),
                 
                 actionButton(inputId = "run",
                              label = h5("Run Model")),
                 
                 br(),
                 br(),
                 
                 checkboxInput(inputId = "graph",
                              "Graph")),
                  
                 
               
            mainPanel(plotOutput("model"))
      )
    )
  )
)



server <- function(input, output){
# code for the trade-offs part
#==================================


##############Results Tab
  
######Graphs

###HCR

#No Climate Response
hcr_none <- ggplot(hcr_none_g, aes(x = hcr, y = prop_good)) +
    geom_col(alpha = 0.8, fill = "#BC6435") + 
    theme_light()+
    coord_cartesian( ylim=c(0,1), expand = FALSE ) +
    scale_x_reverse(labels=c("0.5" = "50%","0.6" = "40%" ,"0.7" = "30%","0.8" = "20%","0.9" = "10%","1.0" = "0%")) +
    labs(title = "Harvest Control Rule", x = "Reduction Amount (%)", y = "Proportion Healthy")+
    theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
          legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
          legend.box.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())

#Perfect Climate Response
hcr_perf <- ggplot(hcr_perf_g, aes(x = hcr, y = prop_good)) +
  geom_col(alpha = 0.8, fill = "#079EDF") + 
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  scale_x_reverse(labels=c("0.5" = "50%","0.6" = "40%" ,"0.7" = "30%","0.8" = "20%","0.9" = "10%","1.0" = "0%")) +
  labs(title = "Harvest Control Rule", x = "Reduction Amount (%)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Precautionary Climate Response
hcr_proxy <- ggplot(hcr_proxy_g, aes(x = hcr, y = prop_good)) +
  geom_col(alpha = 0.8, fill = "#B8CE55") + 
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  scale_x_reverse(labels=c("0.5" = "50%","0.6" = "40%" ,"0.7" = "30%","0.8" = "20%","0.9" = "10%","1.0" = "0%")) +
  labs(title = "Harvest Control Rule", x = "Reduction Amount (%)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#No Climate Response & Perfect Climate Response
hcr_none_perf <- ggplot(hcr_none_perf_g, aes(x = hcr, y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#079EDF"), name = "Climate Response", labels = c("None", "Perfect")) +
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  scale_x_reverse(labels=c("0.5" = "50%","0.6" = "40%" ,"0.7" = "30%","0.8" = "20%","0.9" = "10%","1.0" = "0%")) +
  labs(title = "Harvest Control Rule", x = "Reduction Amount (%)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#No Climate Response & Precautionary Climate Response
hcr_none_proxy <- ggplot(hcr_none_proxy_g, aes(x = hcr, y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#B8CE55"), name = "Climate Response", labels = c("None", "Precautionary")) +
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  scale_x_reverse(labels=c("0.5" = "50%","0.6" = "40%" ,"0.7" = "30%","0.8" = "20%","0.9" = "10%","1.0" = "0%")) +
  labs(title = "Fishing Reduction", x = "Reduction Amount (%)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Perfect Climate Response & Precautionary Climate Response
hcr_perf_proxy <- ggplot(hcr_perf_proxy_g, aes(x = hcr, y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#079EDF", "#B8CE55"), name = "Climate Response", labels = c("Perfect", "Precautionary")) +
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  scale_x_reverse(labels=c("0.5" = "50%","0.6" = "40%" ,"0.7" = "30%","0.8" = "20%","0.9" = "10%","1.0" = "0%")) +
  labs(title = "Harvest Control Rule", x = "Reduction Amount (%)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#All Climate Responses
hcr_all <- ggplot(hcr_all_g, aes(x = hcr, y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#079EDF", "#B8CE55"), name = "Climate Response", labels = c("None", "Perfect", "Precautionary")) +
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  scale_x_reverse(labels=c("0.5" = "50%","0.6" = "40%" ,"0.7" = "30%","0.8" = "20%","0.9" = "10%","1.0" = "0%")) +
  labs(title = "Harvest Control Rule", x = "Reduction Amount (%)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

###Error

#No Climate Response
err_none <- ggplot(err_none_g, aes(x = fct_rev(error), y = prop_good)) +
  geom_col(alpha = 0.8, fill = "#BC6435") + 
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Sampling Error", x = "Accuracy of Estimates", y = "Proportion Healthy") +
  scale_x_discrete(labels=c("0.5" = "Low", "0.3" = "Moderate","0.1" = "High")) +
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Perfect Climate Response
err_perf <- ggplot(err_perf_g, aes(x = fct_rev(error), y = prop_good)) +
  geom_col(alpha = 0.8, fill = "#079EDF") + 
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Sampling Error", x = "Accuracy of Estimates", y = "Proportion Healthy") +
  scale_x_discrete(labels=c("0.5" = "Low", "0.3" = "Moderate","0.1" = "High")) +
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Precautionary Climate Response
err_proxy <- ggplot(err_proxy_g, aes(x = fct_rev(error), y = prop_good)) +
  geom_col(alpha = 0.8, fill = "#B8CE55") + 
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Sampling Error", x = "Accuracy of Estimates", y = "Proportion Healthy") +
  scale_x_discrete(labels=c("0.5" = "Low", "0.3" = "Moderate","0.1" = "High")) +
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#No Climate Response & Perfect Climate Response
err_none_perf <- ggplot(err_none_perf_g, aes(x = fct_rev(error), y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#079EDF"), name = "Climate Response", labels = c("None", "Perfect"))+
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Sampling Error", x = "Accuracy of Estimates", y = "Proportion Healthy") +
  scale_x_discrete(labels=c("0.5" = "Low", "0.3" = "Moderate",
                            "0.1" = "High")) +
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#No Climate Response & Precautionary Climate Response
err_none_proxy <- ggplot(err_none_proxy_g, aes(x = fct_rev(error), y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#B8CE55"), name = "Climate Response", labels = c("None", "Precautionary"))+
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Sampling Error", x = "Accuracy of Estimates", y = "Proportion Healthy") +
  scale_x_discrete(labels=c("0.5" = "Low", "0.3" = "Moderate",
                            "0.1" = "High")) +
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Perfect Climate Response & Precautionary Climate Response
err_perf_proxy <- ggplot(err_perf_proxy_g, aes(x = fct_rev(error), y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#079EDF", "#B8CE55"), name = "Climate Response", labels = c("Perfect", "Precautionary"))+
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Sampling Error", x = "Accuracy of Estimates", y = "Proportion Healthy") +
  scale_x_discrete(labels=c("0.5" = "Low", "0.3" = "Moderate",
                            "0.1" = "High")) +
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#All Climate Responses
err_all <- ggplot(err_all_g, aes(x = fct_rev(error), y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#079EDF", "#B8CE55"), name = "Climate Response", labels = c("None", "Perfect", "Precautionary"))+
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Sampling Error", x = "Accuracy of Estimates", y = "Proportion Healthy") +
  scale_x_discrete(labels=c("0.5" = "Low", "0.3" = "Moderate",
                            "0.1" = "High")) +
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

###Assessment Interval

#No Climate Response
ai_none <- ggplot(ai_none_g, aes(x = assessment_interval, y = prop_good)) +
  geom_col(alpha = 0.8, fill = "#BC6435") + 
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Assessment Intervals", x = "Frequency of Assessment (Years)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Perfect Climate Response
ai_perf <- ggplot(ai_perf_g, aes(x = assessment_interval, y = prop_good)) +
  geom_col(alpha = 0.8, fill = "#079EDF") + 
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Assessment Intervals", x = "Frequency of Assessment (Years)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Precautionary Climate Response
ai_proxy <- ggplot(ai_proxy_g, aes(x = assessment_interval, y = prop_good)) +
  geom_col(alpha = 0.8, fill = "#B8CE55") + 
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Assessment Intervals", x = "Frequency of Assessment (Years)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#No Climate Response & Perfect Climate Response
ai_none_perf <- ggplot(ai_none_perf_g, aes(x = assessment_interval, y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#079EDF"), name = "Climate Response", labels = c("None", "Perfect"))+
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Assessment Intervals", x = "Frequency of Assessment (Years)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#No Climate Response & Precautionary Climate Response
ai_none_proxy <- ggplot(ai_none_proxy_g, aes(x = assessment_interval, y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#B8CE55"), name = "Climate Response", labels = c("None", "Precautionary"))+
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Assessment Intervals", x = "Frequency of Assessment (Years)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Perfect Climate Response & Precautionary Climate Response
ai_perf_proxy <- ggplot(ai_perf_proxy_g, aes(x = assessment_interval, y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#079EDF", "#B8CE55"), name = "Climate Response", labels = c("Perfect", "Precautionary"))+
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Assessment Intervals", x = "Frequency of Assessment (Years)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#All Climate Responses
ai_all <- ggplot(ai_all_g, aes(x = assessment_interval, y = prop_good, fill = response)) +
  geom_col(position = "dodge", alpha = 0.8) + 
  scale_fill_manual(values = c("#BC6435", "#079EDF", "#B8CE55"), name = "Climate Response", labels = c("None", "Perfect", "Precautionary"))+
  theme_light()+
  coord_cartesian( ylim=c(0,1), expand = FALSE ) +
  labs(title = "Assessment Intervals", x = "Frequency of Assessment (Years)", y = "Proportion Healthy")+
  theme(axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 20), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
        legend.text=element_text(size=12),legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.background = element_blank(),
        legend.box.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) 

######Outputs

  output$results <- renderPlot({
    
    if(req(input$metric) == "hcr" & req(input$climate) == "none"){print(hcr_none)}
    if(req(input$metric) == "hcr" & req(input$climate) == "perf"){print(hcr_perf)}
    if(req(input$metric) == "hcr" & req(input$climate) == "proxy"){print(hcr_proxy)}
    if(req(input$metric) == "hcr" & (all(c("none", "perf") %in% input$climate))){print(hcr_none_perf)}
    if(req(input$metric) == "hcr" & (all(c("none", "proxy") %in% input$climate))){print(hcr_none_proxy)}
    if(req(input$metric) == "hcr" & (all(c("perf", "proxy") %in% input$climate))){print(hcr_perf_proxy)}
    if(req(input$metric) == "hcr" & (all(c("none", "perf", "proxy") %in% input$climate))){print(hcr_all)}
    
    if(req(input$metric) == "err" & req(input$climate) == "none"){print(err_none)}
    if(req(input$metric) == "err" & req(input$climate) == "perf"){print(err_perf)}
    if(req(input$metric) == "err" & req(input$climate) == "proxy"){print(err_proxy)}
    if(req(input$metric) == "err" & (all(c("none", "perf") %in% input$climate))){print(err_none_perf)}
    if(req(input$metric) == "err" & (all(c("none", "proxy") %in% input$climate))){print(err_none_proxy)}
    if(req(input$metric) == "err" & (all(c("perf", "proxy") %in% input$climate))){print(err_perf_proxy)}
    if(req(input$metric) == "err" & (all(c("none", "perf", "proxy") %in% input$climate))){print(err_all)}
    
    if(req(input$metric) == "ai" & req(input$climate) == "none"){print(ai_none)}
    if(req(input$metric) == "ai" & req(input$climate) == "perf"){print(ai_perf)}
    if(req(input$metric) == "ai" & req(input$climate) == "proxy"){print(ai_proxy)}
    if(req(input$metric) == "ai" & (all(c("none", "perf") %in% input$climate))){print(ai_none_perf)}
    if(req(input$metric) == "ai" & (all(c("none", "proxy") %in% input$climate))){print(ai_none_proxy)}
    if(req(input$metric) == "ai" & (all(c("perf", "proxy") %in% input$climate))){print(ai_perf_proxy)}
    if(req(input$metric) == "ai" & (all(c("none", "perf", "proxy") %in% input$climate))){print(ai_all)}
    

  })
  
##############Model Tab
  
######Functions

observeEvent(input$param, {
  bio <<- as.numeric(input$bio)
})

observeEvent(input$param, {
  growth <<- as.numeric(input$growth)
})

observeEvent(input$param, {
  clim <<- as.numeric(input$clim)
})

observeEvent(input$param, {
  clim_assump <<- as.numeric(input$clim_assump)
})

observeEvent(input$param, {
  error <<- as.numeric(input$error)
})

observeEvent(input$param, {
  hcr <<- as.numeric(input$hcr)
})

observeEvent(input$param, {
  ai <<- as.numeric(input$ai)
})

observeEvent(input$run, {
  assess <<- seq(ai, 100, ai)
  sim_close <<- sim_closure(b = bio, r = growth, r_s = clim, r_p_s = clim_assump, error = error, hcr = hcr) %>% 
    mutate(id = rep("1", 100))
  sim_noclose <<- sim_fishery(b = bio, r = growth, r_s = clim, r_p_s = clim_assump, error = error, hcr = hcr) %>% 
    mutate(id = rep("2", 100))
  sim_both <<- rbind(sim_close, sim_noclose)
})


######Outputs

output$model <- renderPlot({
  if(req(input$graph) == TRUE){
    both_graph <- ggplot(sim_both, aes(x = year, y = b, group = id)) +
      geom_line(aes(color = id), size = 1) +
      scale_color_manual(values = c("#079EDF", "#B8CE55"), name = "Model Type", labels = c("Closures", "No Closures"))+
      theme_light() +
      coord_cartesian( ylim=c(0,10000), expand = FALSE ) +
      labs(title = "FISHE Mangement", x = "Year", y = "Biomass")+
      theme(legend.key = element_rect(fill = "transparent", colour = "transparent"), axis.text.x=element_text(size=15), axis.text.y=element_text(size=15), plot.title = element_text(hjust = 0.5, face = "bold", size = 15), axis.title.x = element_text(face = "bold", size = 15), axis.title.y = element_text(face = "bold", size = 15), legend.title.align=0.5, panel.background = element_rect(fill = "transparent",colour = NA), plot.background = element_rect(fill = "transparent",colour = NA), legend.title=element_text(size=15), 
            legend.text=element_text(size=12), legend.background = element_blank(),
            legend.box.background = element_blank())
   
    print(both_graph)}
  })
}

shinyApp(ui = ui, server = server)