#Names: Nathaniel Burola, Siya Qui, Mauricio Collado 
#Subject: Somefin' FISHE Shiny App 
#Date: 4/30/2020

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
library(dplyr)
library(here)
library(janitor)
library(plotly)
library(ggplot2)
library(ggmap)
library(maptools)
library(maps)
library(lubridate)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Reading in the new data about mapping vulnerabilites 
load("raw_data/cru_1901_2013_tmp_coarser")

# read in data
fishe <- read_csv(here::here("raw_data", "alldata_combined.csv"))
fishe_clean <- fishe %>% 
  drop_na() %>% 
  mutate(status = factor(status, levels = c("closed", "over", "good"), 
                         labels = c("Closed", "Overfished", "Good"))) %>% 
  mutate(error = as.factor(error)) %>% 
  mutate(hcr = as.factor(hcr))

# reading new biomass data
fishe_b <- read_csv(here::here("raw_data", "shiny_biomass.csv")) %>% 
  rename(biomass= b, catch=c) %>% 
  group_by(year) %>% 
  mutate(hcr_select = 1 - hcr) %>% 
  drop_na() 

# create unique dataset list

lst.b_0  <- unique(fishe_b$b_0) # initial biomass list
lst.r_0  <- unique(fishe_b$r_0) # growth rate list
lst.hcr_select  <- unique(sort(fishe_b$hcr_select)) # hcr list
lst.assess  <- unique(sort(fishe_b$assess)) # assessment interval list
lst.error  <- unique(sort(fishe_b$error)) # error reduction list

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(h1("Evaluating Adaptive Management Strategies for Climate Resilient Fisheries")),
  tags$head(tags$style(
    HTML('#title {
    color: black; 
    font-size: 40px; 
    font-style: bold; 
    } '))),
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
    .tabbable > .nav > li > a[data-value='t1'] {background-color: aqua;   color:black}
    .tabbable > .nav > li > a[data-value='t2'] {background-color: aqua;  color:black}
    .tabbable > .nav > li > a[data-value='t3'] {background-color: aqua; color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: aqua; color:black}
  ")),
  tabsetPanel(
    tabPanel("Introduction",
             strong(h3("About the Project (Background)", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica")),
             em(strong(h4("What is the Enviromental Defense Fund (EDF) and what are they doing with data-limited fisheries management?", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))), 
             tags$img(src = "EDF.png", height = "100px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
             br(),
             p("EDF is a nonprofit environmental advocacy group known for working on issues such as global warming, ecosystem restoration and oceans. Despite the immense value of fisheries, the majority of global fisheries lack the appropriate data to utilizze conventional scientific stock assessment methods.",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(),
             hr(),
             
             em(strong(h4("What is the Framework for Integrated Stock and Habitat Evaluation? (FISHE)", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))),
             tags$img(src = "FISHE.png", height = "300px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
             p("FISHE is an 11 step adaptive management framework designed by EDF to assist with the management of data-limited fisheries. The purpose of the framework is to assist fisheries managers conduct simplified stock assessments and evaluate potential management options with limited inputs.",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
             br(),
             p("While FISHE was designed to capture the inherently dynamic nature of fisheries, it was not designed to address the expected environmental changes stemming from climate change. As climate change has already affected fish stocks worlwdide, it is imperative that FISHE be robut to climate-induced variations.", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(), 
             hr(),
             
             em(strong(h4("Which parts of FISHE are most likely going to be affected by climate change?", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))),
             tags$img(src = "vulnerable.PNG", height = "300px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
             p("Since FISHE has multiple steps, it is important to identify the aspects of the framework that were most vulnerable to climate change:", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(), 
             p("Step 7: Reference Points: theoretical values that allow you to determine the status of the fishery. If you are close to your fishing target, then you could be underfishing. However, if you are close to your limit,  there could be a case of severe overfishing.", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
             br(), 
             p("Step 8: Harvest Control Rules (HCR): simple rules that direct the action to be taken from a resulting target. It could be simple enough such as fish more, reduce by x amount of percentage, or even stop fishing altogether.", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
             br(), 
             p("Step 9: Detailed Assessments: detailed assessments are needed to estimate actual fishery values. Estimated sampling error is a potential factor which may affect results with Tier 1 sampling error representing 10%, Tier 2 representing 30%, and Tier 3 representing 50%", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
             br(), 
             p("Step 11: Implementation and Adaptation: implementation involves choosing Harvest Control Measures (HCM) which describe how the HCR of Step 8 will be implemented. Adaptation involves the process of re-visiting fishery goals and re-evaluating the state of the fishery.", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(),
             hr(),
             
             strong(h3("How was the data created?", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica")),
             em(strong(h4("Was the data collected or was it all simulated by model runs?", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))),
             tags$img(src = "parameters.png", height = "300px", width = "500px", style="display: block; margin-left: auto; margin-right: auto"),
             p("With the baseline model, the following terms were used to represent variables such as growth (r), carrying capacity (k), inital biomass (b0), shape parameter (p), and time. Adding climate into the baseline model was accomplished by using a change in growth (Δr) and a change in carrying capacity (Δk). To simulate management decisions, fishing pressure, harvest rules, assessment intervals, and sampling error was added to the model. All these variables combined allowed 100,000 + simulations to be computed with the model.", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(),
             
             em(strong(h4("What type of model are you running (generic that can be applied to all vs specific that can only be applied to a few?)",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))),
             p("It is important to note that this model is a generic model that can be adapated to a wide range of situations. This is not meant to be applied to specific case studies as every fishery is in a different state prior to being assessed.", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             hr(),
             
             strong(h3("Variable Glossary (defined variables)", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica")),
             em(strong(h4("What are the variables that are going to be defined and utilized in this Shiny App?", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))),
             br(),
             p("- Growth rates (slow, medium, and fast)", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(),
             p("- Harvest Control Rules (HCRs)", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(),
             p("- Error reduction", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(),
             p("- Assessment intervals", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             hr(),
             
    ),
             
             tabPanel("FISHE Framework", 
            strong(h3("About FISHE Framework (What is the FISHE Framework?)", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica")),
             em(strong(h4("What is the Frameworkfor Intergrated Stock and Habitat Evaluation (FISHE)", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))),
             tags$img(src = "FISHE.png", height = "300px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
            br(),
             p("EDF designed FISHE to assist with the management of data-limited fisheries. FISHE is an 11 step adaptive management framework designed to help fisheires managers conduct simplified stock assessments and evaluate potential management options with limited inputs. FISHE can be used and adapted for any type of stock in any geographic location. FISHE is being used to guide management reforms around the world, including Baja California, the Philippines, and Belize.", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
             br(), 
             hr(),
             
             em(strong(h4("What 11 steps are there in this circular process?", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))),
            br(),
             p("FISHE is composed of the following steps:", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(),
             p("Step 1: Goal Setting", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
            br(), 
             p("Step 2: Ecosystem Assessment", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
            br(),
             p("Step 3: Vulnerability Assessment", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
            br(), 
             p("Step 4: Initial Stock Assessment", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
            br(),
             p("Step 5: Prioritization", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
            br(),
             p("Step 6: Performance Indicators", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
            br(),
             p("Step 7: Reference Points",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
            br(),
             p("Step 8: Harvest Control Rules (HCR)",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
            br(),
             p("Step 9: Detailed Assessments",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
            br(),
             p("Step 10: Interpretation",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
            br(),
             p("Step 11: Implementation and Adaptation", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(), 
             p("FISHE is an inclusive process which considers all stakeholders that are involved. Steps 1-8 of the framework are completed before any data is analyzed or assessed. Steps 9-11 involves the actual stock assessment after the management framework is set.", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
             br(), 
            hr(),
             
             em(strong(h4("How is FISHE utilized throughout the world?", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))),
            br(),
             p("As an adaptable management framework, FISHE has been developed to be applicable to fisheries in any type of system. Data collected through the 11 step framework will be equally applicable to these methods. FISHE is utilized in several countries such as Cuba to protect the near endangered lane snapper (Lutjanus synagris), Indonesia to protect the blue swimming crab (Portunus armatus), and other countries including Myanmar, Belize, and Mexico.",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
            hr(),
             
    ),
    tabPanel("Mapping Vulnerabilities", 
             dashboardBody( 
               tags$h3('Climate Change and Land Temperature (1901-2013)', style = "color:steelblue; font-family:Helvetica"),
               p("The average global temperature has increased by 0.9°C (1.5°F) compared to the baseline temperature of 14°C for the period of 1951-1980. Even if it has risen, the magnitude of the temperature increase varies from region to region. Data below comes from the Climatic Research Unit and maps the previous 112 years of global temperature from 1901-2013. Sea surface temperatures are not included in the following dataset considering this is only land temperatures. In addition to showing off land temperatures, the average annual land temperature is also included. (Source: Climatic Research Unit (CRU). “Global Temperatures from 1901 - 2013.” 2014.)", style = "color:white; font-family:Helvetica"),
               br(),
               
               fluidRow(column(width = 7,
                               plotOutput("distPlot",dblclick='plot_dblclick', click = "plot_click")
               ),
               
               column(width = 7,
                      plotOutput('dbclick'))
               )
               
             )),
    tabPanel("Comparing Trade-Offs", 
             tags$h3("Comparing Trade-Offs", style = "color:steelblue; font-family:Helvetica"),
             p("Comparing the proportion of bad outcomes in the form of stacked bar graphs showing how outcomes vary over time by different factors (i.e. Growing Rates, Error Reduction, Harvest Control Rule, etc)", style = "color:white; font-family:Helvetica", hr()),
             radioButtons(inputId = "choice",
                          label = "Checkbox Group",
                          choices = c("Growth rates" = "growth",
                                      "Error reduction" = "error",
                                      "Proportion closed/overfished" = "status",
                                      "Harvest Control Rule (HCR)" = "hcr")),
             mainPanel(plotOutput(outputId = "trade_off"))),
    tabPanel("Biomass Over Time",
             tags$h3("Estimating the biomass", style = "color:steelblue; font-family:Helvetica"),
             p("Select the parameter values to estimate the biomass for the next 100 years. The outcomes will vary according to growing rate, error reduction, assessment interval, starting biomass and harvest control rule.", style = "color:white; font-family:Helvetica",hr()
               ),
             tags$style(type='text/css', 
                        ".selectize-input { font-size: 20px; line-height: 20px;} .selectize-dropdown { font-size: 14px; line-height: 14px; }"),
               sidebarPanel(
                 
             selectInput("select_b_o", 
                         h3("Select biomass start"), 
                         choices = lst.b_0, 
                         selected = 1),
             
             selectInput("select_r_o", 
                           h3("Select growth rate"), 
                           choices = lst.r_0, 
                           selected = 1),
            
              selectInput("select_hcr", 
                         h3("Select harvest control"), 
                         choices = lst.hcr_select, 
                         selected = 0.5),
             
             selectInput("select_assess", 
                         h3("Select assessed years"), 
                         choices = lst.assess, 
                         selected = 0),
            
              selectInput("select_error", 
                         h3("Select error reduction"), 
                         choices = lst.error, 
                         selected = 0)),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          fluidRow(
                            plotOutput(outputId = "biomass_g")),
                          tabPanel("Biomass dataset", downloadButton("downloadData", "Download series"))
                          
                 ))
                       ),

    ))
)



server <- function(input, output){
# code for the trade-offs part
#==================================
#
    tradeOff_growth <- ggplot(fishe_clean, aes(x = assess, fill = growth))+
    geom_histogram(binwidth = 2.5)+
    labs(x = "Frequency of Assessment (Years)",
         y = "Frequency of Growth Rate",
         title = "Assessments Intervels") +
    theme(legend.title = element_text(color = "blue", size = 10, face = "bold"),
          legend.background = element_rect(fill = "lightblue", size = 0.5, linetype = "solid")) + 
    theme_minimal()
  
  tradeOff_error <- ggplot(fishe_clean, aes(x = assess, fill = error))+
    geom_histogram(binwidth = 2.5)+
    labs(x = "Frequency of Assessment (Years)",
         y = "Frequency of Error Reduction",
         title = "Assessments Intervels") +
    theme(legend.title = element_text(color = "blue", size = 10, face = "bold"),
          legend.background = element_rect(fill = "lightblue", size = 0.5, linetype = "solid")) + 
    theme_minimal()
  
  tradeOff_status <- ggplot(fishe_clean, aes(x = assess, fill = status))+
    geom_histogram(binwidth = 2.5)+
    labs(x = "Frequency of Assessment (Years)",
         y = "Frequency of Status",
         title = "Assessments Intervels") +
    theme(legend.title = element_text(color = "blue", size = 10, face = "bold"),
          legend.background = element_rect(fill = "lightblue", size = 0.5, linetype = "solid")) + 
    theme_minimal()
  
  tradeOff_hcr <- ggplot(fishe_clean, aes(x = assess, fill = hcr))+
    geom_histogram(binwidth = 2.5)+
    labs(x = "Frequency of Assessment (Years)",
         y = "Frequency of Harvest Control Rule (HCR)",
         title = "Assessments Intervels") +
    theme(legend.title = element_text(color = "blue", size = 10, face = "bold"),
          legend.background = element_rect(fill = "lightblue", size = 0.5, linetype = "solid")) + 
    theme_minimal()

  
  output$trade_off <- renderPlot({
    if (req(input$choice) == "growth"){print(tradeOff_growth)}
    else if (req(input$choice) == "error"){print(tradeOff_error)}
    else if (req(input$choice) == "status"){print(tradeOff_status)}
    else{print(tradeOff_hcr)}
  })
  
  output$distPlot <- renderPlot({
    mapWorld <- borders("world", colour="gray50", fill="#cce6ff") # create a layer of borders
    p=ggplot() +   mapWorld
    p=p+ggtitle("Double click on any country in any \n continent to observe land temperatures since 1901 until 2013")+
      theme(axis.text.y   = element_blank(),
            line = element_blank(),
            axis.text.x   = element_blank(),
            axis.title.y  = element_blank(),
            axis.title.x  = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust=0.5,size = 18,colour="#663300"),
            panel.border = element_rect(colour = "gray70", fill=NA, size=1))
    
    
    p
    
  })
  
  
  double_clicked <- reactiveValues(
    
    center = NULL 
  )
  
  observeEvent(input$plot_dblclick, {
    
    double_clicked$center <- c(input$plot_dblclick$x,input$plot_dblclick$y)
    
  })
  
  
  lon_index<-reactive({
    z=double_clicked$center
    which((abs(lon-z[1]))<=1)[1]
    
  })
  
  
  lat_index<-reactive({
    z=double_clicked$center
    which((abs(lat-z[2]))<=1)[1]
    
  })
  
  output$dbclick<-renderPlot({
    
    if(!is.na(lon_index())){
      
      z =tmp[lon_index(),lat_index(),]
      
      monthly=matrix(z,nrow=12)
      
      if(length(na.omit(monthly))>0){
        
        annual_min=apply(monthly,2,min,na.omit=TRUE)
        annual_max=apply(monthly,2,max,na.omit=TRUE)
        annual_ave=apply(monthly,2,mean,na.omit=TRUE)
        
        
        annual=as.data.frame(list(annual_ave=annual_ave,years=unique(year(time))))
        
        q=ggplot(annual,aes(x=years,y=annual_ave))+geom_line()+ ylab(expression("Temperature "*~degree*C))+theme_bw()
        q=q+ggtitle(paste0("Annual Average Temperature Trend at (",lon[lon_index()],", ", lat[lat_index()],")"))+xlab('')+theme(axis.title.y = element_text(size=14,angle=90,hjust=.5,vjust=1),
                                                                                                                                axis.text.y = element_text(colour="darkred",size=16,angle=0,hjust=1,vjust=0),plot.title = element_text(vjust=1.5,size = 15,colour="blue"),
                                                                                                                                axis.text.x = element_text(colour="darkred",size=16,angle=0,hjust=1,vjust=0))+stat_smooth(method='lm',color='darkred')
        
        
        q
        
      }
      
    }
    
  })

  
  # Handle dounle clicks on the plot
  
# code for the biomass part
#==================================
#

  # first reactive dataframe
  fishe_react<-reactive({
    #dataframe creation
    fishe_b %>%
      filter(r_0 == input$select_r_o,
             b_0 == input$select_b_o,
             error == input$select_error,
             assess == input$select_assess,
             hcr_select == input$select_hcr
             ) %>% 
        summarize(biomass=mean(biomass),
                  catch=mean(catch))
  })

  # second reactive data frame
 
  output$biomass_g <- renderPlot({
    ggplot() +
      geom_line(data = fishe_react(), aes(x = year, y = biomass), color = "steelblue", size = 1) +
      #geom_line(data = fishe_react(), aes(x = year, y = catch), color = "red", size = 1) +
      labs(x = "Time (Years)",
           y = "Biomass and catch (tons)",
           title = "Biomass evolution in the next 100 years")+
      theme_minimal()
  })
  
  output$table <- renderTable({
    fishe_react()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("fishe", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(fishe_react(), file, row.names = FALSE)
    }
  )
  

}

shinyApp(ui = ui, server = server)