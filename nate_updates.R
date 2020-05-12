#App code simplified by Nate

#Names: Nathaniel Burola, Chase Brewster, Gracie White, and Sara Orofino
#Subject: Somefin' FISHE Shiny App 
#Date: 5/6/2020

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
             em(strong(h4("What is this project about?",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"))), 
             tags$img(src = "EDF.png", height = "100px", width = "300px", style="display: block; margin-left: auto; margin-right: auto"),
             br(),
             p("This is an interactive web application called a Shiny App designed to communicate the main takeaways of the Somefin' FISHE Project. Our client, the Environmental Defense Fund (EDF) can use this project in order to help fishery managers make recommendations with their fish stocks in relation to climate change.",style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             p("To find out more information about the Somefin' FISHE project brief, copy and paste this URL, http://bren.ucsb.edu/research/documents/SomefinFISHEBrief.pdf", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
             p("To find out more information about the Somefin' FISHE project poster, copy and paste this URL, http://bren.ucsb.edu/research/images/SomefinFISHEPoster.png", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"), 
             p("To find out more information about the Somefin' FISHE project final report, copy and paste this URL, http://bren.ucsb.edu/research/documents/SomefinFISHEFinalReport.pdf", style="text-align:justify;color:white;background-color:navy;padding:15px;border-radius:10px;font-family:Helvetica"),
             br(),
             hr(),
             
    ),
    
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