#helper script for data
source("global.R")

library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(magrittr)
library(dplyr)
library(ggplot2)



#options(device='cairo')


#sidebar content 
sidebar <-  dashboardSidebar(sidebarMenu(
  menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
  menuItem("All States", tabName = "All_States"),
  menuItem("Compare States", tabName = "Compare_States"),
  menuItem("All Data", tabName = "All_Data")
 
  
)
)


#body content for each 
body <- dashboardBody(tabItems(
  #Into Tab
  tabItem(tabName = "Intro",
          fluidRow(
            # A static valueBox
            valueBox(count(gun_violence_total_2017), "Incidents", icon = icon("gun")),
            valueBox(sum(gun_violence_total_2017$n_killed), "Total Deaths", icon = icon("gun")),
            valueBox(sum(gun_violence_total_2017$n_injured), "Total Wounded", icon = icon("gun"))
            
            
          ),
          fluidRow(
            box(
              width = 12, solidHeader = TRUE, status = "primary",
              h3("The data in this dashboard captures all incedents of American gun violence that took place in 2017.You can explore a summary of all  incidents  by Total or PerCapita statistics. You may also show the politial affiliation of each state from the 2016 Presidential Election."),
              h3("Three separate datasets were combined to give all of the informaion. All Gun Violence data is a subset of the Kaggle Gun Violence 
                 dataset. Population numbers and voting data were both retrieved from data.gov "),
              h3("Download them all here:",
                 tags$a(href="https://github.com/spcarey/Project2/tree/master/data", "DATA"))
             
              
            )
          )
 ),
  #end Into Tab
  #All States
  tabItem(tabName = "All_States",
          fluidRow(
           box(
             width = 9 ,solidHeader = TRUE,
            collapsible = TRUE,
            DTOutput("table1")
           ),
          
           box(
            title = "Inputs", width = 3 ,solidHeader = FALSE,
            selectInput("Deaths_Wouded_Select", label = h3("Deaths or Wounded"), 
                        choices = list("Deaths"="Deaths", "Wounded"="Wounded"), 
                        selected = 1),
            sliderInput("population_slider", label = h3("Population Range"), min = 500000, 
                        max = 40000000, value = c(0, 40000000)),
            checkboxInput("PerCapita","PerCapita", value = FALSE),
            checkboxInput("PartyBox","Show 2016 Election Party", value = FALSE),
            downloadButton("downloadData", "Download Data"),downloadButton("exportPlot1", "Download Plot")
           )
          ),
          fluidRow(
            box(
               width = 12, solidHeader = TRUE, status = "primary",
               plotOutput("Plot1")
            )
  )),
  #end All States Tab
  #Compare States
  tabItem(tabName = "Compare_States",
          fluidRow(
            box(
              width = 4,solidHeader = TRUE,
              collapsible = TRUE,
              tableOutput("table2")
            ),
            box(
              width = 4 ,solidHeader = TRUE,
              collapsible = TRUE,
              tableOutput("table3")
            ),
            
            box(
              title = "Inputs", width = 4 ,solidHeader = FALSE,
              selectInput("SelectState", label = h3("State: 1"), 
                          choices = state_population_2017$state, 
                          selected = 1),
              selectInput("SelectState2", label = h3("State: 2", style = "color:blue;"), 
                          choices = state_population_2017$state, 
                          selected = "California"),
              selectInput("Deaths_Wouded_Select2", label = h3("Deaths or Wounded"), 
                          choices = list("Deaths"="Deaths", "Wounded"="Wounded"), 
                          selected = 1),
              downloadButton("downloadData2", "Download Data"),downloadButton("exportPlot2", "Download Plot")
            )
          ),
          fluidRow(
            box(
              width = 12, solidHeader = TRUE, status = "primary",
              plotOutput("Plot2")
            )
          )
  ),  

  #end All States Tab
  #ALL DATA
  tabItem(tabName = "All_Data",
          fluidRow(
            box(
              width = 12, solidHeader = TRUE,
              DTOutput("table4"),
              downloadButton("downloadData3", "Download Data")
            )
          )
  )
  #End ALL DATA
  #Supervised Model
 
 
  
))



# dasboard page set up
dashboardPage(
  dashboardHeader(title = "Gun Violence in 2017"),
  sidebar,
  body  
)