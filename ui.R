##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Copyright 2018, Ramanathan Perumal, All rights reserved.
## ramamet4@gmail.com
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(RColorBrewer)
library(DT)

require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(readr)

#####
## country
loc <- read.csv("country.csv")
## year
#yrr <- read.csv("YEAR.csv")

##
group <- read.csv("group.csv")

##---------------------

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Global Commodity Trade")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
     
      ## year select
      selectInput("variable", "Year:",
                  c("2016" = "2016",
                    "2015" = "2015",
                   "2014" = "2014"),
                   selected = "2016"),
#      selectInput("variable", "Year:", 
#                     choices = unique(yrr$y1)),              
        submitButton("update",icon("refresh")), 

       ## category select            
        selectInput("var3", "Category:", 
                    choices = unique(group$grp)),
                    submitButton("filter",icon("filter")),
                    
         ## country select
        selectInput("var2", "Place:", 
                    choices = unique(loc$area)),
                    submitButton("refresh",icon("map-marker")),
                    
    
   # div(style="display:inline-block;width:50%;text-align: center;",submitButton("update1", label = "update", icon = icon("refresh"))),             
    menuItem("Dashboard1", tabName = "dashboard1", icon = icon("object-align-left",lib='glyphicon')),
    menuItem("Dashboard2", tabName = "dashboard2", icon = icon("align-left",lib='glyphicon')),
    menuItem("Dashboard3", tabName = "dashboard3", icon = icon("dashboard",lib='glyphicon')),
    menuItem("DataTable", tabName = "dataTable1", icon = icon("dashboard",lib='glyphicon')),
     menuItem("Contact", tabName = "dashboard4", icon = icon("earphone",lib='glyphicon')),
    menuItem("Data-source", icon = icon("send",lib='glyphicon'), 
             href = "https://www.kaggle.com/unitednations/global-commodity-trade-statistics")
      
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  
  box(
    title = "Export"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyPrd", height = "300px")
    
  )
  
  ,box(
    title = "Import"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyRegion", height = "300px")
  ) 
  
)

### Row3

frow3 <- fluidRow(
   box(
    title = "Category: Export"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("catSpec1", height = "300px")
  )
  
   ,box(
     title = "Category: Import"
     ,status = "primary"
     ,solidHeader = TRUE 
     ,collapsible = TRUE 
     ,plotOutput("catSpec2", height = "300px")
   ) 
  
)

## Row4
frow4 <- fluidRow(
   box(
    title = "Country: Export"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("countrySpec1", height = "300px")
  )
  
   ,box(
    title = "Country: Import"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("countrySpec2", height = "300px")
  ) 
  
)


### Row5
frow5 <- fluidRow(
   box(
    title = "Export: YearWise"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("yrSpec1", height = "300px")
  ) ,
  
    box(
    title = "Import: YearWise"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("yrSpec2", height = "300px")
  )
  
)

### Row6
frow6 <- fluidRow(
  box(width=12,align="center",
    title = "Ratio"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,h2("Terms of Trade ratio [Export/Import]")
    ,plotOutput("compPlot", height ="800px", width="800px")
  )
)

### Row7
frow7 <- fluidRow(
  box(width=12,align="center",
    title = "Selected Country"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,DT::dataTableOutput('tbl')
  )
)

# combine the two fluid rows to make the body
body <- dashboardBody(tabItems(
  tabItem(tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ), 
  tabName = "dashboard1" , frow1, frow2, frow3),
 
 tabItem(tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ), 
 tabName = "dashboard2" , frow4, frow5),
 
  tabItem(tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ), 
  tabName = "dataTable1" , frow7),
 
  tabItem(tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ), 
  tabName = "dashboard4" ,
  h2("Get in touch",style="color:#c3cb71"),
  hr(),
  img(src="ram.jpg",width=200,height=200,align="center"),
  h3("Ramanathan Perumal M.Tech",style="color:#f7f7f7"),
  h3("Research Engineer at KIT",style="color:#f7f7f7"),
  h3("Karlsruhe, Germany",style="color:#f7f7f7"),
  h4("Phone: +4915213423094",style="color:#ffffba"),
  h4("Email: ramamet4@gmail.com",style="color:#ffffba"),
  h4("github/ramamet",style="color:#ffffba")
  
   ),
 
   tabItem(tags$head(
       tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ), 
  tabName = "dashboard3" , frow6)
 ))

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'trade_statistics',
                    header, sidebar, body,
                    skin='red')
