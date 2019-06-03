library(shiny)
library(shinyWidgets)
library(tidyverse)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(rlang)
library(DT)



shinyUI(
  navbarPage(
    title=tags$b(""),
    windowTitle="National Assessment of Educational Progress",
    tabPanel("NAEP Score by State",
             sidebarLayout(
               sidebarPanel(width = 3,
                 helpText("Please select a National Assessment of Educational Progress (NAEP) exam and spending metric."),
                 wellPanel(
                   selectInput(inputId="funding", label="Select Educational Spending:", 
                               choices=c()),
                   selectInput(inputId="score", label="Select a Test:", 
                               choices=c()),
                   checkboxInput(inputId="per_cap4",label ="Adjust for per capita", value = FALSE),
                   shinyWidgets::sliderTextInput(inputId = "year", 
                                                 label = "Time :", 
                                                 choices = c(1992,1996,2000,2003,2005,2007,2009,2011,2013,2015,2017),
                               animate = animationOptions(interval = 1000, loop = TRUE))
                 )
               ),
               mainPanel(fluidRow(column(titlePanel("Expenditure by State"), width = 12, align = "center"),
                 column(plotlyOutput("map_fund"),width = 12),
                 column(titlePanel("Average NAEP Score by State"), width = 12, align = "center"),
                 column(plotlyOutput("map"),width =12))
               )
             )
    ),
    tabPanel("Time Series Exploration",
             sidebarLayout(
               sidebarPanel(width = 3,
                            helpText("Please select states,spending metric, and average test score to compare."),
                            wellPanel(
                              selectInput(inputId="states", label="Select States:", 
                                          choices=c(), multiple = TRUE),
                              selectInput(inputId="funding1", label="Select Educational Spending Metric:", 
                                          choices=c()),
                              selectInput(inputId="score1", label="Select a Test:", 
                                          choices=c()),
                              checkboxInput(inputId="per_cap",label ="Adjust for per capita", value = FALSE)
                            )
               ),
             mainPanel(fluidRow(column(titlePanel("Expenditure Over Time"), width = 12, align = "center"),
               column(plotlyOutput("exp_line"),width = 12),
               column(titlePanel("NAEP Scores Over Time"), width = 12, align = "center"),
               column(plotlyOutput("score_line"),width = 12)
             ))
             )
  ),
  tabPanel("Top Ten",sidebarLayout(
    sidebarPanel(width = 3,
                 helpText("Please select states,spending metric, and average test score to generate top ten results."),
                 wellPanel(
                   selectInput(inputId="funding2", label="Select Educational Spending Metric:", 
                               choices=c()),
                   selectInput(inputId="score2", label="Select a Test:", 
                               choices=c()),
                   checkboxInput(inputId="per_cap2",label ="Adjust for per capita", value = FALSE),
                   shinyWidgets::sliderTextInput(inputId = "year2", 
                                                 label = "Time :", 
                                                 choices = c(1992,1996,2000,2003,2005,2007,2009,2011,2013,2015),
                                                 animate = animationOptions(interval = 1000, loop = TRUE))
                 )
    ),
    mainPanel(fluidRow(
              column(titlePanel("Top Ten States by Expenditure"), width = 12, align = "center"),
              column(plotlyOutput("expen_ten"),width = 12),
              column(titlePanel("Lowest Ten States by Expenditure"), width = 12, align = "center"),
              column(plotlyOutput("expen_ten_low"),width = 12),
              column(titlePanel("Top Ten States by Exam Score"), width = 12, align = "center"),
              column(plotlyOutput("score_ten"),width = 12),
              column(titlePanel("Lowest Ten States by Exam Score"), width = 12, align = "center"),
              column(plotlyOutput("score_ten_low"),width = 12)
              )
    )
  )
  ),
  
  tabPanel("Expenditure v. Score",
           sidebarLayout(
             sidebarPanel(width = 3,
                          helpText("Please select states and spending metric to compare."),
                          wellPanel(
                            pickerInput(inputId="states3", label="Select States:", 
                                        choices=c(), multiple = TRUE,  options = list(`actions-box` = TRUE)),
                            selectInput(inputId="funding3", label="Select Educational Spending Metric:", 
                                        choices=c()),
                            checkboxInput(inputId="per_cap3",label ="Adjust for per capita", value = TRUE)
                          )
             ),
             mainPanel(fluidRow(
               column(titlePanel("Math Scores"), width = 12, align = "center"),
               column(plotlyOutput("math_4th"),width = 6),
               column(plotlyOutput("math_8th"),width = 6),
               column(titlePanel("Reading Scores"), width = 12, align = "center"),
               column(plotlyOutput("reading_4th"),width = 6),
               column(plotlyOutput("reading_8th"),width = 6)
               )
             )
             
           )
  ),
    
  tabPanel("Data",
    mainPanel(fluidRow(
      column(titlePanel("K-12 Financial, Enrollment, and Achievement Data"), width = 12, align = "left"),
      column(dataTableOutput("table"), width = 12, align = "center")
      
      
      
    )
  )
  
  )
)
)

