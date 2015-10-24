# library(mtusRlocal)
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(dplyr)
kol = brewer.pal(12, 'Set3')
# seqDay5 = read.csv('data/seqDay5.csv')
seqDta = read.csv('data/seqDtaCompleteAggregate.csv')


# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage('Navigate', 
             
             tabPanel("Aggregate", plotOutput("aggregate", height = 600, width = 1000), hr(), fluidRow()), 
             
             tabPanel("Sequences", plotOutput("sequence", height = 600, width = 1400), hr(), fluidRow(column(4,sliderInput("time","Choose the time of the day", min = 1, max = 1440, value = c(1, 1440)) ), 
                                                                                                    column(4,selectInput("day", "days:", choices = levels(factor(seqDta$diaryday)) )) 
                                                                                                    )), 
             tabPanel("Table", mainPanel(column(12,
                                                tableOutput('table') 
                                                ) ) ) , 
             
             tabPanel("About", mainPanel(column(6,
                                                includeMarkdown("about.md"), hr() , 
                                                p('This app is designed with RShiny', a('http://shiny.rstudio.com/') ) ) )
             )
  )
)







