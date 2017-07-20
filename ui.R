#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(colorspace)
library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
source("Latency_Functions.R")
lun_data = readRDS("anonymized.rds")


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    titlePanel("Latency Visualizer"),
    fluidRow(
      column(width = 11,
             textInput("filter","LUN Filter:",
                       placeholder = 
                         "Use regex i.e. LUN[5-8]|LUN13 will return LUNs 5 to 8 and 13", 
                       width = "100%")
      )
    ),
    plotlyOutput("plot", height = "40%"),
    fluidRow(
      column(width = 11,
             sliderInput("bins", "Time frame:", min = min(lun_data$Timestamp), 
                         max = max(lun_data$Timestamp), 
                         value = c(min(lun_data$Timestamp), 
                                   max(lun_data$Timestamp)),width = "100%")
      )
    ),
    plotlyOutput("plot2",height = "90%")
  )
)
