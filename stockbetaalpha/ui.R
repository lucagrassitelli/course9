#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(tidyquant)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
  titlePanel("Stock Beta and Alpha Estimation"),

    # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("ticker", "Select a stock ticker:",
                  c("AAPL" = "AAPL", "AMZN" = "AMZN", "GOOG" = "GOOG", "MSFT" = "MSFT")),
      textInput("ticker2", "Enter a stock ticker:", ""),
      dateRangeInput("dates", "Select a date range:",
                     start = "2010-01-01", end = Sys.Date()),
      verbatimTextOutput("text2")
      
    ),
        # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      textOutput("text1"),
      tableOutput("results"),
      plotOutput("plot2"),
      
        )
    )
))

