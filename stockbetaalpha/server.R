#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
#function(input, output, session) {
#server <- 
shinyServer(function(input, output) {
  
  output$text2 <- renderText({
    "Documentation of the App as defined in the forum:
     You can select the ticker either from the above list including the 4 big tech, or by typing the ticker above (e.g. GLD stands for gold).
    On the right-top, a plot with the covariance between the daily returns of the chosen stock and the benchmark S&P500, tracked by SPY.
    On the right-bottom, the time evolution of the price of the stock and of SPY, normalized such that they both start at 1, for the time interval defined above."
  })  
  
  
  
  
    ticker <- reactive({
      ticker <- input$ticker  
      ticker2 <- input$ticker2
      if (ticker2 != "") {
        tryCatch(getSymbols(ticker2, from = input$dates[1], to = input$dates[2]),
                 error = function(e) stop("Invalid ticker symbol."))
        return(ticker2)
      } else {
        return(ticker)
      }
      ticker
    })      

    data <- reactive({
      tickerstock <- ticker()
      dates <- as.Date(input$dates)
      
      # Get data for selected stock and SPY
      stock_data <- getSymbols(tickerstock, from = dates[1], to = dates[2], auto.assign = FALSE)
      spy_data <- getSymbols("SPY", from = dates[1], to = dates[2], auto.assign = FALSE)
      
      adj_stockreturns <- diff(Ad(stock_data))/Ad(stock_data)
      adj_spyreturns <- diff(Ad(spy_data))/Ad(spy_data)

      # Combine and clean data
      data <- na.omit(data.frame(index(stock_data),Ad(stock_data),Ad(spy_data), adj_stockreturns, adj_spyreturns))
      colnames(data) <- c("Date","stock", "SPY", "return_stock","return_SPY")
      data
    })


    
        
    results <- reactive({
      tickerstock <- ticker()
      data <- data()
      #      returns <- ROC(data, type = "discrete")
        
      # Estimate beta and alpha
      model <- lm(data$return_stock ~ data$return_SPY)
      beta <- coef(model)[2]
      alpha <- coef(model)[1]
      
      # Calculate covariance
#      cov <- cov(returns)
#      cov <- as.data.frame(cov)
#      colnames(cov) <- c(tickerstock, "SPY")
#      rownames(cov) <- c(tickerstock, "SPY")
      
      list(beta = beta, alpha = alpha)
    })
    
    output$plot <- renderPlot({
      tickerstock <- ticker()
      data <- data()
      results <- results()

      ggplot(data = data, aes(x = return_SPY, y = return_stock)) +
        geom_point() +
        geom_abline(intercept = results$alpha, slope = results$beta, color = "red") +
        xlab("SPY Returns") +
        ylab(paste(tickerstock,"Returns",sep = " ")) +
        ggtitle("Alpha and Beta Regression")
      
    })
    
    output$plot2 <- renderPlot({
      tickerstock <- ticker()
      data <- data()
      results <- results()
    
      ggplot(data = data, aes(x = Date, y = stock/stock[1], color = tickerstock)) +
        geom_point() +
        geom_line(data = data, aes(x = Date, y = SPY/SPY[1], color = "SPY")) +
        scale_color_manual(values = c("red", "green")) +
        xlab("Date") +
        ylab(paste(tickerstock,"Returns",sep = " ")) +
        scale_y_continuous(trans='log10') +
        ggtitle("Stock price + SPY price") +
        guides(color = guide_legend(title = "Index"))
      
    })  
    
  
    
    output$text1 <- renderText(ticker())
    
    output$results <- renderTable({
      results <- results()
      digits = 5
      results
    })
  })