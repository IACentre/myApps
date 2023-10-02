library(shiny)
library(shinydashboard)
library(quantmod)
library(ggplot2)
library(forecast)
library(tseries)


# Define UIa
ui <- dashboardPage(
  dashboardHeader(title = span(img(src = 'IACentre_OW.PNG', height = 35),"Stock Analyzer"),
                  tags$li(class="dropdown",
                          tags$a(href="https://www.linkedin.com/in/iacentre/" , icon("linkedin"), "My Profile", target="_blank"))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Stock Analysis", tabName = "stock", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(
                  title = "Welcome to Stock Price Forecast",
                  "This is a simple stock price forecasting app."
                )
              )
      ),
      tabItem("stock",
              fluidRow(
                box(
                  title = "Enter Stock Code",
                  textInput("stockCode", "Stock Code", "AAPL"),
                  actionButton("analyzeBtn", "Analyze")
                )
              ),
              fluidRow(
                box(
                  title = "Stock Price Plot",
                  plotOutput("stockPlot")
                ), 
                box(
                  title = "Stock Price Forecast",
                  tableOutput("forecastTable")
                )
              ),
              fluidRow(
                box(
                  title = "Stock Price Forecast",
                  plotOutput("stockPlot1")
                ),
                box(
                  title = "Stock Price",
                  tableOutput("stockTable")
                )
              )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  
  observeEvent(input$analyzeBtn, {
    stockCode <- toupper(input$stockCode)
    
    # Retrieve stock data from Yahoo Finance
    stockData <- getSymbols(stockCode, from = Sys.Date() - 90, to = Sys.Date(), auto.assign = FALSE)
    
    # Check if the stock data is available
    if (!is.null(stockData)) {
      # Fit ARIMA model
      fit <- auto.arima(Cl(stockData), ic = "bic")
      
      # Generate a forecast
      forecastData <- forecast(fit, h = 10)
      
      # Plot stock prices
      output$stockPlot <- renderPlot({
        autoplot(forecastData, main = paste("Stock Price Forecast for", stockCode))
      })
      
      # Display forecast table
      output$forecastTable <- renderTable({
        as.data.frame(forecastData)
      })
      
      # Create a chart using chartSeries
      output$stockPlot1 <- renderPlot({
        chartSeries(stockData, theme = "white", name = stockCode, 
                    TA = "addVo(); addBBands(); addCCI();
                addEMA(50, col= 'black'); addEMA(20, col= 'blue');
                addEMA(5, col= 'red')")
      })
      
      stockData10 <- getSymbols(stockCode, from = Sys.Date() - 10, to = Sys.Date(), auto.assign = FALSE)
      df = data.frame(date = index(stockData10), stockData10)
      colnames(df) <- c("Date", "Open","High","Low", "Close", "Volume","Adjusted")
      output$stockTable <- renderTable({
        df
      })
      
    } else {
      # Display an error message if the stock data is not available
      output$stockPlot <- renderPlot({
        plot(0, type = "n", xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "")
        text(0.5, 0.5, "Stock data not found. Please check the stock code.", cex = 1.5)
      })
      output$forecastTable <- renderTable(NULL)
    }
  })
}

shinyApp(ui, server)
