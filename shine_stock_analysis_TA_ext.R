library(shiny)
library(quantmod) #Quantitative Financial Modelling Framework
library(bslib)

#Using Git Hub

#UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = 'darkly'),
  titlePanel('Candlestick Plots - Stocks (DIY)'),
  sidebarLayout(
    sidebarPanel(
      textInput('symbol', 'Enter Symbol:', "TSLA"),
      selectInput('days', 'Select Days', c('last 7 days', 'last 30 days', 'last 90 days', 'last 180 days', 'last 365 days'))),
    mainPanel(plotOutput('plot'), plotOutput('plot1')))
  
)

#Server
server <- function (input, output, session) {
  output$plot <- renderPlot({
    E <- new.env()
    data <- getSymbols(input$symbol, src = 'yahoo', env = E,
                       #from = '2021-01-01',
                       #to = '2023-9-08') 
                        from=Sys.Date()-3650, to=Sys.Date())
    chartSeries(E[[data]], theme = 'white', name = data,
                TA = "addVo(); addBBands(); addCCI();
                addEMA(50, col= 'black'); addEMA(20, col= 'blue');
                addEMA(5, col= 'red')", subset = input$days)
  })
  
  output$plot1 <- renderPlot({
    E <- new.env()
    data <- getSymbols(input$symbol, src = 'yahoo', env = E,
                       #from = '2021-01-01',
                       #to = '2023-9-08') 
                       from=Sys.Date()-3650, to=Sys.Date())
    chartSeries(E[[data]], theme = 'white', name = data,
                TA = "addVo(); addBBands(); addCCI();
                addEMA(50, col= 'black'); addEMA(20, col= 'blue');
                addEMA(5, col= 'red')", subset = input$days)
  })
}

#App
shinyApp(ui=ui,server = server)