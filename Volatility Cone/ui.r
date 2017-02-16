require(rCharts)
require(quantmod)
symbols <- stockSymbols()
shinyUI(pageWithSidebar(
  headerPanel("Probability Cone with Implied Volatility of Options"),
  
  sidebarPanel(
    "This shiny app lets you select an equity and you can see the probability
    cone of the particular security over which the Implied Volatility of the 
    Options are plotted. The lines represent the different standard deviations of 
    volatilities. This can be used by both option sellers and buyers to trade volatility.
    Zoom and hower through the chart to see more details",
    "PLEASE SELECT A SECURITY THAT HAS OPTIONS",
    selectInput(inputId = "stock_name",
                label = "Choose the security",
                choices = symbols$Name, #choices = c("Male", "Female"),
                selected = "Male")
  ),
  mainPanel(
    showOutput("myChart", "Highcharts")
  )
))