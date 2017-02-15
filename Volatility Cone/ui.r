# shinyServer(
#   pageWithSidebar(
#     headerPanel("Volatility Cone"),
#     sidebarPanel(
#       selectInput("Stock","Select the secuirty name",
#                   choices = c("MSFT","GOOGLE"))
#     ),
#     mainPanel(
#       plotOutput("myPlot")
#     )
#   )
# )

# library(shiny)
# 
# shinyUI(fluidPage(
#   titlePanel(title="First app"),
#   sidebarLayout(
#     sidebarPanel("Side bar-only beer"),
#     mainPanel("Main entrance")
#   )
# ))

require(rCharts)
library(quantmod)
symbols <- stockSymbols()
shinyUI(pageWithSidebar(
  headerPanel("Probability Cone with Implied Volatility of Options"),
  
  sidebarPanel(
    selectInput(inputId = "stock_name",
                label = "Choose the security",
                choices = symbols$Name, #choices = c("Male", "Female"),
                selected = "Male")
    # ,selectInput(inputId = "type",
    #             label = "Choose Chart Type",
    #             choices = c("multiBarChart", "multiBarHorizontalChart"),
    #             selected = "multiBarChart"),
    # checkboxInput(inputId = "stack",
    #               label = strong("Stack Bars?"),
    #               value = FALSE)
  ),
  mainPanel(
    showOutput("myChart", "Highcharts")
  )
))