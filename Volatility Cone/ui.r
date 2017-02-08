shinyServer(
  pageWithSidebar(
    headerPanel("Volatility Cone"),
    sidebarPanel(
      selectInput("Stock","Select the secuirty name",
                  choices = c("MSFT","GOOGLE"))
    ),
    mainPanel(
      plotOutput("myPlot")
    )
  )
)

# library(shiny)
# 
# shinyUI(fluidPage(
#   titlePanel(title="First app"),
#   sidebarLayout(
#     sidebarPanel("Side bar-only beer"),
#     mainPanel("Main entrance")
#   )
# ))
