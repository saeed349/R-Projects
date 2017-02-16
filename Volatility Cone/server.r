library(shiny)
library(rCharts)
source("Vol_Cone.R")
symbols <- stockSymbols()
s<-symbols$Name
shinyServer(
  function(input,output,session){
    output$myChart <- renderChart2({
    
    cone=cone_call(symbols$Symbol[which(s==input$stock_name)])
    h1=hPlot(Implied_Volatility ~ Days_Till_Expiry, data =cone, type ='line',
             group = 'variable')
    h1$legend(enabled=FALSE)
    h1$chart(zoomType="xy")
    return(h1)
    }
    )
}
)

