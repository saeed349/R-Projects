library(shiny)
source("Volatitlity Cone.R")
# shinyServer(
  # function(input,output,session)
  # {
  #   output$myPlot<-renderPlot({
  #     security_type<-input$Stock
  #     
  #     if(security_type=="MSFT")
  #     {
  #       p<-1:10
  #     }
  #     else
  #     {
  #       p<-50:100
  #       
  #     }
  #     pl=cone_call()
  #     plot(pl)
  #     # hist(p,col="red")
  #   }
  #     
  #   )
  #   
  # }
# )

shinyServer(
function(input,output,session)
{
  output$myPlot <- reactivePlot(function() { 
    security_name<-input$Stock
    plot.new()
    plot.window(xlim=c(0,100), ylim = c(-60,100))
    axis(1)
    axis(2)
    title(main="Volatility Cone")
    title(xlab="Time till expiry")
    title(ylab="Volatility")
    # box()
    df<-cone_call(security_name)
    # points(dat$Year, dat$Total, col="red")
    points(50,50,col="red")
    lines(df$Max,col="red")
    lines(df$Min,col="blue")
    lines(df$first_sd_lower,col="yellow")
    lines(df$first_sd_upper,col="green")
    lines(df$second_sd_lower,col="cyan")
    lines(df$second_sd_upper,col="black")
    
    #Stupid move
    # melted_cone=cone_call("TSLA")
    # nPlot(value ~ Days_till_Expiry, group =  'variable', data = melted_cone, 
    #                         type = 'lineChart', id = 'chart')
    lines(df$Min,col="blue")
    if (input$RC) {   lines(dat$Year, dat$dat)}
  })
}
)
