source("Volatitlity Cone.R")
shinyServer(function(input, output, session) {
  
  # output$mychart <- renderLineChart({
  #   # Return a data frame. Each column will be a series in the line chart.
  #   data.frame(
  #     Sine = sin(1:100/10 + input$sinePhase * pi/180) * input$sineAmplitude,
  #     Cosine = 0.5 * cos(1:100/10),
  #     "Sine 2" = sin(1:100/10) * 0.25 + 0.5,y=rnorm(100,50,70)
  #   )
  # })
  output$mychart <- renderLineChart({
    # Return a data frame. Each column will be a series in the line chart.
    cone_call("TSLA")
  })
  
})
  

# output_test <- renderLineChart({
# p=data.frame(
#   Sine = sin(1:100/10 + 90 * pi/180) * 1,
#   Cosine = 0.5 * cos(1:100/10),
#   "Sine 2" = sin(1:100/10) * 0.25 + 0.5
# )
# })