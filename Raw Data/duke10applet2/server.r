library(shiny)
require(ggplot2)
require(dplyr)

#The cleaned duke 10 data
duke10 <- read.csv("duke10long.csv")


# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  #output$plot1 <-renderPlot({})

})
