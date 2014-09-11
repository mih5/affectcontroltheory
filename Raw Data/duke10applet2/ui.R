library(shiny)
duke10 <- read.csv("duke10long.csv")

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Duke 2010 Impression Change Study"),
    
    fluidRow(
      column(4,
             #drop-down menu for selecting Context
             selectInput("context", "Context:", choices = levels(duke10$Context), width='120%')
      ),
      column(8,
             #drop-down menu for selecting Context
             selectInput("context", "Context:", choices = levels(duke10$Context))
      )
    )
    
  )
)

