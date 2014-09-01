library(shiny)

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Duke 2010 Impression Change Study"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      sidebarPanel(),
      
      mainPanel()
  
    )
  )
)

