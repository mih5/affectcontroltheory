library(shiny)
duke10 <- read.csv("duke10fixed.csv")

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Duke 2010 Impression Change Study"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      sidebarPanel(
        p("Data from:"),
        selectInput("epa","Affective Dimension", choices=c("Evaluation (bad - good)"= "Evaluation","Potency (powerless - powerfull)" = "Potency","Activity (quiet - noisy)" = "Activity")),
        
        conditionalPanel(condition = "input.epa == 'Evaluation' ",
          selectInput("evaluation","Identity/Behavior",choices=names(duke10)[c(22:58,1285:1330)])
        ),
        
        conditionalPanel(condition = "input.epa == 'Potency' ",
          selectInput("potency","Identity/Behavior",choices=names(duke10)[c(59:95,1331:1376)])
        ),
        
        conditionalPanel(condition = "input.epa == 'Activity' ",
          selectInput("activity","Identity/Behavior",choices=names(duke10)[c(96:132,1376:1420)])            
        ),
        
        
        selectInput("demo","Demographic:",choices=list("Sex", "Year", "Hispanic", "Race", "Born_US", "Family_Income")),
        sliderInput("binSelect", label = "Width of histogram bins", min = 0.1, max = 2, value=1, step = 0.1)
        ),
      
      mainPanel(
        plotOutput('plot1')
        )
  
    )
  )
)

