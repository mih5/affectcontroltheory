library(shiny)
require(ggplot2)
require(dplyr)

#The cleaned duke 10 data
duke10 <- read.csv("duke10fixed.csv")

#change data coding for clarity
duke10 <- duke10 %>% mutate("Sex" = ifelse(duke10$sex==1,"Female","Male")) %>%
  mutate("Sex" = ifelse(duke10$sex==1,"Female","Male"))%>%
  mutate("Year" = c("First-Year", "Second-Year", "Third-Year", "Fourth-Year", "Fifth-Year", "Grad Student", "Not a Student")[duke10$year]) %>%
  mutate("Hispanic" = ifelse(duke10$hisp==1,"Hispanic", "Not Hispanic")) %>%
  mutate("Race" = c("White", "Black", "Native American", "Asian", "Other")[duke10$race1]) %>%
  mutate("Born_US" = ifelse(duke10$usborn==1, "Born in US", "Not Born in US")) %>%
  mutate("Family_Income" = ifelse(duke10$pincome <4, "< $70000", ifelse(duke10$pincome < 6, "$70000-$150000", "> $150000")))


# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  output$plot1 <-renderPlot({
    
    if(input$epa == "Evaluation"){
      selectedIdentityBehavior <- input$evaluation
    }
    else if(input$epa == "Potency"){
      selectedIdentityBehavior <- input$potency
    }
    else if(input$epa == "Activity"){
      selectedIdentityBehavior <- input$activity
    }
    
    
    
    dataSelect <- duke10[,c(input$demo,selectedIdentityBehavior)]
    dataSelect <- dataSelect[complete.cases(dataSelect),]
    names(dataSelect) <- c("group", "variable")
    
    binselect <- input$binSelect
    
    p <- ggplot(data=dataSelect, aes(x=variable)) + 
      geom_histogram(color = "steelblue", fill = "white", binwidth = binselect) + 
      aes(y = ..density..) + ylab("density") + xlab(selectedIdentityBehavior) +
      facet_grid(group ~ ., labeller = label_both ) + 
      theme(text = element_text(size=14))
    p
  }, height = 600, width = 600)
  
})
