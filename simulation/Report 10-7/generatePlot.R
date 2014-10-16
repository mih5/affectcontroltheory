generatePlot <- function(resultsMatrix){
  require(ggplot2)
  require(reshape2)
  require(dplyr)
  
  
  plotData <- melt(resultsMatrix)
  
  plotData <- plotData %>% mutate(method = sapply(plotData$variable, FUN = getMethod)) %>% mutate(statType = factor(sapply(plotData$variable, FUN = getStat)))
  
  plotData2 <- plotData %>% group_by(method, statType) %>% summarize(value.mean = mean(value))
  
  p <- ggplot(plotData, aes(x = value)) + geom_histogram(aes(y=..density..), color = "steelblue", fill="white", binwidth=1) + geom_vline(data=plotData2, aes(xintercept=value.mean,linetype="dashed", colour="red"))
    
  p + facet_grid(method ~ statType, scales = "free")+theme_set(theme_gray(base_size = 20))
  
  
  
}


generateSensPlot <- function(resultsMatrix){
  
  plotData <- mutate(resultsMatrix, trial = rownames(resultsMatrix))
  
  plotData <- melt(plotData)
  
  plotData <- plotData %>% mutate(method = sapply(plotData$variable, FUN = getMethod)) %>% mutate(statType = factor(sapply(plotData$variable, FUN = getStat)))
  
  plotData <- dcast(plotData[,c("value","method", "statType","trial")], trial + method ~ statType)
  
  plotData <- plotData %>% mutate(Sensitivity = round(TruePositive/(TruePositive+FalseNegative),digits=2)) %>% mutate(Specificity = round(TrueNegative/(TrueNegative + FalsePositive),digits=2))
  
  plotData <- melt(plotData[,c("trial","method","Sensitivity","Specificity")])
  
  plotData2 <- plotData %>% group_by(method, variable) %>% summarize(value.mean = mean(value))
  
  p <- ggplot(plotData, aes(x=value)) + geom_histogram(aes(y=..density..), color = "steelblue", fill="white", binwidth=.1) +  geom_vline(data=plotData2, aes(xintercept=value.mean,linetype="dashed", colour="red"))
                                                                                                                                         
  p + facet_grid(method ~ variable, scales = "free")+theme_set(theme_gray(base_size = 20))
  
}


getMethod <- function(variableName){
  index = grep(substr(variableName,1,3),c("stepwise","anova","bma","bms"))
  return(c("stepwise","anova","bma","bms")[index])
}

getStat <- function(variableName){
  name = substr(variableName,3,nchar(as.character(variableName)))
  select = c(grepl("TruePositive", name), grepl("FalsePositive", name), grepl("FalseNegative", name), grepl("TrueNegative", name))
  return(c("TruePositive","FalsePositive","FalseNegative","TrueNegative")[select])
}