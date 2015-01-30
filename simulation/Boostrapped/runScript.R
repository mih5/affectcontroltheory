# IMPORT DATA
setwd("~/GitHub/affectControlTheory/simulation/Boostrapped")
source("runBootstrap.R")
require(corrplot)
require(dplyr)
require(BMA)
require(BMS)
require(dplyr)

duke10 <- read.csv("~/GitHub/affectcontroltheory/Data Merged by Event/duke10_by_event.csv")

X1 <- duke10[,10:18]

# TWO-WAY CROSS WORD INTERACTIONS
X1.interaction = as.matrix(t(apply(X1,1,combn,2,prod)))
colnames(X1.interaction)=paste(combn(names(duke10[,10:18]),2,paste,collapse="."),sep="")
X1.interaction.2=X1.interaction[,c(-1,-2,-9,-22,-23,-27,-34,-35,-36)]

# THREE WAY CROSS WORD INTERACTIONS

#first, generate all three-way interaction names
X1.3.names.full=paste(combn(names(duke10[,10:18]),3,paste,collapse="."),sep="")

#for-loop looks through all the interaction names and stores only those which are cross-word interactions
X1.3.names = matrix(nrow = 84, ncol =2)
for (i in 1:length(X1.3.names.full)){
  interaction=X1.3.names.full[i]
  words = c(substr(interaction,1,1),substr(interaction,4,4),substr(interaction,7,7))
  if(!is.na(sum(pmatch(c("a","b","o"),words)))){
    X1.3.names[i,] <- c(interaction,TRUE)
  }
  else{
    X1.3.names[i,] <- c("",FALSE)
  }
}

#then, calculate the interactions and use the names to select which ones to keep
X1.interaction.3 <- as.matrix(t(apply(X1,1,combn,3,prod)))[,X1.3.names[,2]=="TRUE"]
colnames(X1.interaction.3)=X1.3.names[which(X1.3.names[,2]=="TRUE"),1]

# COMBINE TWO-WAY AND THREE-WAY INTERACTIONS
X1.design.matrix = (cbind(X1,X1.interaction.2,X1.interaction.3))

data2 <- data.frame("response"=duke10[,1],"intercept"=1,X1.design.matrix)
names(data2)

storage <- list()

BOOSTRAPS = 10
for(i in 1:BOOSTRAPS){
  bootstrap <- runBootstrap(data2)
  storage[[length(storage)+1]] <- bootstrap  
}

storage[[1]]$var.iden
storage[[2]]$var.iden
