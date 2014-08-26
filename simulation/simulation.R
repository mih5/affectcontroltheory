#################
# LOAD PACKAGES #
#################

require(mvtnorm) #for sampling from multivariate normal
require(ggplot2) #for plotting
require(BMA) #Adrian Raftery's Bayesian Model Averaging Package
require(magrittr) #for coding semantics

set.seed(10294) #true model

###################################
# TRUE MODEL FOR eae              #
# THE IN-CONTEXT ACTOR EVALUATION #
###################################


#Model coefficients
sim.B = c(-0.3,0.19,0.23,-0.25,rnorm(24,0,.0125),0.25,rnorm(20,0,.0125),-0.25,rnorm(14,0,.025))

qplot(x=1:64,y=sim.B,xlab="Index",ylab="Coefficient",size=5)+theme(text=element_text(size=20),legend.position="none")

############################
# SIMULATED DATA FROM MODEL#
############################

#I want the data generated from the true model to have characteristics similar to the duke10 data
duke10 <- read.csv("~/GitHub/affectcontroltheory/Data Merged by Event/duke10_by_event.csv")

corrplot(cor(duke10[,c("ae","ap","aa","be","bp","ba","oe","op","oa")]))

#covariate means
X.mean=matrix(colMeans(duke10[,c("ae","ap","aa","be","bp","ba","oe","op","oa")]))
#covariate SD
X.var=var(duke10[,c("ae","ap","aa","be","bp","ba","oe","op","oa")])

#simulated covariates
sim.X1= rmvnorm(1000,X.mean,X.var)
colnames(sim.X1) <- colnames(duke10[,10:18])

# TWO-WAY CROSS WORD INTERACTIONS
sim.X1.interaction = as.matrix(t(apply(sim.X1,1,combn,2,prod)))
colnames(sim.X1.interaction)=paste(combn(names(duke10[,10:18]),2,paste,collapse="."),sep="")
sim.X1.interaction.2=sim.X1.interaction[,c(-1,-2,-9,-22,-23,-27,-34,-35,-36)]

# THREE WAY CROSS WORD INTERACTIONS

#first, generate all three-way interaction names
sim.X1.3.names.full=paste(combn(names(duke10[,10:18]),3,paste,collapse="."),sep="")

#for-loop looks through all the interaction names and stores only those which are cross-word interactions
sim.X1.3.names = matrix(nrow = 84, ncol =2)
for (i in 1:length(sim.X1.3.names.full)){
  interaction=sim.X1.3.names.full[i]
  words = c(substr(interaction,1,1),substr(interaction,4,4),substr(interaction,7,7))
  if(!is.na(sum(pmatch(c("a","b","o"),words)))){
    sim.X1.3.names[i,] <- c(interaction,TRUE)
    print(interaction)
  }
  else{
    sim.X1.3.names[i,] <- c("",FALSE)
  }
}

#then, calculate the interactions and use the names to select which ones to keep
sim.X1.interaction.3 <- as.matrix(t(apply(sim.X1,1,combn,3,prod)))[,sim.X1.3.names[,2]=="TRUE"]
colnames(sim.X1.interaction.3)=sim.X1.3.names[which(sim.X1.3.names[,2]=="TRUE"),1]

# COMBINE TWO-WAY AND THREE-WAY INTERACTIONS
sim.X1.design.matrix = (cbind(1,sim.X1,sim.X1.interaction.2,sim.X1.interaction.3))


#let the sd of the random error come from a lm on the real data
model.summary <- lm(eae ~. , data = duke10[,c("eae", "ae","ap","aa","be","bp","ba","oe","op","oa")]) %>% summary()
Y.sd <- model.summary$sigma

#GENERATE FAKE DATA FROM MODEL
#here we add intercept
sim.Y=matrix(rmvnorm(1,sim.X1.design.matrix%*%sim.B,diag(Y.sd^2,1000)),ncol=1)

# COMPARE FAKE DATA WITH REAL DATA
sd(sim.Y)
sd(duke10$eae)

#minus the intercept
sim.X1.design.dataframe <- data.frame(sim.X1.design.matrix)
corrplot(cor(sim.X1.design.dataframe[,c("ae","ap","aa","be","bp","ba","oe","op","oa")]))
corrplot(cor(duke10[,c("ae","ap","aa","be","bp","ba","oe","op","oa")]))

####################
# CONDUCT ANALYSIS #
####################

sim.data <- data.frame(cbind(eae=sim.Y,sim.X1.design.matrix))

#OLS REGRESSION
sim.model.ols = lm(V1~.,data=sim.data[,-2])
summary(sim.model.ols)

#BAYESIAN MODEL AVERAGING
sim.model.BMA = bicreg(sim.data[,-1],sim.data[,1])
summary(sim.model.BMA)

#LASSO


