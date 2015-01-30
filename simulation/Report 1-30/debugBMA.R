# DEBUG BMA
# BMA doesn't work with priors specified, why is this the case?

setwd("~/GitHub/affectControlTheory/simulation/Report 1-30")
source("modelCoefs.R")
source("lynnPriors.R")

sim.B <- simulation.model.eae.3way
trainSize = 100
testSize = 100

require(mvtnorm) #for sampling from multivariate normal
require(ggplot2) #for plotting
require(BMA) #Adrian Raftery's Bayesian Model Averaging Package
require(BMS)
require(dplyr) #for transforming data
require(magrittr) #for coding semantics

# GENERATE DATA

#I want the data generated from the true model to have characteristics similar to the duke10 data
duke10 <- read.csv("~/GitHub/affectcontroltheory/Data Merged by Event/duke10_by_event.csv")

#covariate means
X.mean=matrix(colMeans(duke10[,c("ae","ap","aa","be","bp","ba","oe","op","oa")]))
#covariate SD
X.var=var(duke10[,c("ae","ap","aa","be","bp","ba","oe","op","oa")])

#simulated covariates
sim.X1= rmvnorm(trainSize+testSize,X.mean,X.var)
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
sim.Y=matrix(rmvnorm(1,sim.X1.design.matrix%*%sim.B,diag(Y.sd^2,trainSize+testSize)),ncol=1)

#minus the intercept
sim.X1.design.dataframe <- data.frame(sim.X1.design.matrix)

sim.data <- data.frame(cbind(eae=sim.Y,sim.X1.design.matrix))
names(sim.data) <- c("response","intercept",names(sim.data)[c(-1,-2)])

####################
# CONDUCT ANALYSIS #
####################

#sample observations for training set
select.training.set <- sample(1:(testSize+trainSize),trainSize)

#train.data, a training set consisting of 150 observations
train.data <- sim.data[select.training.set,]
#test.data, a test set consisting of 850 observations
test.data <- sim.data[-select.training.set,]

# BMA

priors <- lynnPriors[,"eae"]

# makes no sense, only intercept
sim.model.BMA <- bic.glm(x = train.data[,c(-1,-2)], y = train.data[,"response"], maxCol=64, glm.family = gaussian(), prior.param = priors)
summary(sim.model.BMA)

# error with dropcols when maxCol is less than number of predictors
sim.model.BMA <- bic.glm(x = train.data[,c(-1,-2)], y = train.data[,"response"], maxCol=63, glm.family = gaussian(), prior.param = rep(0.5,63))

# works now
sim.model.BMA <- bic.glm(x = train.data[,c(3:11)], y = train.data[,"response"], maxCol=65, glm.family = gaussian(), prior.param = rep(0.5,9))
summary(sim.model.BMA)

# doesn't work
sim.model.BMA <- bic.glm(x = train.data[,c(3:11)], y = train.data[,"response"], maxCol=9, glm.family = gaussian(), prior.param = rep(0.5,9))
summary(sim.model.BMA)


# works now
sim.model.BMA <- bic.glm(x = train.data[,c(3:50)], y = train.data[,"response"], maxCol=65, glm.family = gaussian())
summary(sim.model.BMA)

# gets very slow
sim.model.BMA <- bic.glm(x = train.data[,c(3:52)], y = train.data[,"response"], maxCol=65, glm.family = gaussian())
summary(sim.model.BMA)

sim.model.BMA <- bic.glm(x = train.data[,c(-1,-2)], y = train.data[,"response"], maxCol=30, glm.family = gaussian(), strict = TRUE)
summary(sim.model.BMA)

# examine bic.glm code
# https://github.com/cran/BMA/blob/master/R/bic.glm.R

glm.out <- glm(y ~ ., family = gaussian, data = data.frame(y = train.data[,"response"], train.data[,c(-1,-2)]))
dropglm <- drop1(glm.out, test = "Chisq")
# dropped <- which.max(dropglm$LRT[-1]) + 1
# so essentially here is the problem
dropped <- which.max(dropglm[,"Pr(>Chi)"][-1]) + 1
dropped

# source correction
source("fixBMA.R")
assignInNamespace("bic.glm", bic.glm, "BMA")

sim.model.BMA <- bic.glm(x = train.data[,c(-1,-2)], y = train.data[,"response"], maxCol=30, glm.family = gaussian(), prior.param = priors)
summary(sim.model.BMA)

var.iden.BMA <- sim.model.BMA$namesx[sim.model.BMA$probne0>50]
var.iden.BMA
