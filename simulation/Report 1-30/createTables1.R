setwd("~/GitHub/affectControlTheory/simulation/Report 1-30")
source("modelCoefs.R")
require("xtable")

models <- c("eae","ebe","eoe","eap", "ebp", "eop", "eaa", "eba", "eoa")

checker <- function(identified.variables, true.variables){
  truePos <- sum(identified.variables %in% true.variables)
  falsePos <- sum(! identified.variables %in% true.variables)
  falseNeg <- sum(! true.variables %in% identified.variables)
  trueNeg <- 63 - truePos - falsePos - falseNeg
  return(c(truePos,falsePos,falseNeg,trueNeg))
}

sensitivity <- function(info, n){
  return(info[1]/(info[1]+info[3]))
}

sensitivityString <- function(info, n){
  return(paste(info[1]/n,"/",(info[1]+info[3])/n, sep = ""))
}

idenError <- function(info){
  return(info[2]/(info[1]+info[2]))
}

idenErrorString <- function(info, n){
  return(paste(info[2]/n,"/",(info[1]+info[2])/n,sep = ""))
}

sensStorage <- NULL
sensStringStorage <- NULL
idenStorage <- NULL
idenStringStorage <- NULL

for(i in 1:length(models)){
  # load data
  load(paste("storage/",models[i],"simulation.txt", sep = ""))
  
  stepwiseSum <- rep(0,4)
  anovaSum <- rep(0,4)
  bmaSum <- rep(0,4)
  bmsSum <- rep(0,4)
  bmaPriorSum <- rep(0,4)
  bmsPriorSum <- rep(0,4)
  
  simulations = length(storage)
  
  for(j in 1:simulations){
    vars.iden <- storage[[j]]$var.iden
    coef.estimation <- storage[[j]]$coef.est
    stepwiseSum = stepwiseSum + checker(vars.iden$stepwise, vars.iden$truth)
    anovaSum = anovaSum + checker(vars.iden$anova, vars.iden$truth)
    bmaSum = bmaSum + checker(vars.iden$bma, vars.iden$truth)
    bmsSum = bmsSum + checker(vars.iden$bms, vars.iden$truth)
    bmaPriorSum = bmaPriorSum + checker(vars.iden$bma.prior, vars.iden$truth)
    bmsPriorSum = bmsPriorSum + checker(vars.iden$bms.prior, vars.iden$truth)
  }
  
  sens <- c(sensitivity(stepwiseSum, simulations),
    sensitivity(anovaSum, simulations),
    sensitivity(bmaSum, simulations),
    sensitivity(bmsSum, simulations),
    sensitivity(bmaPriorSum, simulations),
    sensitivity(bmsPriorSum, simulations))

  sensString <- c(sensitivityString(stepwiseSum, simulations),
    sensitivityString(anovaSum, simulations),
    sensitivityString(bmaSum, simulations),
    sensitivityString(bmsSum, simulations),
    sensitivity(bmaPriorSum, simulations),
    sensitivity(bmsPriorSum, simulations))
  
  iden <- c(idenError(stepwiseSum),
    idenError(anovaSum),
    idenError(bmaSum),
    idenError(bmsSum),
    idenError(bmaPriorSum),
    idenError(bmsPriorSum))
  
  idenString <- c(idenErrorString(stepwiseSum, simulations),
    idenErrorString(anovaSum, simulations),
    idenErrorString(bmaSum, simulations),
    idenErrorString(bmsSum, simulations),
    idenError(bmaPriorSum),
    idenError(bmsPriorSum))
  
  sensStorage <- rbind(sensStorage, sens)
  sensStringStorage <- rbind(sensStringStorage, sensString)
  idenStorage <- rbind(idenStorage, iden)
  idenStringStorage <- rbind(idenStringStorage, idenString)
  
}

inContextNames <- c("Ae","Be","Oe","Ap", "Bp", "Op", "Aa", "Ba", "Oa")
methods <- c("Stepwise", "ANOVA", "BMA", "BMS", "BMA w/ prior", "BMS w/ prior")
rownames(sensStorage) <- inContextNames
rownames(sensStringStorage) <- inContextNames
rownames(idenStorage) <- inContextNames
rownames(idenStringStorage) <- inContextNames
colnames(sensStorage) <- methods
colnames(sensStringStorage) <- methods
colnames(idenStorage) <- methods
colnames(idenStringStorage) <- methods

sensStorage <- rbind(sensStorage,apply(sensStorage, MARGIN = 2, FUN = mean))
idenStorage <- rbind(idenStorage, apply(idenStorage, MARGIN = 2, FUN = mean))

xtable(round(sensStorage,2))
xtable(round(idenStorage,2))


par(mfcol=c(3,3))
plot(simulation.model.eae.3way, main = "A'e")
plot(simulation.model.ebe.3way, main = "B'e")
plot(simulation.model.eoe.3way, main = "O'e")
plot(simulation.model.eap.3way, main = "A'p")
plot(simulation.model.ebp.3way, main = "B'p")
plot(simulation.model.eop.3way, main = "O'p")
plot(simulation.model.eaa.3way, main = "A'a")
plot(simulation.model.eba.3way, main = "B'a")
plot(simulation.model.eoa.3way, main = "O'a")

names(simulation.model.eae.3way)

# 