setwd("~/GitHub/affectControlTheory/simulation/Report 1-30")
source("modelCoefs.R")

models <- c("eae","ebe","eoe","eap", "ebp", "eop", "eaa", "eba", "eoa")

#function for filling storage
fillArray = function(coef){
  simCoef = rep(0,length(truthModel))
  names(simCoef) <- names(truthModel)
  simCoef[names(coef)] <- coef
  return(simCoef)
}

storeBias <- NULL
storeVar <- NULL

for(i in 1:length(models)){
  # load data
  load(paste("storage/",models[i],"simulation.txt", sep = ""))
  
  truthModel <- eval(parse(text = paste("simulation.model.",models[i],".3way", sep = "")))
  
  getError <- function(X){
    return( (X-truthModel)^2 )
  }
  
  #storage for coefficients
  storeCoefStepwise = NULL
  storeCoefANOVA = NULL
  storeCoefBMA = NULL
  storeCoefBMS = NULL
  storeCoefBMAprior = NULL
  storeCoefBMSprior = NULL
  
  simulations = length(storage)
  
  for(j in 1:simulations){
    simulation <- storage[[j]]$coef.est
    storeCoefStepwise <- rbind(storeCoefStepwise, fillArray(simulation$stepwise))
    storeCoefANOVA <- rbind(storeCoefANOVA, fillArray(simulation$anova))
    storeCoefBMA <- rbind(storeCoefBMA, fillArray(simulation$bma))
    storeCoefBMS <- rbind(storeCoefBMS, fillArray(simulation$bms))
    storeCoefBMAprior <- rbind(storeCoefBMAprior, fillArray(simulation$bma.prior))
    storeCoefBMSprior <- rbind(storeCoefBMSprior, fillArray(simulation$bms.prior))
    
  }
#   
#   plot(colMeans(t(apply(storeCoefStepwise, MARGIN = 1, FUN = getError))), ylim = c(0,0.04))
#   plot(colMeans(t(apply(storeCoefANOVA, MARGIN = 1, FUN = getError))), ylim = c(0,0.04))
#   plot(colMeans(t(apply(storeCoefBMA, MARGIN = 1, FUN = getError))), ylim = c(0,0.04))
#   plot(colMeans(t(apply(storeCoefBMS, MARGIN = 1, FUN = getError))), ylim = c(0,0.04))
#   
#   plot(apply(storeCoefStepwise, MARGIN = 2, FUN = var), ylim = c(0, 0.03))
#   plot(apply(storeCoefANOVA, MARGIN = 2, FUN = var), ylim = c(0, 0.03))
#   plot(apply(storeCoefBMA, MARGIN = 2, FUN = var), ylim = c(0, 0.03))
#   plot(apply(storeCoefBMS, MARGIN = 2, FUN = var), ylim = c(0, 0.03))

    storeBias <- rbind(storeBias,
      c(
        mean(colMeans(t(apply(storeCoefStepwise, MARGIN = 1, FUN = getError)))),
        mean(colMeans(t(apply(storeCoefANOVA, MARGIN = 1, FUN = getError)))),
        mean(colMeans(t(apply(storeCoefBMA, MARGIN = 1, FUN = getError)))),
        mean(colMeans(t(apply(storeCoefBMS, MARGIN = 1, FUN = getError)))),
        mean(colMeans(t(apply(storeCoefBMAprior, MARGIN = 1, FUN = getError)))),
        mean(colMeans(t(apply(storeCoefBMSprior, MARGIN = 1, FUN = getError))))
      )
    )

    storeVar <- rbind(storeVar,
                      
                      c( mean(apply(storeCoefStepwise, MARGIN = 2, FUN = var)),
                         mean(apply(storeCoefANOVA, MARGIN = 2, FUN = var)),
                         mean(apply(storeCoefBMA, MARGIN = 2, FUN = var)),
                         mean(apply(storeCoefBMS, MARGIN = 2, FUN = var)),
                         mean(apply(storeCoefBMAprior, MARGIN = 2, FUN = var)),
                         mean(apply(storeCoefBMSprior, MARGIN = 2, FUN = var))
                        )
                      
                      )

}

inContextNames <- c("Ae","Be","Oe","Ap", "Bp", "Op", "Aa", "Ba", "Oa")
methods <- c("Stepwise", "ANOVA", "BMA", "BMS", "BMA w/ prior", "BMS w/ prior")

rownames(storeBias) <- inContextNames
rownames(storeVar) <- inContextNames
colnames(storeBias) <- methods
colnames(storeVar) <- methods

storeBias <- rbind(storeBias, t(colMeans(storeBias)))
storeVar <- rbind(storeVar, t(colMeans(storeVar)))

require(xtable)

xtable(storeBias, digits = -1)

xtable(storeVar, digits = -1)

