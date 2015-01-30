
setwd("~/GitHub/affectControlTheory/simulation/Report 1-30")
source("runSimulation.R")
source("modelCoefs.R")
source("lynnPriors.R")

SIMULATIONS = 45;

models <- c("eae","ebe","eoe", "eap", "ebp", "eop", "eaa", "eba", "eoa")

ptm = proc.time()
for (i in 1:length(models)){
  model <- eval(parse(text=paste("simulation.model.",models[i],".3way",sep="")))
  storage <- list()
  for (j in 1:SIMULATIONS){
    simulation <- runSimulation(model, trainSize=200, testSize=1, prior = lynnPriors[,paste(models[i])])
    storage[[length(storage)+1]] <- simulation
  }
  storageFile <- paste("~/GitHub/affectControlTheory/simulation/Report 1-30/storage/",models[i],"simulation.txt",sep="")
  save(storage, file=storageFile)
}
proc.time()-ptm

# 1 simulation, 9 models takes about 10 minutes
# 45 simulation, 9 models takes about 4.7 hours