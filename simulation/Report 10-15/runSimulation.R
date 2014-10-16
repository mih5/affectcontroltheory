runSimulation <- function(sim.B, trainSize, testSize){
  #function which runs a simulation for a specified model
  #and returns a list of:
  #variables identified
  #mean square predictive errors
  
  require(mvtnorm) #for sampling from multivariate normal
  require(ggplot2) #for plotting
  require(BMA) #Adrian Raftery's Bayesian Model Averaging Package
  require(BMS)
  require(dplyr) #for transforming data
  require(magrittr) #for coding semantics
  require(randomForest)
  
  
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
  
  
  #OLS REGRESSION
  sim.model.ols = lm(response~.,data=train.data[,-2])
  
  #STEPWISE REGRESSION
  sim.model.stepwise = step(sim.model.ols, trace = FALSE)
  
  #store identified variables
  var.iden.stepwise <- names(sim.model.stepwise$coefficients)[-1]
  
  #BAYESIAN MODEL AVERAGING
  
  #unfortunately can't specify prior probability
  sim.model.BMA <- bicreg(x = train.data[,c(-1,-2)], y = train.data[,"response"], maxCol=30)
  #store identified variables
  var.iden.BMA <- sim.model.BMA$namesx[sim.model.BMA$probne0>50]
  
  #BAYESIAN MODEL AVERAGING
  sim.model.BMS <- bms(train.data[,-2], iter=10000)
  
  
  var.iden.BMS <- rownames(coef(sim.model.BMS))[which(coef(sim.model.BMS)[,1]>0.5)]
  
  #HEISE ANOVA ANALYSIS
  
  #select the main factors and get medians for the columns
  train.data.main.factors <- select(train.data,response,ae,ap,aa,be,bp,ba,oe,op,oa)
  mainFactorMedians <- select(train.data.main.factors,ae,ap,aa,be,bp,ba,oe,op,oa)%>%apply(MARGIN=2, FUN=median)
  
  #then, dichotomize these main factors
  #note, mutation written out manually for clarity
  train.data.dichotomized <-
    mutate(train.data.main.factors, 
           ae = ae > mainFactorMedians["ae"],
           ap = ap > mainFactorMedians["ap"],
           aa = aa > mainFactorMedians["aa"],
           be = be > mainFactorMedians["be"],
           bp = bp > mainFactorMedians["bp"],
           ba = ba > mainFactorMedians["ba"],
           oe = oe > mainFactorMedians["oe"],
           op = op > mainFactorMedians["op"],
           oa = oa > mainFactorMedians["oa"],) 
  
  #generate interaction terms from names in the train.data
  #requires \\., regular expression for "."
  formula <- gsub("\\.","*",names(train.data)[c(-1,-2)]) %>% paste(collapse = "+")
  formula
  
  #conduct anova
  anova.model <- aov(response ~ ae+ap+aa+be+bp+ba+oe+op+oa+ae*be+ae*bp+ae*ba+ae*oe+ae*op+ae*oa+ap*be+ap*bp+ap*ba+ap*oe+ap*op+ap*oa+aa*be+aa*bp+aa*ba+aa*oe+aa*op+aa*oa+be*oe+be*op+be*oa+bp*oe+bp*op+bp*oa+ba*oe+ba*op+ba*oa+ae*be*oe+ae*be*op+ae*be*oa+ae*bp*oe+ae*bp*op+ae*bp*oa+ae*ba*oe+ae*ba*op+ae*ba*oa+ap*be*oe+ap*be*op+ap*be*oa+ap*bp*oe+ap*bp*op+ap*bp*oa+ap*ba*oe+ap*ba*op+ap*ba*oa+aa*be*oe+aa*be*op+aa*be*oa+aa*bp*oe+aa*bp*op+aa*bp*oa+aa*ba*oe+aa*ba*op+aa*ba*oa, data = train.data.dichotomized)
  
  #get p-values from anova
  #ignore the "residual" output
  var.iden.anova <- (rownames(anova(anova.model))[-64])[ anova(anova.model)$'Pr(>F)'[-64] < 0.01 ]
  var.iden.anova <- gsub(":",".",var.iden.anova)
  
  #run anova model
  
  sim.model.anova <- lm(response~.,data=train.data[,c("response",var.iden.anova)])
  
  
  #TRUE MODEL
  
  #minus the intercept
  var.model <- names(sim.B)[sim.B!=0][-1]
  
  #########################
  # COEFFICIENT ESTIMATES #
  #########################
  
  #stepwise
  coef.stepwise <- coef(summary(sim.model.stepwise))[, "Estimate"]
  
  #BMA
  coef.BMA <- sim.model.BMA$postmean
  
  #BMS
  coef.BMS <- coef(sim.model.BMS)[,"Post Mean"]
  
  #ANOVA
  coef.anova <- coef(summary(sim.model.anova))[, "Estimate"]
  
  
  
  ##############
  # PREDICTION #
  ##############
  
  #stepwise
  residuals.stepwise <- test.data[,1]-predict(sim.model.stepwise, newdata=test.data[,-1])
  
  #BMA
  #predict(sim.model.BMA, newdata = test.data)
  
  #obtain median model 
  paste(var.iden.BMA, collapse="+")
  median.bma.model <- lm(response ~ ae+be+ba+ae.be+ap.be+be.op+bp.oe+ap.be.op, data = train.data)
  residuals.bma.median <- test.data[,1] - predict(median.bma.model, newdata = test.data)
  
  #ANOVA model
  paste(var.iden.anova, collapse="+")
  anova.model <- lm(response ~ ae+aa+be+ba+be.op, data=train.data)
  residuals.anova <- test.data[,1] - predict(anova.model, newdata = test.data)
  
  #BMA-BMS
  residuals.bms <- test.data[,1] - predict(sim.model.BMS, newdata = test.data[-2])
  
  #
  
  
  return(list("var.iden" = list("truth" = var.model,
                                "stepwise" = var.iden.stepwise,
                                "anova" = var.iden.anova,
                                "bma" = var.iden.BMA,
                                "bms" = var.iden.BMS),
         "pred.error" = list("stepwise" = residuals.stepwise,
                             "anova" = residuals.anova,
                             "bma" = residuals.bma.median,
                             "bms" = residuals.bms),
         "coef.est" = list("stepwise" = coef.stepwise,
                           "anova" = coef.anova,
                           "bma" = coef.BMA,
                           "bms" = coef.BMS))
  )
  
}

checker <- function(identified.variables, true.variables){
  truePos <- sum(identified.variables %in% true.variables)
  falsePos <- sum(! identified.variables %in% true.variables)
  falseNeg <- sum(! true.variables %in% identified.variables)
  trueNeg <- 63 - truePos - falsePos - falseNeg
  print(paste("True positive:", truePos))
  print(paste("False positive:", falsePos))
  print(paste("False negative:", falseNeg))
  print(paste("True negative:", trueNeg))
  print(paste("Sensitivity:", round(truePos/(truePos+falseNeg),digits=2)))
  print(paste("Specificity:", round(trueNeg/(trueNeg + falsePos),digits=2)))
  return(c(truePos,falsePos,falseNeg,trueNeg))
}
