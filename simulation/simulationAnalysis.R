setwd("~/GitHub/affectControlTheory/simulation")
source("runSimulation.R")

#Model coefficients
#coefficients taken from Lynn's book
sim.B = c("intercept" = -0.098, 
          "ae" = 0.47, 
          "ap" = 0, 
          "aa" = 0, 
          "be" = 0.425, 
          "bp" = -0.069, 
          "ba" = -0.106, 
          "oe" = 0.055, 
          "op" = 0,
          "oa" = 0, 
          "ae.be" = 0.048,
          "ae.bp" = -0.038,
          "ae.ba" = 0,
          "ae.oe" = 0, 
          "ae.op" = 0, 
          "ae.oa" = 0, 
          "ap.be" = 0, 
          "ap.bp" = 0, 
          "ap.ba" = 0, 
          "ap.oe" = 0, 
          "ap.op" = 0, 
          "ap.oa" = 0, 
          "aa.be" = 0.048, 
          "aa.bp" = 0, 
          "aa.ba" = 0 , 
          "aa.oe" = 0, 
          "aa.op" = 0, 
          "aa.oa" = 0, 
          "be.oe" = 0, 
          "be.op" = -0.058, 
          "be.oa" = 0, 
          "bp.oe" = -0.07, 
          "bp.op" = 0, 
          "bp.oa" = 0, 
          "ba.oe" = 0, 
          "ba.op" = 0, 
          "ba.oa" = 0, 
          "ae.be.oe" = 0, 
          "ae.be.op" = 0, 
          "ae.be.oa" = 0, 
          "ae.bp.oe" = 0, 
          "ae.bp.op" = 0, 
          "ae.bp.oa" = 0, 
          "ae.ba.oe" = 0, 
          "ae.ba.op" = 0, 
          "ae.ba.oa" = 0, 
          "ap.be.oe" = 0, 
          "ap.be.op" = 0, 
          "ap.be.oa" = 0, 
          "ap.bp.oe" = -0.05, 
          "ap.bp.op" = 0, 
          "ap.bp.oa" = 0, 
          "ap.ba.oe" = 0, 
          "ap.ba.op" = 0, 
          "ap.ba.oa" = 0, 
          "aa.be.oe" = 0, 
          "aa.be.op" = 0, 
          "aa.be.oa" = 0, 
          "aa.bp.oe" = 0, 
          "aa.bp.op" = 0, 
          "aa.bp.oa" = 0.05, 
          "aa.ba.oe" = 0, 
          "aa.ba.op" = 0, 
          "aa.ba.oa" = 0)

stuff <- runSimulation(sim.B, 1000, 1000)

identified <- stuff$var.iden

predicted <- stuff$pred.error




hist(predicted$stepwise,main="Predictive Error", xlim=c(-4,4))
hist(predicted$anova,main="Predictive Error", xlim=c(-4,4))
hist(predicted$bma,main="Predictive Error", xlim=c(-4,4))
hist(predicted$bms,main="Predictive Error", xlim=c(-4,4))

mean(predicted$stepwise^2)
mean(predicted$anova^2)
mean(predicted$bma^2)
mean(predicted$bms^2)




checker(identified$stepwise, identified$truth)
checker(identified$anova, identified$truth)
checker(identified$bma, identified$truth)
checker(identified$bms, identified$truth)

