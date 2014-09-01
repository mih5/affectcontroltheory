###################################
# EXPLORATORY ANALYSIS OF DUKE 10 #
###################################

require(reshape2)
require(ggplot2)
require(dplyr)

#The cleaned duke 10 data
duke10 <- read.csv("~/GitHub/affectControlTheory/Raw Data/Duke 2010 ENGLISH - FINAL.csv")

head(duke10)

#examine covariates/demographic variables

p <- ggplot(duke10, aes(x = ia42))
p + geom_density(aes(stroke = factor(sex), color = factor(sex)))

