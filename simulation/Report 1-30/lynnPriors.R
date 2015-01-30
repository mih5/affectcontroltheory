priorscsv <- read.csv("~/GitHub/affectControlTheory/simulation/Report 1-30/priorscsv.csv")
mergeWith = data.frame(predictor = names(simulation.model.eae.3way)[-1])
mergedPriors <- merge(mergeWith, priorscsv, all = TRUE, sort = FALSE)

mergedPriors$predictor <- factor(mergedPriors$predictor, levels = mergeWith$predictor)
mergedPriors <- mergedPriors[order(mergedPriors$predictor),]
mergedPriors[is.na(mergedPriors)] <- 0.5
mergedPriors[mergedPriors == 1] <- 0.99
lynnPriors <- mergedPriors

