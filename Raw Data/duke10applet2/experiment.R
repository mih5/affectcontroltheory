require(dplyr)



duke10 <- read.csv("duke10long.csv")


duke10 %>% group_by("Context")