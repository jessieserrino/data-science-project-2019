source("team_helper_functions.r")

data <- read.csv("output/hclust.csv", na.strings=c(""," ","NA"), header=TRUE) # Loading data
table(data$cluster, data$Is_Resigning)
table(data$cluster, data$JobLevel)
table(data$cluster, data$Gender)

