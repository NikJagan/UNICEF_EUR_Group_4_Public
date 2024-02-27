data <- read.csv("/Users/marleenhaubitz/Documents/Erasmus University/DSMA/UNICEF/unicef_eur_group4/datasets/cleaned_joined_data.csv")
install.packages("devtools")
library(devtools) 
install_github("susanathey/causalTree") 
library(causalTree)
# Load the party package
library(party)

data <- data[, names(data) %notin% c("Year", "Postcode", "Week")] 

#CONDITIONAL INFERENCE TREE
ctree_model <- ctree(donation_count ~ ., 
										 data = data, 
										 controls = ctree_control(teststat = "quad", mincriterion = 0.95))

plot(ctree_model)


#CAUSAL TREE
data2 <- data
data2$pledge.dummy <- as.numeric(data$pledge_count != 0)
data2 <- data2[1:10000, ]

tree <- causalTree(donation_count ~ . - pledge_count, 
									 data = data2, treatment = data2$pledge.dummy,
									 split.Rule = "CT", cv.option = "CT", split.Honest = TRUE, cv.Honest = TRUE, split.Bucket = FALSE, 
									 xval = 5, cp = 0, minsize = 20, propensity = 0.5)

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
opfit <- prune(tree, opcp)
rpart.plot(opfit)

#UNPRUNED TREE
rpart.plot(tree)

