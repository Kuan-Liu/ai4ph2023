library(tidyverse)
library(learnr)
library(grf)
options(scipen = 999)

# Data prep;
data <- read.csv("data/rhc.csv", header=T)
# define exposure variable
data$A <- ifelse(data$swang1 =="No RHC", 0, 1)
# outcome is dth30, a binary outcome measuring survival status at day 30;
data$Y <- ifelse(data$dth30 =="No", 0, 1)
data2 <- select(data, -c(cat2, adld3p, urin1, swang1,
                         sadmdte, dschdte, dthdte, lstctdte, death, dth30,
                         surv2md1, das2d3pc, t3d30, ptid))
data2 <- rename(data2, id = X)
covariates <- select(data2, -c(id, A, Y))
# Potential subgroups of interest;
table(data2$race)
table(data2$income)
table(data2$cat1)

# getting matrix for causal forest;
X <- covariates %>%  model.matrix(~., .)
X <- X[ , -1]
Y <- data2$Y
W <- data2$A

set.seed(123)
cforest <- causal_forest(
  X,
  Y,
  W,
  num.trees = 4000,
  sample.weights = NULL,
  clusters = NULL,
  equalize.cluster.weights = FALSE,
  sample.fraction = 0.5,
  mtry = min(ceiling(sqrt(ncol(X)) + 20), ncol(X)),
  min.node.size = 10,
  honesty = TRUE,
  honesty.fraction = 0.5,
  honesty.prune.leaves = TRUE,
  alpha = 0.05,
  imbalance.penalty = 0,
  stabilize.splits = TRUE,
  ci.group.size = 2,
  tune.parameters = "none",
  compute.oob.predictions = TRUE
)


cforest

# which variables appear to make a difference for treatment effects? HTE?
# we can inspect variable_importance, which measures how often a variable Xj was split on.;
varimp <- variable_importance(cforest)
ranked.vars <- order(varimp, decreasing = TRUE)

# Top 5 variables according to this measure
colnames(X)[ranked.vars[1:5]]

#the first tree;
plot(get_tree(cforest, index=1))
#the 30th tree;
plot(get_tree(cforest, index=30))

# estimate ATE;
average_treatment_effect(cforest, target.sample = "all")

# estimate ATT;
average_treatment_effect(cforest, target.sample = "treated")

# test whether there is HTE;
test_calibration(cforest)
#We can reject the null of no heterogeneity;

# Conditional ATE for race
#white;
average_treatment_effect(cforest, target.sample = "all", subset = X[ , "racewhite"] == 1)
#other;
average_treatment_effect(cforest, target.sample = "all", subset = X[ , "raceother"] == 1)
#black;
average_treatment_effect(cforest, target.sample = "all", subset = X[ , "raceother"] == 0 & X[ , "racewhite"] == 0)

