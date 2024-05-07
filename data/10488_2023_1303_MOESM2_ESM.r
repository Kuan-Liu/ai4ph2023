################################################################################
# A tutorial introduction to heterogeneous treatment effect estimation
# with meta-learners
# 
# Administration and Policy and Mental Health Services Research
#
# Marie Salditt, Theresa Eckes, and Steffen Nestler (University of Muenster)
# e-mail address of corresponding author: msalditt@uni-muenster.de
#
#
# Code Part I:
# This file contains the code to reproduce the main results
# of the illustrative data example
################################################################################

#--------------------------------------------------------------------------------
# PREPARE WORKSPACE, LOAD NECESSARY PACKAGES AND LOAD DATA 
#--------------------------------------------------------------------------------

# Modeling packages
library(ranger) # random forest

# Helper packages
library(psych) # descriptive statistics and histogram plot
library(cobalt) # balance diagnostics
library(groupdata2) # data handling
library(dplyr) # data handling
library(plyr)  # data handling
library(tictoc) # check computation time
library(xtable) # to create LaTeX code for tables

# Packages used for creating plots
library(ggplot2) 
library(gtable)
library(ggpubr)
library(gridExtra )

rm(list = ls())

setwd("...") # set working directory
dfs <- read.table("AddHealthExample_imputed.txt", header = T) 
dfs$A_factor <- factor(dfs$A, levels = c(0,1), labels = c("Untreated", "Treated"))
# ranger needs dichotomous outcome variables to be a factor variable to fit classification rather than regression forests

#- set seed for reproducibility
seed <- 123
set.seed(123)


#--------------------------------------------------------------------------------
# FURTHER PREPARATIONS 
#--------------------------------------------------------------------------------

#- define conditional mean models
mu_model <- Y ~ A + Age + Sex + Hispanic + Black + Asian + Native.American + OtherRace + 
  ParentalEducation + EnoughMoney + Income + Health + 
  Intelligence + SelfEsteem + DecisionMaking + ProblemAvoidance + 
  AlcoholUse + TeamSports + Excercise + TimeWithFriends + HoursVideosWeekly + 
  ActivitiesWithParent + ParentalCloseness + FamilySupport + 
  AttemptedSuicideInFamily + CompletedSuicideInFamily + FriendAttemptedSuicide + 
  FriendCompletedSuicide + SuicidalThoughts + OneAttemptedSuicide + 
  AtLeastTwoAttemptedSuicides + PriorTreatment + CES_D 

m_model <- Y ~ Age + Sex + Hispanic + Black + Asian + Native.American + OtherRace + 
  ParentalEducation + EnoughMoney + Income + Health + 
  Intelligence + SelfEsteem + DecisionMaking + ProblemAvoidance + 
  AlcoholUse + TeamSports + Excercise + TimeWithFriends + HoursVideosWeekly + 
  ActivitiesWithParent + ParentalCloseness + FamilySupport + 
  AttemptedSuicideInFamily + CompletedSuicideInFamily + FriendAttemptedSuicide + 
  FriendCompletedSuicide + SuicidalThoughts + OneAttemptedSuicide + 
  AtLeastTwoAttemptedSuicides + PriorTreatment + CES_D 

#- define propensity score model
ps_model <- A_factor ~ Age + Sex + Hispanic + Black + Asian + Native.American + OtherRace + 
  ParentalEducation + EnoughMoney + Income + Health + 
  Intelligence + SelfEsteem + DecisionMaking + ProblemAvoidance + 
  AlcoholUse + TeamSports + Excercise + TimeWithFriends + HoursVideosWeekly + 
  ActivitiesWithParent + ParentalCloseness + FamilySupport + 
  AttemptedSuicideInFamily + CompletedSuicideInFamily + FriendAttemptedSuicide + 
  FriendCompletedSuicide + SuicidalThoughts + OneAttemptedSuicide + 
  AtLeastTwoAttemptedSuicides + PriorTreatment + CES_D 

# this will be handy for some computations later
Y <- dfs$Y
A <- dfs$A
covariateNames <- labels(terms(ps_model))
X <- dfs[, covariateNames] 
n <- nrow(dfs)
n_features <- length(covariateNames)

#- set settings for the random forests

# number of trees in the random forest
num.trees <-  n_features * 10

# create hyperparameter grid for tuning
hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10),
  replace = c(TRUE, FALSE),
  sample.fraction = c(.5, .63, .8, 1),
  error = NA
)
hyper_grid <- hyper_grid[!(hyper_grid$replace == FALSE & hyper_grid$sample.fraction == 1), ]


#--------------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS AND INITIAL IMBALANCE
#--------------------------------------------------------------------------------
sum(dfs$A) # 353
sum(dfs$A==0) # 3491

# some summary statistics 
desStats <- psych::describeBy(dfs, group = "A")
desStats

# inspect initial unbalance: mean differences of covariates between treatment groups
bal.unadj <- cobalt::bal.tab( x = X,
                              treat = A,
                              method = "weighting", continuous = "std", s.d.denom = "pooled",
                              binary = "raw", stats = c("mean.diffs"))


#- create covariate imbalance plot
oldNames <- var.names(bal.unadj)
dput(oldNames)
newNames <- c(Age = "Age", Sex = "Sex", Hispanic = "Hispanic", 
              Black = "Black", Asian = "Asian", Native.American = "Native American", 
              OtherRace = "Other Race", ParentalEducation = "Parental Education", 
              EnoughMoney = "Enough Money", Income = "Income", Health = "Health", 
              Intelligence = "Intelligence", SelfEsteem = "Self-esteem", DecisionMaking = "Decision making", 
              ProblemAvoidance = "Problem avoidance", AlcoholUse = "Alcohol use", 
              TeamSports = "Teamsports", Excercise = "Excercise", TimeWithFriends = "Time with friends", 
              HoursVideosWeekly = "Video hours per week", ActivitiesWithParent = "Parental involvement", 
              ParentalCloseness = "Parental closeness", FamilySupport = "Family support", 
              AttemptedSuicideInFamily = "Attempted suicide in family", 
              CompletedSuicideInFamily = "Completed suicide in family", 
              FriendAttemptedSuicide = "Friend attempted suicide",
              FriendCompletedSuicide = "Friend completed suicide", 
              SuicidalThoughts = "Suicidal thoughts", OneAttemptedSuicide = "One attempted suicide", 
              AtLeastTwoAttemptedSuicides = ">= 2 attempted suicides", 
              PriorTreatment = "Prior treatment", CES_D = "Prior CES-D")
varNames <- data.frame(old = oldNames, new = newNames)

lp <- cobalt::love.plot( x = X,
                         treat = A, abs = F, 
                         method = "weighting", continuous = "std", s.d.denom = "pooled",
                         binary = "raw", stats = "mean.diffs", stars = "raw",
                         var.names = varNames,
                         position = "none",
                         thresholds = c(m = .1))
# stars indicate that raw mean differences (rather than standardized mean differences)
# are shown, which is the case for all dichotomous variables
# png("BalancePlot.png", width = 6.8, height = 5.7, units = "in",
#     res = 400)
# plot(lp)
# dev.off()

#- plot pairwise correlations between variables
# https://briatte.github.io/ggcorr/
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
cp <- ggcorr(data = dfs[,!(colnames(dfs) %in% "A_factor")],  geom = "blank", label = TRUE, hjust = 1,
              layout.exp = 1, label_size = 3.5) +
  geom_point(size =8, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.05)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = "none", alpha = "none")
# png("CorrelationsPlot.png", width = 9.8, height = 8.7, units = "in",
#     res = 400)
# plot(cp)
# dev.off()

#- save correlation plot and imbalance plot together in one plot
ds_plot <- ggarrange(cp, lp, ncol = 2)
ds_plot
# ggsave("BalanceAndCorrplot.png", ds_plot, width = 15.6, height = 7.3, dpi = 400)


#--------------------------------------------------------------------------------
# ESTIMATE THE PROPENSITY SCORE
#--------------------------------------------------------------------------------
# fit a random forest with default setting
ps_fit_default <- ranger::ranger(formula = ps_model,
                                 data = dfs,
                                 probability = TRUE,
                                 seed = seed)
default_error <- ps_fit_default$prediction.error

# execute grid search for hyperparameter tuning
library(tictoc)
tictoc::tic("Tuning PS model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
  ps_fit <- ranger::ranger(
    formula = ps_model,
    data = dfs,
    probability = TRUE,
    num.trees = num.trees, 
    mtry = hyper_grid$mtry[k],
    min.node.size = hyper_grid$min.node.size[k],
    replace = hyper_grid$replace[k],
    sample.fraction = hyper_grid$sample.fraction[k],
    verbose = TRUE,
    seed = seed
  )
  # save OOB error
  hyper_grid$error[k] <- ps_fit$prediction.error
}
tictoc::toc() 

# calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
res_ps_hyperparams <- hyper_grid %>%
    arrange(error) %>%
    mutate(perc_gain = (default_error - error) / default_error * 100) %>%
    head(10)
res_ps_hyperparams
# the improvement of the best model over the default model is only .25%

# choose the best performing hyperparameter values and rerun the the random forest
# with these values
ps_hyperparams <- res_ps_hyperparams[1, ]

ps_fit <- ranger::ranger(
  formula = ps_model,
  data = dfs,
  probability = TRUE,
  num.trees = num.trees,
  mtry = ps_hyperparams$mtry,
  min.node.size = ps_hyperparams$min.node.size,
  replace = ps_hyperparams$replace,
  sample.fraction = ps_hyperparams$sample.fraction,
  verbose = TRUE,
  seed = seed
)

ps_hat <- ps_fit$predictions[,2] # get the OOB predictions

# check the positivity assumption
round(range(ps_hat), 3) #    0.000 0.679
quantile(ps_hat)
# 0%        25%        50%        75%       100% 
# 0.00000000 0.02868741 0.05768899 0.11838350 0.67850099 

# ensure positivity: add/substract a small threshold to estimated propensity scores equal to zero/one
epsilon <- .01
ps_hat <- ifelse(ps_hat < epsilon, epsilon,
                 ifelse(ps_hat > 1-epsilon, 1-epsilon, ps_hat))
quantile(ps_hat)
# 0%        25%        50%        75%       100% 
# 0.01000000 0.02868741 0.05768899 0.11838350 0.67850099 
# Note that the PS distribution seems to be only minimally affected by this
# transformation.


#--------------------------------------------------------------------------------
# METALEARNERS WITHOUT CROSSFITTING
#--------------------------------------------------------------------------------

# create empty matrix to save descriptive statistics of the estimated ITEs
res <- matrix(0, nrow = 5, ncol = 7)
LearnerNames <- c("T-Learner", "S-Learner",
  "X-Learner", "DR-Learner", "R-Learner")
rownames(res) <- LearnerNames
colnames(res) <- c("Mean", "SD", "Min", "25%", "Median", "75%", "Max")



# T-Learner
#--------------------------------------------------------------------------------

#- Split the data into treatment and control groups
dfs0 <- dfs[dfs$A == 0, ]
dfs1 <- dfs[dfs$A == 1, ]

#- Train a regression model using the control group
#------------------------------------------------------

hyper_grid$error <- NA
# execute grid search for hyperparameter tuning
tictoc::tic("Tuning mu0 model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
    mu0_fit <- ranger::ranger(formula = m_model,
    data = dfs0,
    num.trees = num.trees, 
    mtry = hyper_grid$mtry[k],
    min.node.size = hyper_grid$min.node.size[k],
    replace = hyper_grid$replace[k],
    sample.fraction = hyper_grid$sample.fraction[k],
    verbose = TRUE,
    seed = seed
  )
  # save OOB error
  hyper_grid$error[k] <- mu0_fit$prediction.error
}
tictoc::toc() 

# calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
mu0_fit_default <- ranger::ranger( formula = m_model, data = dfs0, 
                                   keep.inbag = TRUE, seed = seed )
default_error <- mu0_fit_default$prediction.error
res_mu0_hyperparams <- hyper_grid %>%
  arrange(error) %>%
  mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10)
res_mu0_hyperparams
# the improvement of the best 10 models over the default model ranges from 0.6% to 1%

# choose the best performing hyperparameter values and rerun the the random forest
# with these values
mu0_hyperparams <- res_mu0_hyperparams[1, ]
mu0_fit <- ranger::ranger( formula = m_model, data = dfs0, keep.inbag = TRUE,
                           seed = seed, num.trees = num.trees,
                           mtry = mu0_hyperparams$mtry,
                           min.node.size = mu0_hyperparams$min.node.size,
                           replace = mu0_hyperparams$replace,
                           sample.fraction = mu0_hyperparams$sample.fraction,
                           verbose = TRUE )

# Obtain predictions for mu_0, use OOB predictions where applicable
mu0_hat <- rep(0, n)
mu0_hat[A==0] <- mu0_fit$predictions # OOB predictions 
mu0_hat[A==1] <- predict(mu0_fit, dfs1)$predictions


#- Train a regression model using the treatment group
#------------------------------------------------------

hyper_grid$error <- NA
# execute grid search for hyperparameter tuning
library(tictoc)
tictoc::tic("Tuning mu1 model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
  mu1_fit <- ranger::ranger( formula = m_model,
                             data = dfs1,
                             num.trees = num.trees, 
                             mtry = hyper_grid$mtry[k],
                             min.node.size = hyper_grid$min.node.size[k],
                             replace = hyper_grid$replace[k],
                             sample.fraction = hyper_grid$sample.fraction[k],
                             verbose = TRUE,
                             seed = seed )
  # save OOB error
  hyper_grid$error[k] <- mu1_fit$prediction.error
}
tictoc::toc() 

# calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
mu1_fit_default <- ranger::ranger( formula = m_model, data = dfs1, 
                                   keep.inbag = TRUE, seed = seed )
default_error <- mu1_fit_default$prediction.error
res_mu1_hyperparams <- hyper_grid %>%
  arrange(error) %>%
  mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10)
res_mu1_hyperparams
# the improvement of the best 10 models over the default model ranges from 0.6% to 1.4% 


# choose the best performing hyperparameter values and rerun the the random forest
# with these values
mu1_hyperparams <- res_mu1_hyperparams[1, ]
mu1_fit <- ranger::ranger( formula = m_model, data = dfs1, keep.inbag = TRUE,
                           seed = seed, num.trees = num.trees,
                           mtry = mu1_hyperparams$mtry,
                           min.node.size = mu1_hyperparams$min.node.size,
                           replace = mu1_hyperparams$replace,
                           sample.fraction = mu1_hyperparams$sample.fraction,
                           verbose = TRUE )

#- Obtain predictions for mu_1
mu1_hat <- rep(0, n)
mu1_hat[A==1] <- mu1_fit$predictions # OOB predictions 
mu1_hat[A==0] <- predict(mu1_fit, dfs0)$predictions

#- Compute the CATE as the difference between the models' predictions
cate_t <- mu1_hat - mu0_hat


#- Summarize results
hist(cate_t, main  = "T-Learner")
res[1, ] <- c(mean(cate_t), sd(cate_t), quantile(cate_t))



# S-Learner
#--------------------------------------------------------------------------------

#- Train a regression model including the treatment variable as covariate

# execute grid search
hyper_grid$error <- NA
tictoc::tic("Tuning mu model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
  mu_fit <- ranger::ranger( formula = mu_model,
                            data = dfs,
                            num.trees = num.trees, 
                            mtry = hyper_grid$mtry[k],
                            min.node.size = hyper_grid$min.node.size[k],
                            replace = hyper_grid$replace[k],
                            sample.fraction = hyper_grid$sample.fraction[k],
                            verbose = TRUE,
                            seed = seed
  )
  # save OOB error
  hyper_grid$error[k] <- mu_fit$prediction.error
}
tictoc::toc()

# calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
mu_fit_default <- ranger::ranger( formula = mu_model, data = dfs, 
                                  keep.inbag = TRUE, seed = seed ) # default model
default_error <- mu_fit_default$prediction.error
res_mu_hyperparams <- hyper_grid %>%
  arrange(error) %>%
  mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10)
res_mu_hyperparams
# the improvement of the best model ranges between 0.63$ and 1.3%

# choose the best performing hyperparameter values and rerun the the random forest
# with these values
mu_hyperparams <- res_mu_hyperparams[1, ]
mu_fit <- ranger::ranger( formula = mu_model, data = dfs, keep.inbag = TRUE,
                          seed = seed, num.trees = num.trees,
                          mtry = mu_hyperparams$mtry,
                          min.node.size = mu_hyperparams$min.node.size,
                          replace = mu_hyperparams$replace,
                          sample.fraction = mu_hyperparams$sample.fraction,
                          verbose = TRUE )

#- Predict mu_0 by setting A = 0 for all persons, use OOB predictions where applicable
dfsTMP <- dfs
dfsTMP$A <- 0
mu0_hat_s <- rep(0, n)
mu0_hat_s[A==0] <- mu_fit$predictions[A==0] # OOB predictions 
mu0_hat_s[A==1] <- predict(mu_fit, dfsTMP)$predictions[A==1] 

#- Predict mu_1 by setting A = 1 for all persons, use OOB predictions where applicable
dfsTMP$A <- 1 
mu1_hat_s <- rep(0, n)
mu1_hat_s[A==1] <- mu_fit$predictions[A==1] # OOB predictions 
mu1_hat_s[A==0] <- predict(mu_fit, dfsTMP)$predictions[A==0] 

#- Estimate the CATE as the difference between the predictions by treatment status
cate_s <- mu1_hat_s - mu0_hat_s

#- Summarize results
hist(cate_s, main  = "S-Learner")
res[2, ] <- c(mean(cate_s), sd(cate_s), quantile(cate_s))



# X-Learner
#--------------------------------------------------------------------------------

# We already estimated the conditional mean functions separately in the treatment
# groups in the T-Learner, and we already estimated the propensity score.


#- Now we compute the pseudo-outcome, i.e., the
# 'imputed treatment effects' using the estimated
# conditional mean function from the respective other group
dfs0$psi_x_0 <- predict(mu1_fit, dfs0)$predictions - dfs0$Y
dfs1$psi_x_1 <- dfs1$Y - predict(mu0_fit, dfs1)$predictions

#- Regress the pseudo-outcome on the covariates separately in the treatment groups

# Untreated group
#------------------
m_xlearner_0 <- update(ps_model, psi_x_0 ~ .)

#- execute grid search
hyper_grid$error <- NA
tictoc::tic("Tuning tau_0 model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
  tau_x_0_fit <- ranger::ranger( formula = m_xlearner_0,
                                 data = dfs0,
                                 num.trees = num.trees, 
                                 mtry = hyper_grid$mtry[k],
                                 min.node.size = hyper_grid$min.node.size[k],
                                 replace = hyper_grid$replace[k],
                                 sample.fraction = hyper_grid$sample.fraction[k],
                                 verbose = TRUE,
                                 seed = seed )
  # save OOB error
  hyper_grid$error[k] <-  tau_x_0_fit$prediction.error
}
tictoc::toc() 

#- calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
tau_x_0_fit_default <- ranger::ranger( formula = m_xlearner_0, data = dfs0, 
                                       keep.inbag = TRUE, seed = seed )
default_error <- tau_x_0_fit_default$prediction.error
res_tau_x_0_hyperparams <- hyper_grid %>%
  arrange(error) %>%
  mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10)
res_tau_x_0_hyperparams
# the improvement of the best model over the default model is 1%, the rest below 0.5%

#- choose the best performing hyperparameter values and rerun the the random forest
# with these values
tau_x_0_hyperparams <- res_tau_x_0_hyperparams[1, ]
tau_x_0_fit <- ranger::ranger( formula = m_xlearner_0, data = dfs0, keep.inbag = TRUE,
                          seed = seed, num.trees = num.trees,
                          mtry = tau_x_0_hyperparams$mtry,
                          min.node.size = tau_x_0_hyperparams$min.node.size,
                          replace = tau_x_0_hyperparams$replace,
                          sample.fraction = tau_x_0_hyperparams$sample.fraction,
                          verbose = TRUE )


# Treated group
#------------------
m_xlearner_1 <- update(ps_model, psi_x_1 ~ .)
hyper_grid$error <- NA
tictoc::tic("Tuning tau_1 model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
  tau_x_1_fit <- ranger::ranger( formula = m_xlearner_1,
                                 data = dfs1,
                                 num.trees = num.trees, 
                                 mtry = hyper_grid$mtry[k],
                                 min.node.size = hyper_grid$min.node.size[k],
                                 replace = hyper_grid$replace[k],
                                 sample.fraction = hyper_grid$sample.fraction[k],
                                 verbose = TRUE,
                                 seed = seed )
  # save OOB error
  hyper_grid$error[k] <-  tau_x_1_fit$prediction.error
}
tictoc::toc() 

# calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
tau_x_1_fit_default <- ranger::ranger( formula = m_xlearner_1, data = dfs1, 
                                       keep.inbag = TRUE, seed = seed )
default_error <- tau_x_1_fit_default$prediction.error
res_tau_x_1_hyperparams <- hyper_grid %>%
  arrange(error) %>%
  mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10)
res_tau_x_1_hyperparams
# the improvement of the best 10 models over the default model ranges between 1.4 and 3.9%


# choose the best performing hyperparameter values and rerun the the random forest
# with these values
tau_x_1_hyperparams <- res_tau_x_1_hyperparams[1, ]
tau_x_1_fit <- ranger::ranger( formula = m_xlearner_1, data = dfs1, keep.inbag = TRUE,
                               seed = seed, num.trees = num.trees,
                               mtry = tau_x_1_hyperparams$mtry,
                               min.node.size = tau_x_1_hyperparams$min.node.size,
                               replace = tau_x_1_hyperparams$replace,
                               sample.fraction = tau_x_1_hyperparams$sample.fraction,
                               verbose = TRUE )

#- Predict treatment effects per group using the two estimated pseudo-outcome regressions,
# use OOB predictions where applicable
tau_x_0_hat <- rep(0, n)
tau_x_0_hat[A==0] <- tau_x_0_fit$predictions
tau_x_0_hat[A==1] <- predict(tau_x_0_fit, dfs1)$predictions
tau_x_1_hat <- rep(0, n)
tau_x_1_hat[A==1] <- tau_x_1_fit$predictions 
tau_x_1_hat[A==0] <- predict(tau_x_1_fit, dfs0)$predictions
                                                                  

#- Compute the CATE as weighted combination of tau0 and tau1,
# using the estimated propensity score
cate_x <- ps_hat*tau_x_0_hat + (1-ps_hat)*tau_x_1_hat

#- Summarize results
hist(cate_x, main  = "X-Learner")
res[3, ] <- c(mean(cate_x), sd(cate_x), quantile(cate_x))



# DR-Learner 
#--------------------------------------------------------------------------------

# We already estimated the conditional mean functions in the T-Learner,
# and we already estimated the propensity score.

#- Compute the pseudo-outcome of the DR-learner
augmentedTerm <- 1/ps_hat * (A * (Y - mu1_hat)) - 
  1/(1-ps_hat)* ( (1-A) * (Y - mu0_hat) )
dfs$psi_dr <- mu1_hat - mu0_hat + augmentedTerm

#- Regress the pseudo-outcome on the covariates
m_drlearner <- update(ps_model, psi_dr ~ .) # make psi_r the outcome in the model

hyper_grid$error <- NA
tictoc::tic("Tuning tau_dr model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
  tau_dr_fit <- ranger::ranger( formula = m_drlearner,
                                data = dfs,
                                num.trees = num.trees, 
                                mtry = hyper_grid$mtry[k],
                                min.node.size = hyper_grid$min.node.size[k],
                                replace = hyper_grid$replace[k],
                                sample.fraction = hyper_grid$sample.fraction[k],
                                verbose = TRUE,
                                seed = seed )
  # save OOB error
  hyper_grid$error[k] <-  tau_dr_fit$prediction.error
}
tictoc::toc() 

# calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
tau_dr_fit_default <- ranger::ranger( formula = m_drlearner, data = dfs, 
                                      keep.inbag = TRUE, seed = seed )
default_error <- tau_dr_fit_default$prediction.error
res_tau_dr_hyperparams <- hyper_grid %>%
  arrange(error) %>%
  mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10)
res_tau_dr_hyperparams
# the improvement of the best 10 models over the default model is around 1.7%

# choose the best performing hyperparameter values and rerun the the random forest
# with these values
tau_dr_hyperparams <- res_tau_dr_hyperparams[1, ]
tau_dr_fit <- ranger::ranger( formula = m_drlearner, data = dfs, keep.inbag = TRUE,
                              seed = seed, num.trees = num.trees,
                              mtry = tau_dr_hyperparams$mtry,
                              min.node.size = tau_dr_hyperparams$min.node.size,
                              replace = tau_dr_hyperparams$replace,
                              sample.fraction = tau_dr_hyperparams$sample.fraction,
                              verbose = TRUE )

#- Compute the CATE as the predictions from the pseudo-outcome regression,
# use OOB predictions 
cate_dr <- tau_dr_fit$predictions

#- Summarize results
hist(cate_dr, main  = "DR-Learner")
res[4, ] <- c(mean(cate_dr), sd(cate_dr), quantile(cate_dr))


#--------------------------------------------------------------------------------
# Additional code not used in the analysis:
# Compute CATE with confidence intervals when OLS regression
# is used as final base learner 

tau_dr_fit.glm <-  glm(m_drlearner, family = gaussian(), data = dfs)
pred_dr.glm <- predict(tau_dr_fit.glm, se.fit = T)
cate_dr.glm <- pred_dr.glm$fit
cor(cate_dr, cate_dr.glm) # 0.3766835
critval <- qnorm(0.975)
upr <- cate_dr.glm + (critval * pred_dr.glm$se.fit) # upper CI limit
lwr <- cate_dr.glm - (critval * pred_dr.glm$se.fit) # lower CI limit

# Create plot of sorted treatment effect estimates with 95% confidence intervals 
dfs_dr.glm.wide <- cbind.data.frame(ITE = cate_dr.glm, CL = lwr, CU = upr) # predicted ITEs with confidence intervals
dfs_dr.glm <- reshape2::melt(dfs_dr.glm.wide[order(dfs_dr.glm.wide$ITE),])
dfs_dr.glm$ID <- rep(1:n, times = 3)
ggplot2::ggplot(dfs_dr.glm, aes(x = ID,y = value, group = variable) ) +
  geom_line(aes(color=variable, alpha = variable)) +
  scale_alpha_manual(values=c(1.0,0.5 ,0.5)) +
  theme_bw() +
  labs(y = "Estimated Individual Treatment Effect", x = "Sorted Observation") +
  theme(legend.position="none"  ) + 
  scale_color_manual(values = c("blue", "lightblue", "lightblue")) +
  geom_smooth(data=dfs_dr.glm[dfs_dr.glm["variable"] == "CL" | dfs_dr.glm["variable"] == "CU",],
              linetype="dashed", linewidth=0.5) 
#--------------------------------------------------------------------------------




# R-Learner 
#--------------------------------------------------------------------------------

#- Train a regression model for m(X) = E(Y|X) and obtain OOB predictions

hyper_grid$error <- NA
library(tictoc)
tictoc::tic("Tuning m model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
  m_fit <- ranger::ranger(formula = m_model,
                          data = dfs,
                          num.trees = num.trees, 
                          mtry = hyper_grid$mtry[k],
                          min.node.size = hyper_grid$min.node.size[k],
                          replace = hyper_grid$replace[k],
                          sample.fraction = hyper_grid$sample.fraction[k],
                          verbose = TRUE,
                          seed = seed
  )
  # save OOB error
  hyper_grid$error[k] <- m_fit$prediction.error
}
tictoc::toc() 

# calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
m_fit_default <- ranger::ranger( formula = m_model, data = dfs, 
                                 keep.inbag = TRUE, seed = seed)
default_error <- m_fit_default$prediction.error
res_m_hyperparams <- hyper_grid %>%
  arrange(error) %>%
  mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10)
res_m_hyperparams
# the improvement of the best model over the default model is 1.1%,
# the rest below 1% 

# choose the best performing hyperparameter values and rerun the the random forest
# with these values
m_hyperparams <- res_m_hyperparams[1, ]
m_fit <- ranger::ranger( formula = m_model, data = dfs, keep.inbag = TRUE,
                         seed = seed, num.trees = num.trees,
                         mtry = m_hyperparams$mtry,
                         min.node.size = m_hyperparams$min.node.size,
                         replace = m_hyperparams$replace,
                         sample.fraction = m_hyperparams$sample.fraction,
                         verbose = TRUE )

m_hat <- m_fit$predictions

#- Compute the pseudo-outcome of the R-learner
resid_treat <- A - ps_hat
resid_out <- Y - m_hat
psi_r <- resid_out / resid_treat
dfs$psi_r <- psi_r

#- Compute weights
w <- resid_treat^2 

#- Regress pseudo-outcome on covariates using weights w
m_rlearner <- update(ps_model, psi_r ~ .) # make psi_r the outcome in the model

hyper_grid$error <- NA
library(tictoc)
tictoc::tic("Tuning tau_r model")
for (k in seq_len(nrow(hyper_grid))) {
  # fit model for the k-th hyperparameter combination 
  tau_r_fit <- ranger::ranger(  formula = m_rlearner, data = dfs,
                                case.weights = w,
                                num.trees = num.trees, 
                                mtry = hyper_grid$mtry[k],
                                min.node.size = hyper_grid$min.node.size[k],
                                replace = hyper_grid$replace[k],
                                sample.fraction = hyper_grid$sample.fraction[k],
                                verbose = TRUE,
                                seed = seed )
  # save OOB error
  hyper_grid$error[k] <-  tau_r_fit$prediction.error
}
tictoc::toc()

# calculate percentage improvement in the prediction error
# compared to default random forest and assess top 10 models
tau_r_fit_default <- ranger::ranger( formula = m_rlearner, data = dfs,
                                     case.weights = w, keep.inbag = TRUE, seed = seed)
default_error <- tau_r_fit_default$prediction.error
res_tau_r_hyperparams <- hyper_grid %>%
  arrange(error) %>%
  mutate(perc_gain = (default_error - error) / default_error * 100) %>%
  head(10)
res_tau_r_hyperparams
# the improvement of the best 10 models over the default model is around 3.5%

# choose the best performing hyperparameter values and rerun the the random forest
# with these values
tau_r_hyperparams <- res_tau_r_hyperparams[1, ]
tau_r_fit <- ranger::ranger( formula = m_rlearner, data = dfs,
                             case.weights = w, keep.inbag = TRUE,
                             seed = seed, num.trees = num.trees,
                             mtry = tau_r_hyperparams$mtry,
                             min.node.size = tau_r_hyperparams$min.node.size,
                             replace = tau_r_hyperparams$replace,
                             sample.fraction = tau_r_hyperparams$sample.fraction,
                             verbose = TRUE )

#- Compute the CATE as the predictions from the weighted pseudo-outcome regression,
# use OOB predictions
cate_r <- tau_r_fit$predictions
# Observations with large weights will more often be selected into the bootstrap samples
# of the single trees. If some observations have very large weights,
# this can have the effect that they are ALWAYS sampled, that is,
# they are used for training every single tree in the random forest.
# Thus, for such observations one cannot compute an OOB prediction.
# For these observations, we use the normal random forest predictions:
NaNIDX <- which(is.nan(cate_r))
length(NaNIDX) # 85 observations
cate_r[NaNIDX] <- predict(tau_r_fit, X)$predictions[NaNIDX]

#- Summarize results
hist(cate_r, main  = "R-Learner")
res[5, ] <- c(mean(cate_r), sd(cate_r), quantile(cate_r))



#--------------------------------------------------------------------------------
# SUMMARIZE RESULTS FOR ALL META-LEARNERS
#--------------------------------------------------------------------------------
round(res, 2)
#            Mean   SD    Min   25% Median  75%   Max
# T-Learner  1.07 1.12  -3.19  0.31   0.98 1.75  6.82
# S-Learner  0.47 0.93  -3.89 -0.10   0.36 0.95  6.48
# X-Learner  0.74 0.92  -1.64  0.10   0.60 1.25  5.19
# DR-Learner 0.75 1.25  -6.97  0.17   0.69 1.26 11.38
# R-Learner  0.75 0.94  -8.10  0.38   0.79 1.16 16.04


# Create LaTeX table 
#--------------------------------------
print( xtable::xtable(res,
                      caption = "Descriptive statistics of the individual treatment effects as estimated by the different meta-learners",
                      label =  "tab:CATE", align = c("l", rep("c", 7)),
                      digits = 2, booktabs=TRUE ) )
# table code can be customized in LaTeX as desired 


#- Plot histograms and pairwise correlations
#---------------------------------------------
res_cates <- cbind.data.frame(cate_t, cate_s, cate_x, cate_dr, cate_r)
colnames(res_cates) <- LearnerNames
round( cor(res_cates), 2)
#            T-Learner S-Learner X-Learner DR-Learner R-Learner
# T-Learner       1.00      0.17      0.73       0.34      0.29
# S-Learner       0.17      1.00      0.13       0.07      0.07
# X-Learner       0.73      0.13      1.00       0.37      0.36
# DR-Learner      0.34      0.07      0.37       1.00      0.22
# R-Learner       0.29      0.07      0.36       0.22      1.00

# Plot histograms and pairwise correlations
# png("HistCorPlot.png", width = 8.5, height = 7, units = "in",
#     res = 400)
# psych::pairs.panels(res_cates, breaks = 30)
# dev.off()


## Save results
# write.table(res, "DesStats_CATE.txt", col.names = T)
# write.table(res_cates, "EstimatedCATE.txt", col.names = T)