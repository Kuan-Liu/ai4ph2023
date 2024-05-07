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
# Code Part II:
# This file contains the code to reproduce the heterogeneity analysis
# of the illustrative data example using a 2-step cross-fitting version
# of the X-learner
################################################################################


#--------------------------------------------------------------------------------
# PREPARE WORKSPACE, LOAD NECESSARY PACKAGES AND LOAD DATA 
#--------------------------------------------------------------------------------

# Modeling packages
library(ranger) # random forest

# Helper packages
library(groupdata2) # data handling
library(dplyr) # data handling
library(plyr)  # data handling
library(estimatr) # robust standard errors
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

#- results from full-sample versions
res <- read.table("DesStats_CATE.txt", header= T)
res_cates <- read.table("EstimatedCATE.txt", header = T)


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

# this will be needed for some computations later
Y <- dfs$Y
A <- dfs$A
covariateNames <- labels(terms(ps_model))
X <- dfs[, covariateNames] 
n <- nrow(dfs)
n_features <- length(covariateNames)
epsilon <- .01 # threshold for extreme propensity scores

dfs$ID <- c(1:nrow(dfs)) # unique ID variable

#- set seed for reproducibility
seed <- 123

# create hyperparameter grid for tuning
hyper_grid <- expand.grid(
  num.trees =  n_features * 10,
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10),
  replace = c(TRUE, FALSE),
  sample.fraction = c(.5, .63, .8, 1),
  error = NA
)
hyper_grid <- hyper_grid[!(hyper_grid$replace == FALSE & hyper_grid$sample.fraction == 1), ]
# add the default random forest settings to grid
hyper_grid_prob <- rbind(hyper_grid,
                         c(500, floor(sqrt(n_features)), 10, TRUE, 1, NA))
hyper_grid_reg <- rbind(hyper_grid,
                        c(500, floor(sqrt(n_features)), 5, TRUE, 1, NA))

#--------------------------------------------------------------------------------
# X-Learner with full-sample estimation of nuisance functions
# and repeated 10-fold crossfitting for CATE
#--------------------------------------------------------------------------------

# things for first step
resMatrix <- matrix(NA, nrow(dfs), 3)
colnames(resMatrix) <- c("mu0", "mu1", "psi_x")

# things for second step
nFolds <- 10; nrep <- 50
counter <- 0; critval <- qnorm(0.975); num_tiles <- 5
tauMatrix <- matrix(0, n, nrep) # to save the estimated CATEs per iteration
HetList <- list(beta1 = NULL, beta2 = NULL, GATES = NULL, CLAN = NULL) # to save results from heterogeneity analysis

tictoc::tic("X-Learner with 2-step crossfitting procedure")
s_x <- Sys.time()

dfs0 <- dfs[dfs$A == 0, ]
dfs1 <- dfs[dfs$A == 1, ]


# STEP 1: Generate pseudo-outcomes for full sample 
#--------------------------------------------------------------

# We use full-sample estimation with OOB predictions here,
# but the pseudo-outcomes could also be obtained via k-fold cross-fitting.

#- Train a random forest for the control group data:
# execute grid search for hyperparameter tuning
tictoc::tic("Tuning mu0 model")
for (k in seq_len(nrow(hyper_grid_reg))) {
  # fit model for the k-th hyperparameter combination 
  mu0_fit <- ranger::ranger(formula = m_model,
                            data = dfs0,
                            num.trees = hyper_grid_reg$num.trees[k], 
                            mtry = hyper_grid_reg$mtry[k],
                            min.node.size = hyper_grid_reg$min.node.size[k],
                            replace = hyper_grid_reg$replace[k],
                            sample.fraction = hyper_grid_reg$sample.fraction[k],
                            verbose = TRUE,
                            seed = seed
  )
  # save OOB error
  hyper_grid_reg$error[k] <- mu0_fit$prediction.error
}
tictoc::toc() 

# choose the best performing hyperparameter values and rerun the the random forest
# with these values
res_mu0_hyperparams <- hyper_grid_reg %>% arrange(error)
mu0_hyperparams <- res_mu0_hyperparams[1, ]
mu0_fit <- ranger::ranger( formula = m_model, data = dfs0, keep.inbag = TRUE,
                           seed = seed, num.trees = mu0_hyperparams$num.trees,
                           mtry = mu0_hyperparams$mtry,
                           min.node.size = mu0_hyperparams$min.node.size,
                           replace = mu0_hyperparams$replace,
                           sample.fraction = mu0_hyperparams$sample.fraction,
                           verbose = TRUE )

# Obtain predictions for mu_0, use OOB predictions where applicable:
mu0_hat <- rep(0, n)
mu0_hat[dfs$A==0] <- mu0_fit$predictions # OOB predictions 
mu0_hat[dfs$A==1] <- predict(mu0_fit, dfs1)$predictions
resMatrix[, "mu0"] <- mu0_hat

#- Train a random forest for the treatment group data:
tictoc::tic("Tuning mu1 model")
hyper_grid_reg$error <- NA
for (k in seq_len(nrow(hyper_grid_reg))) {
  # fit model for the k-th hyperparameter combination 
  mu1_fit <- ranger::ranger(formula = m_model,
                            data = dfs1,
                            num.trees = hyper_grid_reg$num.trees[k], 
                            mtry = hyper_grid_reg$mtry[k],
                            min.node.size = hyper_grid_reg$min.node.size[k],
                            replace = hyper_grid_reg$replace[k],
                            sample.fraction = hyper_grid_reg$sample.fraction[k],
                            verbose = TRUE,
                            seed = seed
  )
  # save OOB error
  hyper_grid_reg$error[k] <- mu1_fit$prediction.error
}
tictoc::toc() 

# choose the best performing hyperparameter values and rerun the the random forest
# with these values
res_mu1_hyperparams <- hyper_grid_reg %>% arrange(error)
mu1_hyperparams <- res_mu1_hyperparams[1, ]
mu1_fit <- ranger::ranger( formula = m_model, data = dfs1, keep.inbag = TRUE,
                           seed = seed, num.trees = mu1_hyperparams$num.trees,
                           mtry = mu1_hyperparams$mtry,
                           min.node.size = mu1_hyperparams$min.node.size,
                           replace = mu1_hyperparams$replace,
                           sample.fraction = mu1_hyperparams$sample.fraction,
                           verbose = TRUE )

# Obtain predictions for mu_1, use OOB predictions where applicable:
mu1_hat <- rep(0, n)
mu1_hat[dfs$A==1] <- mu1_fit$predictions # OOB predictions 
mu1_hat[dfs$A==0] <- predict(mu1_fit, dfs0)$predictions
resMatrix[, "mu1"] <- mu1_hat

# Compute the pseudo-outcome using the estimated conditional mean function from the respective other group:
psi_x_0 <- predict(mu1_fit, dfs0)$predictions - dfs0$Y
psi_x_1 <- dfs1$Y - predict(mu0_fit, dfs1)$predictions

# Save the pseudo outcomes
resMatrix[, "psi_x"][dfs0$ID] <- psi_x_0 
resMatrix[, "psi_x"][dfs1$ID] <- psi_x_1


# STEP 2: CATE estimation and heterogeneity analysis
#--------------------------------------------------------------
# use repeated 10-fold cross-fitting 

while( counter < nrep) {
  
  counter <- counter + 1
  cat("This is iteration: ", counter, "out of", nrep,"\n")
  
  set.seed(seed + counter)
  
  # Split data into three approximately even sized folds, with
  # the number of treated and untreated persons distributes evenly across folds
  folds <- groupdata2::fold(dfs, k = nFolds, cat_col = "A")
  
  matrixTMP <- matrix(0, n, 2) # to save PS and m(x) for heterogeneity analysis per repetition
  colnames(matrixTMP ) <- c("ps", "m")
  
  for (f in 1:nFolds) {
    
    dfs_train <- as.data.frame( folds[folds$.folds != f,] )
    dfs_heldout <- as.data.frame( folds[folds$.folds == f,] )
    
    # Train a classification model to get the propensity scores
    ps_fit <- ranger::ranger(y = dfs_train$A, x = dfs_train[, covariateNames],
                             probability = TRUE)
    
    # Predict propensity score for heldout data
    ps_hat <- predict(ps_fit,  dfs_heldout)$predictions[, 2]
    
    # Ensure positivity assumption by adding/substracting a small threshold to
    # estimated propensity scores close to zero/one
    # ps_hat <- ifelse(ps_hat < epsilon, epsilon,
    #                  ifelse(ps_hat > 1-epsilon, 1-epsilon, ps_hat))
    matrixTMP[dfs_heldout$ID, "ps"] <- ps_hat
    
    # Compute the treatment effect functions separately in the two groups
    X_train <- dfs_train[,  covariateNames]
    A_train <- dfs_train$A
    tau_x_0_fit <- ranger::ranger(y = resMatrix[dfs_train$ID, "psi_x"][A_train==0],
                                  x = X_train[A_train==0, ])
    tau_x_1_fit <- ranger::ranger(y = resMatrix[dfs_train$ID, "psi_x"][A_train==1],
                                  x = X_train[A_train==1, ])
    
    # Predict the treatment effects using the two functions and combine the
    # predictions using the propensity score 
    tau_x_0_hat <- predict(tau_x_0_fit, dfs_heldout)$predictions
    tau_x_1_hat <- predict(tau_x_1_fit, dfs_heldout)$predictions
    tauMatrix[dfs_heldout$ID, counter] <- ps_hat* tau_x_0_hat + (1-ps_hat)* tau_x_1_hat
    
    #- Estimation of additional nuisance function for the heterogeneity analyses
    m_fit <- ranger::ranger(m_model, dfs_train)
    m_hat <- predict(m_fit,  dfs_heldout)$predictions
    matrixTMP[dfs_heldout$ID, "m"] <- m_hat
  }
  
  #----------------------------------------------------------------------------
  # Heterogeneity analysis
  #----------------------------------------------------------------------------
  
  # 1) Global test for heterogeneity
  resid_treat <- A - matrixTMP[, "ps"]
  resid_out <- Y - matrixTMP[, "m"]
  cate_x <- tauMatrix[, counter] 
  ate_hat <- mean( cate_x  ) 
  blp_dfs <- data.frame( outcome = resid_out, pred1 = resid_treat, 
                         pred2 <- (cate_x - ate_hat) * resid_treat )
  best.linear.predictor <- lm(outcome ~ 0 + pred1 + pred2, data = blp_dfs)
  # calculate robust standard errors
  blp.output <- lmtest::coeftest(best.linear.predictor, vcov = sandwich::vcovCL, 
                                 type = "HC3")
  # save results
  HetList$beta1 <- rbind( HetList$beta1, blp.output[1, ] )
  HetList$beta2 <- rbind( HetList$beta2, blp.output[2, ] )
  
  # 2) GATES
  # Define 5 subgroups based on estimated CATE
  tiles <- factor(dplyr::ntile(cate_x, num_tiles))
  dummies <- model.matrix(~ 0 + tiles)
  gates_dfs <- data.frame( outcome = resid_out, resid_treat * dummies  )
  m_gates <- reformulate(paste0("tiles", 1:num_tiles), intercept = FALSE, response = "outcome")
  res_gates <- lm(m_gates, data = gates_dfs)
  # calculate robust standard errors
  gates.output <- lmtest::coeftest(res_gates, vcov = sandwich::vcovCL, 
                                   type = "HC3")
  GATES_unsorted <- as.data.frame( gates.output[,] )
  GATES_unsorted$ntile <-  1:num_tiles
  GATES <- GATES_unsorted[order(GATES_unsorted$Estimate), ] 
  GATES$sortedGroup <- 1:num_tiles
  GATES$CL = GATES$Estimate - critval * GATES$`Std. Error`
  GATES$CU = GATES$Estimate + critval * GATES$`Std. Error`
  HetList$GATES <- rbind(HetList$GATES, GATES)
  
  # 3) CLAN: Comparison of average covariates between the 20% most and least affected
  #--------------------------------------------------------------------------------
  
  ntile_x <- factor(dplyr::ntile(cate_x, num_tiles)) # assign respective subgroup to each observation
  dfs$ntile_x <- plyr::mapvalues(ntile_x, GATES$ntile, GATES$sortedGroup) # sort the subgroups
  
  # Per covariate, extract the mean and CI for the 20% most positively and 20% most negatively
  # affected persons and test their difference with a Welch-test,
  # using Holm-adjusted p-values
  CLAN <- sapply(covariateNames, function(x){
    
    # select first and fifth sorted subgroup
    dfsTMP <- dfs[dfs$ntile_x %in% c(1,num_tiles),]
    dfsTMP$ntile_x <- factor(dfsTMP$ntile_x)
    
    # extract mean and robust standard error / CIs per subgroup
    lm.fit <- estimatr::lm_robust(as.formula(paste0(x, ' ~ 0 + ntile_x')), data = dfsTMP)
    sum.lm.fit <- data.frame( coef(summary(lm.fit))[, c("Estimate", "CI Lower", "CI Upper")] )
    
    # perform Welch test
    m <- as.formula(paste0(x, ' ~ ntile_x'))
    ttest.diff <- stats::t.test(m, data = dfsTMP, var.equal = FALSE) 
    effect.size <- rstatix::cohens_d(m, data = dfsTMP, var.equal = FALSE, hedges.correction = TRUE )
    
    # store results
    res <- c(mean_group1 = sum.lm.fit[1,1], CI.Lower_g1 = sum.lm.fit[1,2], CI.Upper_g1 = sum.lm.fit[1,3],
             mean_group5 = sum.lm.fit[2,1], CI.Lower_g5 = sum.lm.fit[2,2], CI.Upper_g5 = sum.lm.fit[2,3],
             diff = ttest.diff$estimate[[1]] - ttest.diff$estimate[[2]],
             CI.Lower = ttest.diff$conf.int[1], CI.Upper = ttest.diff$conf.int[2],
             pvalue = ttest.diff$p.value, hedgesG = unname(effect.size$effsize) )
    return( res )
  })
  # adjust the p-values using Holm's correction for multiple testing
  CLAN <- data.frame( t(CLAN) )
  CLAN$pvalue <- p.adjust(CLAN$pvalue, method = "holm")
  HetList$CLAN <- rbind(HetList$CLAN, CLAN)
}
e_x <- Sys.time()
tictoc::toc()
time_x <- difftime(e_x, s_x, units = "secs")[[1]]


# Take medians across repetitions as the final estimates
#---------------------------------------------------------------------------

cate_x <- rowMeans(tauMatrix)
hist(cate_x, main  = "X-Learner with repeated crossfitting for CATE")
c(mean(cate_x), sd(cate_x), quantile(cate_x))
#                           0%        25%        50%        75%       100% 
# 0.8085422  0.8469352 -1.4881962  0.2209718  0.6988687  1.2858661  5.2820286 

# compare to full-sample version
res[3, ]
cor(res_cates$X.Learner, cate_x) # 0.9605285



# 1) Global test for heterogeneity
#--------------------------------------------------------
beta1All <- as.data.frame(HetList$beta1)
# compute 95% confidence intervals
beta1All$CL <- beta1All$Estimate - critval * beta1All$'Std. Error'
beta1All$CU <- beta1All$Estimate + critval * beta1All$'Std. Error'
beta1 <- apply(beta1All, 2, median)
round(beta1, 3)
# Estimate Std. Error    t value   Pr(>|t|)         CL         CU 
#   0.661      0.280      2.351      0.019      0.110      1.208 

beta2All <- as.data.frame(HetList$beta2)
# compute 95% confidence intervals
beta2All$CL <- beta2All$Estimate - critval * beta2All$'Std. Error'
beta2All$CU <- beta2All$Estimate + critval * beta2All$'Std. Error'
beta2 <- apply(beta2All, 2, median)
round(beta2, 3)
# Estimate Std. Error    t value   Pr(>|t|)         CL         CU 
#   0.943      0.336      2.847      0.004      0.293      1.590 


# 2) GATES
#--------------------------------------------------------
GATES <- aggregate(cbind(Estimate, `Std. Error`, CL, CU) ~ sortedGroup, data = HetList$GATES, median)
round(GATES, 3) 
#         ntile Estimate Std. Error     CL    CU
# 1           1   -0.548      0.489 -1.520 0.420
# 2           2   -0.018      0.528 -1.054 0.992
# 3           3    0.685      0.597 -0.564 1.809
# 4           4    1.185      0.656 -0.118 2.464
# 5           5    2.074      0.804  0.525 3.656

# Plot GATES
p_gates <- ggplot(GATES, aes(x=sortedGroup, y=Estimate)) + 
  geom_point() +
  geom_errorbar(aes(ymin=CL, ymax=CU), width=.2,
                position=position_dodge(0.05)) + 
  geom_hline(yintercept = 0, col = "black", linewidth = 0.2,
             linetype = "dashed")  +
  labs(x = "Group by size of predicted treatment effect", y = "Estimated Treatment Effect" ) + 
  theme_bw(base_size = 12) + theme(panel.grid.minor = element_blank())
print(p_gates)
# ggsave("GATES_Xlearner.png", p_gates, width = 5.84, height = 4.10, dpi = 300)



# 2) CLAN
#--------------------------------------------------------
HetList$CLAN$covariate <- rep(1:n_features, nrep)
CLAN <- aggregate(. ~ covariate, data = HetList$CLAN, median)
rownames(CLAN) <- covariateNames
CLAN$covariate <- NULL
round(CLAN, 3)  

# focus on covariates whose mean difference is non-negligible (Hedge's g > .2)
selectedCovariateNames <- rownames(CLAN[abs(CLAN$hedgesG) > .2, ])
round(CLAN[selectedCovariateNames, c("mean_group1", "mean_group5", "diff", "hedgesG", "pvalue")], 3)
#                              mean_group1 mean_group5   diff hedgesG pvalue
# Hispanic                          0.052       0.143 -0.091  -0.312      0
# Black                             0.133       0.248 -0.115  -0.295      0
# Asian                             0.001       0.102 -0.100  -0.463      0
# Health                            4.081       3.691  0.383   0.428      0
# ProblemAvoidance                  2.880       3.354 -0.469  -0.461      0
# AlcoholUse                        2.892       1.966  0.914   0.544      0
# TeamSports                        1.567       1.187  0.379   0.340      0
# Excercise                         1.891       1.348  0.539   0.535      0
# TimeWithFriends                   2.495       1.088  1.400   1.740      0
# HoursVideosWeekly                20.109      24.796 -4.810  -0.219      0
# ActivitiesWithParent              5.296       6.488 -1.203  -0.364      0
# ParentalCloseness                 4.229       4.451 -0.218  -0.346      0
# FamilySupport                     3.817       4.147 -0.327  -0.460      0
# AtLeastTwoAttemptedSuicides       0.043       0.005  0.038   0.247      0
# PriorTreatment                    0.161       0.075  0.084   0.262      0
# CES_D                            10.140      12.033 -1.849  -0.240      0


# Create LaTeX table 
CLANTab <- CLAN[rownames(CLAN) %in% selectedCovariateNames, ]
CLANTab$blank <- rep(NA, nrow(CLANTab)) # add empty column (for the table)
CLANTab <- cbind.data.frame(CLANTab$blank, CLANTab[, 1:3],
                            CLANTab$blank, CLANTab[, 4:6],
                            CLANTab$blank, CLANTab[, 7:11])
print( xtable::xtable(CLANTab,
                      caption = "Results of CLAN",
                      label =  "tab:CLAN", align = c("l", rep("c", 14)),
                      digits = c(rep(2, 13), 3, 2)), booktabs = TRUE )
# table code can be customized in LaTeX as desired 