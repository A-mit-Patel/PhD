#------------------------------------#
# TITE-PO-CRM                        #
#------------------------------------#

## Program Details ---- 
# Author: Amit Patel
# Version number: 1.0 
# Version date: 5 May 2023

## Background ---- 
# This function is to facilitate the use of the TITE-PO-CRM methodology for 
# dose-finding trials. This is an altered version of the pocrm.imp function
# from the pocrm package. 

## Parameters ----
# prior.s -     A matrix of values corresponding to the possible orderings 
#               of the toxicity probabilities from the skeleton 
# prior.o -     A vector of prior prbabilites for the ordering. (Must sum up to  
#               1 and be the same length as the number of possible orders)
# target -      The target DLT rate 
# dlt -         Vector of patient outcomes indicating if patient experienced a  
#               DLT (1 indicates DLT, 0 indicates no DLT)
# dose -        A vector of dose-levels assigned to patients (Length of dose  
#               must be the same as the length of tox)
# weight -      A vector of weights for each patient. Weights must take values 
#               between 0 and 1 (Length of weigh must be the same as length of  
#               tox and dose)
# stop.low -    Recommend stopping the trial if toxicity is too high at lowest 
#               dose (TRUE/FALSE default is FALSE)
# stop.target - DLT rate required to trigger stopping rule. Must be specified if
#               stop.low == TRUE (Must be higher than target)
# stop.conf -   Confidence level to trigger stopping rule. Must be specified if 
#               stop.low == TRUE

## Packages ----
# These packages aren't require to run this function but contain helpful 
# functions which will facilitate the use of this function. getprior in the 
# dfcrm pacakge to obtain a skeleton and getwm from pocrm to obtain the matrix
# of skeleton values (prior.s)
library(dfcrm)
library(pocrm)

tite.pocrm <- function(prior.s, 
                       prior.o,
                       target,
                       dlt,
                       dose,
                       weight,
                       stop.low = FALSE,
                       stop.target = NA, 
                       stop.conf = NA)
{
  
  # Empty vector to store estimates 
  pred <- rep(0, nrow(prior.s))

  # Empty vector to store likelihood
  lik <- rep(0, nrow(prior.s))
  
  # n is the number of patients
  n <- length(dlt)  
  
  # Loop through each order
  for (k in 1:nrow(prior.s)) {
    # Apply skeleton values to patients doses
    beta <- prior.s[k,][dose] 
    
    # Likelihood calculation
    ll <- function(a) {
      la <- 0
      # Loop through each patient 
      for (i in 1:n) { 
        la <- la + dlt[i] * a * log(weight[i]*beta[i]) + 
          (1-dlt[i]) * log((1 -weight[i]*beta[i]^a))
      }
      la
    }
    
    # Estimates for each order
    pred[k] <- optimize(f = ll, interval = c(0, 500), maximum = T)$maximum
    # Likelihood for each order
    lik[k] <- ll(pred[k])

  }
  
  # Posterior probabilities of each order
  pord <- (exp(lik) * prior.o)/sum(exp(lik) * prior.o)
  # Select order with greatest posterior
  ord <- which.is.max(round(pord,7)) 
  # Estimate of the model parameter from the selected order
  bhat <- pred[ord]
  # Updated estimates of the toxicity probabilities 
  rpred <- prior.s[ord, ]^bhat
  # Select dose level closest to the target 
  next.lev <- which.is.max(-(abs(rpred - target))) 
  # Obtain the current dose level
  current.lev <- tail(dose, n = 1) 
  
  
  # Calculation of the inverse variance 
  beta <- prior.s[ord,][dose]
  inverse.var <- 0
  for (i in 1:n) { 
    inverse.var <- inverse.var + ((1-dlt[i])*(weight[i]*beta[i]^bhat)*(log((weight[i]*beta[i])))^2)/
      (1-((weight[i]*beta[i]^bhat)))^2
  }
  
  inverse.var <- ifelse(inverse.var == 0, 0, 1/inverse.var)
  

  # If stopping rule is true check toxicity at lowest dose 
  if (stop.low == TRUE){
    
    # Sample to check if lowest dose too toxic 
    samp <- rnorm(n = 100000, mean = bhat, sd = sqrt(inverse.var))
    tox.low <- prior.s[ord, 1]^samp
    prob.tox.low <- mean(tox.low > stop.target) 
    
    if(prob.tox.low > stop.conf){
      next.lev <- "stop"
    }
  }
  
  # 95% Confidence Intervals 
  crit <- qnorm(0.975)
  ptox.L <- prior.s[ord,]^(bhat + crit*sqrt(inverse.var))
  ptox.U <- prior.s[ord,]^(bhat - crit*sqrt(inverse.var))
  
  # Output 
  out <- list(ord.prob = round(pord,3), 
              order.est =  ord, 
              b.est = round(bhat, 3),
              ptox.est = rpred, 
              dose.rec = next.lev,
              ptox.U = ptox.U,
              ptox.L = ptox.L,
              post.var = 1/inverse.var,
              se = sqrt((inverse.var^2)/n))

}

################################################################################
# Examples 
################################################################################

# Recreate the example from the pocrm package with full weighting to see if 
# function works 

# orders<-matrix(nrow=8,ncol=8)
# orders[1,] <- c(1,2,3,4,5,6,7,8)
# orders[2,] <- c(1,3,2,4,5,6,7,8)
# orders[3,] <- c(1,2,3,5,4,6,7,8)
# orders[4,] <- c(1,2,3,4,5,7,6,8)
# orders[5,] <- c(1,3,2,5,4,6,7,8)
# orders[6,] <- c(1,3,2,4,5,7,6,8)
# orders[7,] <- c(1,2,3,5,4,7,6,8)
# orders[8,] <- c(1,3,2,5,4,7,6,8)
# 
# skeleton <- c(0.01,0.03,0.10,0.20,0.33,0.47,0.60,0.70)
# 
# prior.s <- getwm(orders,skeleton)
# 
# prior.o <- rep(1/8,8)
# 
# target <- 0.20
# 
# dose <- c(2,3,5,4,7,5,4,3,2,2,3)
# 
# dlt <- c(0,0,0,0,1,1,1,0,0,1,1)
# 
# weight <- rep(1, length(dose))
# 
# tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target, 
#            dlt = dlt, dose = dose, weight = weight) -> fit1 
# 
# pocrm.imp(alpha = prior.s, prior.o = prior.o, theta = target, y = dlt, 
#           combos = dose) -> fit2

# Recommended dose matches, order recommendation differs but probabilities 
# for the ordering are the same so this is acceptable 

################################################################################

# ADePT Example 
# orders<-matrix(nrow=2,ncol=6)
# orders[1,]<-c(1,2,3,4,5,6)
# orders[2,]<-c(1,2,3,5,4,6)
# 
# skeleton <- c(0.012,0.036,0.084,0.157,0.25,0.355)
# 
# prior.s <- getwm(orders,skeleton)
# prior.o <- rep(1/2,2)
# target <- 0.25
# 
# dose <-   c(2,2,2,3,3,3)
# dlt <-    c(0,0,0,1,0,0)
# weight <- c(1,1,1,1,0.8,0.8)
# 
# tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target, 
#            dlt = dlt, dose = dose, weight = weight) -> fit1 
# 
# # Check stopping works 
# dlt <-    c(1,1,1,1,0,0)
# weight <- c(1,1,1,1,0.8,0.8)
# 
# 
# tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target, 
#            dlt = dlt, dose = dose, weight = weight, 
#            stop.low = TRUE, stop.target = 0.35, stop.conf = 0.7) -> fit1 
################################################################################