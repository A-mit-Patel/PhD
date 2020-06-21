library(binom)
library(nnet)
library(dfcrm)

wages.tait.ext <- function(y, z, dose, n.ar, p.skel, q.skel, tul, ell,
                      prior.tox = NULL, prior.eff = NULL, safety.conf = 0.95,
                      futility.conf=0.95){
  
  # y - vector to indicate if patient experiences a toxicity event (0 or 1) 
  # z - vector to indicate if patient experiences a efficacy event (0 or 1)
  # dose - vector to show which dose each patient received
  # n.ar - number of patiets for adaptive randomisation phase 
  # p.skel - vector/matrix for toxicity skeleton
  # q.skel - vector/matrix for efficacy skeleton
  # TUL - toxicity upper limit
  # ELL - efficacy lower limit
  # prior.tox - prior for each toxicity ordering
  # prior.eff - prior for each efficacy ordering
  # control.arm - activate feature to randomly allocate to control arm 
  
  post.tox <- function(a, p, y){
    s2 <- 1.34
    lik <- exp(-0.5*a*a/s2) 
    for (i in 1:length(p)){
      lik <- lik*(((p[i]^exp(a))^y[i]) * ((1-(p[i]^exp(a)))^(1-y[i])))
    }
    return(lik)
  }
  
  posttoxf <- function(a, c, p, y, i){
    c[i]^(exp(a))*post.tox(a, p, y)
  }
  
  post.eff <- function(b, q, z){
    s2 <- 1.34
    lik <- exp(-0.5*b*b/s2) 
    for (i in 1:length(q)){
      lik <- lik*(((q[i]^exp(b))^z[i]) * ((1-(q[i]^exp(b)))^(1-z[i])))
    }
    return(lik)
  }
  
  postefff <- function(b, c, q, z, i){
    c[i]^(exp(b))*post.eff(b, q, z)
  }
  
  
  # Change skeletons to matricies 
  if(is.vector(p.skel)) p.skel=t(as.matrix(p.skel));
  if(is.vector(q.skel)) q.skel=t(as.matrix(q.skel));
  
  
  
  ncomb <- ncol(p.skel) # number of combos/doses
  ptox.hat <- numeric(ncomb) # estimate of toxicity prob
  peff.hat <- numeric(ncomb) # estimate of efficacy prob
  nord.tox <- nrow(p.skel) # number of orders for toxicity 
  nord.eff <- nrow(q.skel) # number of orders for efficacy
  ptox.hat = numeric(ncomb) # estimate of toxicity prob
  peff.hat = numeric(ncomb) # estimate of efficacy prob
  
  
  # If priors for ordering of toxicity and efficacy are null consider all orders
  # to be equally likely
  if(is.null(prior.tox)){ 
    prior.tox <- rep(1/nord.tox, nord.tox)
  }
  if(is.null(prior.eff)){
    prior.eff <- rep(1/nord.eff, nord.eff)
  }
  
  # Calculate posterior probabilites for each toxicity ordering 
  marginal.tox <- rep(0, nord.tox)
  for(i in 1:nord.tox){ # Loop through each order for tox
    # match the toxicity skeleton values to the dose patient received
    p <- p.skel[i,][dose] 
    marginal.tox[i] <- integrate(post.tox, lower=-Inf, upper=Inf, 
                                 p=p, y=y)$value
  }
  postprob.tox <- (marginal.tox * prior.tox) / sum(marginal.tox * prior.tox)
  
  # Calculate posterior probabilities for each efficacy ordering
  marginal.eff <- rep(0, nord.eff)
  for (i in 1:11) { # Loop through each order for eff
    #match the efficacy skeleton values to the dose patient received
    q <- q.skel[i,][dose]
    marginal.eff[i] <- integrate(post.eff, lower=-Inf, upper=Inf,
                                 q=q, z=z)$value
  }
  postprob.eff <- (marginal.eff * prior.eff) / sum(marginal.eff * prior.eff)

  # Toxicity model selection
  # identify the model/order with the highest posterior probability
  if(nord.tox > 1){
    mtox.sel <- which.is.max(postprob.tox)
  }
  else {
    mtox.sel <- 1
  }

  # Efficacy model selection
  # identify the model/order with the highest posterior probability
  if(nord.eff > 1){
    meff.sel <- which.is.max(postprob.eff)
  }
  else {
    meff.sel <- 1
  }

  # Calculate posterior mean of toxicity probability at each dose combo
  for (i in 1:ncomb) {
    p <- p.skel[mtox.sel,][dose[1:length(dose)]]
    c <- p.skel[mtox.sel,]
    ptox.hat[i] <- integrate(posttoxf, lower=-Inf, upper=-Inf, c=c,
                             p=p, y=y, i=i)$value / marginal.tox[mtox.sel]
  }
  # Calculate posterior mean of efficacy probability at each dose combo
  for (i in 1:ncomb) {
    q <- q.skel[meff.sel,][dose[1:length(dose)]]
    c <- q.skel[meff.sel,]
    peff.hat[i] <- integrate(postefff, lower=-Inf, upper=-Inf, c=c,
                             q=q, z=z, i=i)$value / marginal.eff[meff.sel]
  }

  # Find set of non-toxic doses
  aset <- which(ptox.hat <= tul)
  if(length(aset)==0){
    aset = which.min(ptox.hat)
  }

  # Assign randomisation probabilities based on estimates of efficacy
  peff.hat.aset <- rep(0,ncomb)
  peff.hat.aset[aset] <- peff.hat[aset]
  ar.prob <- peff.hat.aset / sum(peff.hat.aset)

  if(length(aset)==1){
    # If only one dose safe the best and recommended dose can only be this
    comb.best <- aset
    comb.curr <- aset
  }
  else {
    # Else best dose is one with max probability of efficacy
    comb.best <- which.max(peff.hat.aset)
    # In AR stage sample from aset based on probabilities calculated after AR
    # current dose is best
    ifelse(length(dose) <= n.ar,
           comb.curr <- sample(1:ncomb, 1, prob = ar.prob),
           comb.curr <- comb.best)
  }

  max.dose.tried <- max(dose)
  if(comb.curr > max.dose.tried + 1){
    # Prevent skipping untried doses
    # Doesnt't this contradict the whole point of AR stage???
    comb.curr <- max.dose.tried + 1
  }

  #stopping rules
  stop <- 0
  safety <- binom.confint(sum(y[dose == 1] == 1), sum(dose == 1),
                          conf.level = safety.conf, methods = "exact")$lower
  if(safety > tul){
    stop <- 1
  }
   if(length(dose) > n.ar){
     futility <- binom.confint(sum(z[dose==comb.curr]==1), sum(dose==comb.curr),
                               conf.level=futility.conf, methods="exact")$upper
     if(futility < ell){
       stop <- 1
     }
   }
  else {
    futility <- NULL
  }
  
  return(list(
    # marginal.tox = marginal.tox,
    ToxBayesFactor = postprob.tox,
    ProbTox = ptox.hat,
    ToxSkeleton = mtox.sel,
    # marginal.eff = marginal.eff,
    EffBayesFactor = postprob.eff,
    EffSkeleton = meff.sel,
    ProbEff = peff.hat,
    AdaptiveRandProb = ar.prob,
    RecommendedDose = comb.curr,
    OptimalDose = comb.best,
    Dose1ToxLowerBound = safety,
    LowestActiveDoseToxLowerBound = safety,
    RecommendedDoseEffUpperBound = futility,
    AdmissibleSet = aset,
    Stop = stop
  ))
}


source('T:/Code/R/CRCTU/wagestait/kb/version2/wagestait.R')

##### Specify the total number of doses
d <- 6
##### Specify the number of possible toxicity orderings
s <- 1   
### Specify a set of toxicity skeleton values
p.skel <- c(0.01, 0.08, 0.15, 0.22, 0.29, 0.36)
##### Specify the number of possible efficacy orderings
g <- 11   
###Specifiy the possible efficacy orderings of the drug combinations
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c(0.60, 0.50, 0.40, 0.30, 0.20, 0.10)  
q.skel[2,] <- c(0.50, 0.60, 0.50, 0.40, 0.30, 0.20)  
q.skel[3,] <- c(0.40, 0.50, 0.60, 0.50, 0.40, 0.30)  
q.skel[4,] <- c(0.30, 0.40, 0.50, 0.60, 0.50, 0.40)  
q.skel[5,] <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.50)  
q.skel[6,] <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60)  
q.skel[7,] <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.60)  
q.skel[8,] <- c(0.30, 0.40, 0.50, 0.60, 0.60, 0.60)  
q.skel[9,] <- c(0.40, 0.50, 0.60, 0.60, 0.60, 0.60)  
q.skel[10,] <- c(0.50, 0.60, 0.60, 0.60, 0.60, 0.60)  
q.skel[11,] <- c(rep(0.60, 6))  

# Limits
tul <- 0.33 ## toxicity upper limit 
ell <- 0.05 ## efficacy lower limit

n = c(3, 3, 2, 0, 0, 0)  # Dose 1 (d1) given to 3 patients; d2 to 3; d3 to 2 pts; etc
y = c(1, 0, 1, 0, 0, 0) # 1 tox at d1, 0 @ d2, 1 @ d3, etc
z = c(0, 1, 2, 0, 0, 0) # 0 eff at d1; 1 @ d2; 2 @ d3; etc
set.seed(1)  # Set seed for reproducible randomness
dose_decision <- wages.tait(y, z, n, p.skel, q.skel, n.ar=16, comb.curr=3)

dose_decision$AdmissibleSet  # Only doses 1 and 2 are currently admissible...
dose_decision$ProbTox < tul  # ...because only those doses have Pr(Tox) below the upper limit
dose_decision$EffBayesFactor  # Efficacy model likelihoods to select most likely, i.e. Bayes Factors
which.max(dose_decision$EffBayesFactor)  # Efficacy skeleton 6 is the most likely, i.e. monontonic increasing
dose_decision$ProbEff
dose_decision$AdaptiveRandProb
dose_decision$RecommendedDose  # Remember, in the adaptive randomisation phase, this is random!
dose_decision$OptimalDose
dose_decision$Stop  # Do not stop yet...

# Need to adjust y,z and n
# N is now dose to indicate the dose each patient received
dose <- c(1,1,1,2,2,2,3,3) # D1 given to 3, D2 given to 3, D3 given to 2
y <- c(0,0,1,0,0,0,1,0) #1 tox at D1, 1 tox at D3. Test code to ensure changing where these occur make no diff 
z <- c(0,0,0,0,1,0,1,1) # 1 eff at D2, 2 at D3
set.seed(1)
dose_decision1 <- wages.tait.ext(y, z, dose, n.ar=16, p.skel, q.skel, tul, ell)

dose_decision$AdmissibleSet  # Only doses 1 and 2 are currently admissible...
dose_decision1$AdmissibleSet
dose_decision$ProbTox < tul  # ...because only those doses have Pr(Tox) below the upper limit
dose_decision1$ProbTox < tul
dose_decision$EffBayesFactor  # Efficacy model likelihoods to select most likely, i.e. Bayes Factors
dose_decision1$EffBayesFactor 
which.max(dose_decision$EffBayesFactor)  # Efficacy skeleton 6 is the most likely, i.e. monontonic increasing
which.max(dose_decision1$EffBayesFactor)
dose_decision$ProbEff
dose_decision1$ProbEff
dose_decision$AdaptiveRandProb
dose_decision1$AdaptiveRandProb
dose_decision$RecommendedDose  # Remember, in the adaptive randomisation phase, this is random!
dose_decision1$RecommendedDose
dose_decision$OptimalDose
dose_decision1$OptimalDose
dose_decision$Stop 
dose_decision1$Stop 

