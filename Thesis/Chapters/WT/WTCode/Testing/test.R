
# This code for the Wages & Tait method for phase I/II dose-finding does not belong to CRCTU!
# It was given to us to for research purposes: Nolan sent it to KB on 8-Jan-2016
# *** Definitely do not publish or redistribute this code! ***


# The intended scope is Research, not Trial Conduct.
# Note: functions here are not validated.


# Here starts Nolan's file

#########################################################################
#									
# 	This R Program simulates the method of Wages and Tait (2015)* 				
#									
#	*Wages, N. and Tait, C. (2015) Seamless Phase I/II Adaptive Design 
#	for Oncology Trials of Molecularly Targeted Therapies. J Biopharm Stats.							
#									
#########################################################################



###install required R packages
library(binom)
library(nnet)
library(dfcrm)


# Written by KB to split out method calculation and simulation
post.tox <- function(a, p, y, n) {
  s2=1.34
  lik=1
  for(j in 1:length(p)){
    pj=p[j]**exp(a)
    lik=lik*pj^y[j]*(1-pj)^(n[j]-y[j]);
  }
  return(lik*exp(-0.5*a*a/s2));
}

# the posterior mean of ptox
posttoxf <- function(a, p, y, n, j) { 
  p[j]^(exp(a))*post.tox(a, p, y, n); 
}

post.eff <- function(b, q, z, n) {
  s2=1.34
  lik=1
  for(j in 1:length(q)){
    qj=q[j]**exp(b)
    lik=lik*qj^z[j]*(1-qj)^(n[j]-z[j]);
  }
  return(lik*exp(-0.5*b*b/s2));
}

# the posterior mean of peff
postefff <- function(b, q, z, n, j) {
  q[j]^(exp(b))*post.eff(b, q, z, n); 
}

wages.tait = function(y, z, n, p.skel, q.skel, n.ar, comb.curr = NULL, 
                      mprior.tox = NULL, mprior.eff = NULL, 
                      safety.confidence = 0.95, futility.confidence = 0.95,
                      check.tox.at.dose.level = 1, 
                      lowest.is.placebo = FALSE, placebo.rand.prob = NULL) {
  
  # y, number of toxicity events at each dose
  # z, number of efficacy events at each dose
  # n, number of patients allocated to each dose
  # p.skel, toxicity skeleton(s) as vector or matrix
  # q.skel, efficacy skeleton(s) as vector or matrix
  # n.ar, the number of patients to use in the adaptive randomisation stage
  # comb.curr, deprecated, ignored
  # mprior.tox, prior for each toxicity ordering
  # mprior.eff, prior for each efficacy ordering
  # safety.confidence is the % CI to construct when checking whether lowest dose
  #   is too toxic
  # futility.confidence is the % CI to construct when checking whether current
  #   dose is too toxic
  # check.tox.at.dose.level is the index of the dose on which design
  #   should invoke the logic to detect excess toxicity. In the majority of
  #   cases this will be dose-level 1. If you have a trivial lowest dose ("d=0")
  #   so that toxicity is nearly impossible, you will want to check for excess
  #   tox at dose 2 instead (lest you short-circuit the early stopping criteria)
  #   This is KB's embellishment.
  
  # lowest.is.placebo is an indicator to identify if the lowest dose is set to 
  #   be a control/placebo dose 
  # placebo.rand.prob is the probability of allocating to the placebo/control 
  #   dose level
  
  comb.curr = NULL
  
  if(is.vector(p.skel)) p.skel = t(as.matrix(p.skel));
  if(is.vector(q.skel)) q.skel = t(as.matrix(q.skel));
  
  ncomb = ncol(p.skel);   # number of doses or combinations
  ptox.hat = numeric(ncomb); # estimate of toxicity prob
  peff.hat = numeric(ncomb); # estimate of efficacy prob
  nord.tox = nrow(p.skel);
  if(is.null(mprior.tox))
    mprior.tox = rep(1 / nord.tox, nord.tox)
  nord.eff = nrow(q.skel);
  if(is.null(mprior.eff))
    mprior.eff = rep(1 / nord.eff, nord.eff)
  
  marginal.tox = rep(0, nord.tox);
  for(k in 1:nord.tox) {
    marginal.tox[k] = integrate(post.tox, lower=-Inf, upper=Inf, p=p.skel[k,], 
                                y=y, n=n)$value;
  }
  postprob.tox = (marginal.tox * mprior.tox) / sum(marginal.tox * mprior.tox);
  
  marginal.eff = rep(0, nord.eff);
  for(k in 1:nord.eff) {
    marginal.eff[k] = integrate(post.eff,lower=-Inf,upper=Inf, q=q.skel[k,], 
                                z=z, n=n)$value;
  }
  postprob.eff = (marginal.eff * mprior.eff) / sum(marginal.eff * mprior.eff);
  
  # toxicity model selection, identify the model with the highest posterior prob
  if(nord.tox > 1) { 
    mtox.sel = which.is.max(postprob.tox); 
  } 
  else {
    mtox.sel = 1;
  }
  
  # efficacy model selection, identify the model with the highest posterior prob
  if(nord.eff > 1){
    meff.sel = which.is.max(postprob.eff); 
  } 
  else {
    meff.sel = 1;
  }
  
  # calculate posterior mean of toxicity probability at each combo
  for(j in 1:ncomb){
    ptox.hat[j] = integrate(posttoxf, lower=-Inf, upper=Inf, p.skel[mtox.sel,], 
                            y, n, j)$value / marginal.tox[mtox.sel]; 
  }
  
  # calculate posterior mean of efficacy probability at each combo
  for(j in 1:ncomb){
    peff.hat[j] = integrate(postefff, lower=-Inf, upper=Inf, q.skel[meff.sel,], 
                            z, n, j)$value / marginal.eff[meff.sel]; 
  }
  
  # determine set of safe doses based on toxicity 
  aset = which(ptox.hat <= tul)
  if(length(aset)==0) { 
    aset = which.min(ptox.hat) 
  }
  
  peff.hat.aset = rep(0, ncomb)
  peff.hat.aset[aset] = peff.hat[aset]
  # deterimine randomisation probabilities based on efficacy 
  ar.prob = peff.hat.aset / sum(peff.hat.aset)
  
  if(lowest.is.placebo == TRUE){
    ar.prob = c(placebo.rand.prob, 
                ar.prob[-1]*(1-placebo.rand.prob)/sum(ar.prob[-1]))
  }

  
  if(length(aset) == 1) {
    # The best and recommended dose can only be this
    comb.best <- aset
    comb.curr <- aset
  } 
  else {
    # The best dose is always that in admissible set with maximal Pr(Eff)
    comb.best <- which.max(peff.hat.aset)
    # In AR stage, sample current dose from admissible doses with weighted prob;
    # After AR stage, current dose is best dose
    ifelse(sum(n) < n.ar, 
           comb.curr <- sample(1:ncomb, 1, prob=ar.prob),
           comb.curr <- comb.best)
  }
  
  max.dose.given <- max(seq(1, ncomb)[n > 0])
  if(comb.curr > max.dose.given + 1) {
    # Recommending dose above maximum given, so prevent skipping
    comb.curr <- max.dose.given + 1 
    skipping_prevented = TRUE
  } else {
    skipping_prevented = FALSE
  }
  
  ########## Stopping rules
  stop = 0
  tox.dl = check.tox.at.dose.level
  safety = binom.confint(y[tox.dl], n[tox.dl], conf.level = safety.confidence, 
                         methods="exact")$lower
  if(safety > tul){
    stop = 1
  }
  
  if(sum(n) > n.ar){
    # We are not in the AR phase, so stopping for futility is used.
    futility = binom.confint(z[comb.curr], n[comb.curr], 
                             conf.level = futility.confidence, 
                             methods = "exact")$upper
    if(n[comb.curr] > 0 & futility < ell){
      # Recommended dose has been given; upper efficacy estimate is below limit
      
      # stop = 2  # KB - surely an error? The stop value is added below to a previous value 
      # so it should only be 0 or 1? Maybe the logic NW wanted was ">0 => stop".
      stop = 1
    }
  }
  else {
    futility = NULL
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
    Stop = stop,
    SkippingPrevented = skipping_prevented,
    Note = "The parameter Dose1ToxLowerBound is a misnomer.
    Use LowestActiveDoseToxLowerBound instead."
  ))
}



###Load the function 'bpocrm' 
wt.sim.one <- function(p0, q0, p.skel, q.skel, tul, ell, cohortsize, ncohort, 
                       start.comb, n.ar, mprior.tox = NULL, mprior.eff = NULL, 
                       safety.confidence = 0.95, futility.confidence = 0.95,
                       check.tox.at.dose.level = 1,
                       lowest.is.placebo = FALSE, placebo.rand.prob = NULL) {
  
  # p0, true toxicity probabilities
  # q0, true efficacy probabilities
  # p.skel, toxicity skeleton(s)
  # q.skel, efficacy skeleton(s)
  # tul, toxicity upper limit
  # ell, efficacy lower limit
  # cohortsize, obvious
  # ncohort, number of cohorts
  # start.comb, starting dose-level
  # n.ar, number of patients in adaptive randomisation stage
  # mprior.tox, Prior weights to toxicity models, uniform if omitted
  # mprior.eff, Prior weights to efficacy models, uniform if omitted
  # safety.confidence is the % CI to construct when checking whether lowest dose
  #   is too toxic
  # futility.confidence is the % CI to construct when checking whether current
  #   dose is too toxic
  # check.tox.at.dose.level is the index of the dose on which design
  #   should invoke the logic to detect excess toxicity. In the majority of
  #   cases this will be dose-level 1. If you have a trivial lowest dose ("d=0")
  #   so that toxicity is nearly impossible, you will want to check for excess
  #   tox at dose 2 instead (lest you short-circuit the early stopping criteria)
  #   This is KB's embellishment.
  
  # if a single ordering is inputed as a vector, convert it to a matrix
  if(is.vector(p.skel)) p.skel = t(as.matrix(p.skel));
  # if a single ordering is inputed as a vector, convert it to a matrix
  if(is.vector(q.skel)) q.skel = t(as.matrix(q.skel));
  
  ### run a trial 	
  ncomb = ncol(p.skel);   # number of combos
  y = rep(0, ncomb);  # number of toxicity/responses at each dose level
  z = rep(0, ncomb);   # number of efficacy at each dose level
  n = rep(0, ncomb);  # number of treated patients at each dose level
  comb.curr = start.comb;  # current dose level	 
  ptox.hat = numeric(ncomb); # estimate of toxicity prob
  peff.hat = numeric(ncomb); # estimate of efficacy prob
  comb.select = rep(0, ncomb); # a vector of indicators for dose selection
  stop = 0; # indicate if trial stops early
  tox.skel = rep(0, nrow(p.skel))
  eff.skel = rep(0, nrow(q.skel))
  
  for(i in 1:ncohort)
  {
    # generate data for a new cohort of patients
    y[comb.curr] = y[comb.curr] + rbinom(1, cohortsize, p0[comb.curr]);
    z[comb.curr] = z[comb.curr] + rbinom(1, cohortsize, q0[comb.curr]);
    n[comb.curr] = n[comb.curr] + cohortsize;
    
    # TODO (...something - but what?!)
    cohort.update = wages.tait(y, z, n, p.skel, q.skel, n.ar, 
                               comb.curr = comb.curr, 
                               mprior.tox = mprior.tox, mprior.eff = mprior.eff,
                               safety.confidence = safety.confidence, 
                               futility.confidence = futility.confidence,
                               check.tox.at.dose.level = check.tox.at.dose.level,
                               lowest.is.placebo = lowest.is.placebo, 
                               placebo.rand.prob = placebo.rand.prob
    )
    # cat('ToxSkel', cohort.update$ToxSkeleton, '\n')
    # cat('EffSkel', cohort.update$EffSkeleton, '\n')
    
    stop = cohort.update$Stop
    comb.curr = cohort.update$RecommendedDose
    tox.skel[cohort.update$ToxSkeleton] = tox.skel[cohort.update$ToxSkeleton] +1
    eff.skel[cohort.update$EffSkeleton] = eff.skel[cohort.update$EffSkeleton] +1
    
    if(stop > 0)
      break
  }
  
  if(stop == 0){
    comb.select[comb.curr] = comb.select[comb.curr] + 1;
  }
  
  # cat('FinalToxSkel', cohort.update$ToxSkeleton, '\n')
  # cat('FinalEffSkel', cohort.update$EffSkeleton, '\n')
  return(list(comb.select = comb.select, tox.data = y, eff.data = z, 
              pt.allocation = n, stop = stop, 
              ToxSkel = tox.skel, FinalToxSkel = cohort.update$ToxSkeleton,
              EffSkel = eff.skel, FinalEffSkel = cohort.update$EffSkeleton))
}
# Nolan originally called this bpocrm
bpocrm = wt.sim.one



###Load the function 'bpocrm.sim' 
wt.sim <- function(p0, q0, p.skel, q.skel, tul, ell, cohortsize, ncohort, 
                   start.comb, n.ar, ntrial, 
                   mprior.tox = NULL, mprior.eff = NULL, 
                   safety.confidence = 0.95, futility.confidence = 0.95,
                   check.tox.at.dose.level = 1, 
                   verbose = TRUE, really.verbose = FALSE,
                   full_output = FALSE, 
                   lowest.is.placebo = FALSE, placebo.rand.prob = NULL) {
  
  # p0, true toxicity probabilities
  # q0, true efficacy probabilities
  # p.skel, toxicity skeleton(s)
  # q.skel, efficacy skeleton(s)
  # tul, toxicity upper limit
  # ell, efficacy lower limit
  # cohortsize, obvious
  # ncohort, number of cohorts
  # start.comb, starting dose-level
  # n.ar, number of patients in adaptive randomisation stage
  # ntrial, number of trials to simulate
  # mprior.tox, Prior weights to toxicity models, uniform if omitted
  # mprior.eff, Prior weights to efficacy models, uniform if omitted
  # safety.confidence is the % CI to construct when checking whether lowest dose
  #   is too toxic
  # futility.confidence is the % CI to construct when checking whether current
  #   dose is too toxic
  # check.tox.at.dose.level is the index of the dose on which design
  #   should invoke the logic to detect excess toxicity. In the majority of
  #   cases this will be dose-level 1. If you have a trivial lowest dose ("d=0")
  #   so that toxicity is nearly impossible, you will want to check for excess
  #   tox at dose 2 instead (lest you short-circuit the early stopping criteria)
  #   This is KB's embellishment.
  
  StartTime = Sys.time()
  
  # if a single ordering is inputed as a vector, convert it to a matrix
  if(is.vector(p.skel)) p.skel = t(as.matrix(p.skel));
  # if a single ordering is inputed as a vector, convert it to a matrix
  if(is.vector(q.skel)) q.skel = t(as.matrix(q.skel));
  
  ncomb = length(p0)
  comb.select <-y <-z <-n <- matrix(nrow = ntrial, ncol = ncomb)
  tox.skel <- matrix(nrow = ntrial, ncol = nrow(p.skel))
  final.tox.skel <- rep(0, nrow(p.skel))
  print(final.tox.skel)
  eff.skel <- matrix(nrow = ntrial, ncol = nrow(q.skel))
  final.eff.skel <- rep(0, nrow(q.skel))
  nstop = 0
  
  for(i in 1:ntrial){
    if(verbose & i %% floor(ntrial / 10) == 0) print(i)
    result <- bpocrm(p0, q0, p.skel, q.skel, tul, ell, cohortsize, ncohort, 
                     start.comb, n.ar, mprior.tox = mprior.tox, 
                     mprior.eff = mprior.eff,
                     safety.confidence = safety.confidence, 
                     futility.confidence = futility.confidence,
                     check.tox.at.dose.level = check.tox.at.dose.level,
                     lowest.is.placebo = lowest.is.placebo, 
                     placebo.rand.prob = placebo.rand.prob)
    if(result$stop == 0) {
      comb.select[i,] = result$comb.select
    } else {
      comb.select[i,] = 0
    }
    y[i, ] <- result$tox.data
    z[i, ] <- result$eff.data
    n[i, ] <- result$pt.allocation
    tox.skel[i, ] <- result$ToxSkel
    final.tox.skel[result$FinalToxSkel] <- final.tox.skel[result$FinalToxSkel]+1
    eff.skel[i, ] <- result$EffSkel
    final.eff.skel[result$FinalEffSkel] <- final.eff.skel[result$FinalEffSkel]+1
    nstop <- nstop + result$stop

  }
  
  if(really.verbose) {
    cat("True tox probability: ", round(p0,3), sep="    ",  "\n");
    cat("True eff probability: ", round(q0,3), sep="    ",  "\n");
    cat("selection percentage: ", formatC(colMeans(comb.select)*100, digits=1, 
                                          format="f"), sep="    ",  "\n");
    cat("number of toxicities:    ", formatC(colMeans(y), digits=1, format="f"), 
        sep="    ",   "\n");
    cat("number of responses:    ", formatC(colMeans(z), digits=1, format="f"), 
        sep="    ",   "\n");
    cat("number of patients treated:     ", formatC(colMeans(n), digits=1, 
                                                    format="f"), sep="    ",   
        "\n");
    cat("percentage of stop:    ", nstop/ntrial*100, "\n");
  }
  
  # Return object
  l <- list(
    TrueTox = p0, 
    TrueEff = q0,
    ToxSkel = p.skel,
    EffSkel = q.skel,
    TUL = tul,
    ELL = ell,
    NumSims = ntrial,
    NumPatients = cohortsize * ncohort,
    CohortSize = cohortsize,
    NumCohorts = ncohort,
    SizeARPhase = n.ar,
    SafetyConfidence = safety.confidence,
    FutilityConfidence = futility.confidence,
    TreatedAtDose = round(colMeans(n), 1),
    ToxAtDose = round(colMeans(y), 1),
    EffAtDose = round(colMeans(z), 1),
    ProbSelect = round(colMeans(comb.select), 4),
    NStop = nstop,
    ProbStop = nstop / ntrial,
    
    CohortToxSkel = tox.skel,
    ProbFinalToxSkel = final.tox.skel / ntrial,
    CohortEffSkel = eff.skel,
    ProbFinalEffSkel = final.eff.skel / ntrial,
    SimulationTime = Sys.time()- StartTime
  )
  
  # Full output includes patients
  if(full_output) {
    l$FullTreatedAtDose = n
    l$FullToxAtDose = y
    l$FullEffAtDose = z
    l$FullRecommendation = comb.select
    l$FullStopTrial = stop_trial
  }
  
  return(l)
}
# Nolan originally called this bpocrm.sim
bpocrm.sim = wt.sim
 



