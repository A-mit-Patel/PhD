#Load RtC-WT functions 
source('RtC-WT.R')

###############################################################################
###############################################################################
# Standard Wages and Tait design for the two arm trial concept
###############################################################################
###############################################################################

# DESIGN SPECIFICATION

# Specify the total number of doses.
d <- 4
# Specify the number of toxicity orderings
s <- 1 
# Specify a set of toxicity skeleton values
p.skel <- c(0.1, 0.2, 0.3, 0.4)
# Specify the number of efficacy orderings 
g <- 7
# Specify the possible efficacy orderings of the drug combination
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c(0.4, 0.3, 0.2, 0.1)
q.skel[2,] <- c(0.3, 0.4, 0.3, 0.2)
q.skel[3,] <- c(0.2, 0.3, 0.4, 0.3)
q.skel[4,] <- c(0.1, 0.2, 0.3, 0.4)
q.skel[5,] <- c(0.2, 0.3, 0.4, 0.4)
q.skel[6,] <- c(0.3, 0.4, 0.4, 0.4)
q.skel[7,] <- c(0.4, 0.4, 0.4, 0.4)

# Toxicity upper limit 
tul <- 0.35
# Efficacy lower limit 
ell <- 0.15 
# Number of patients 
n <- 51
# Cohort size for each inclusion 
cohortsize <- 3

# Number of cohorts 
ncohort <- n/cohortsize

# Starting combination 
start.comb <- 1 

# Size of adaptive randomisation phase 
n.ar <-  24
  
# Stopping criteria 
safety.conf <- 0.90 
futility.conf <- 0.90 

# Repeatable simulations use seeds
seed <- 123
ntrial <- 10000

# SPECIFY SCENARIOS
# Specify true toxicity and efficay rates for each scenario

# Scenario 1 - good monotonic eff, tolerable
s1eff <- c(0.25, 0.35, 0.45, 0.55)
s1tox <- c(0.05, 0.10, 0.20, 0.25)

# Scenario 2 - good monotonic eff, toxic 
s2eff <- c(0.25, 0.35, 0.45, 0.55)
s2tox <- c(0.50, 0.55, 0.65, 0.75)

# Scenario 3 - ineffective, tolerable 
s3eff <- c(0.03, 0.06, 0.09, 0.12)
s3tox <- c(0.05, 0.10, 0.20, 0.25)

# Scenario 4 - ineffective, toxic 
s4eff <- c(0.03, 0.06, 0.09, 0.12)
s4tox <- c(0.50, 0.55, 0.65, 0.75)

# Scenario 5 - plateau@3, tolerable 
s5eff <- c(0.35, 0.55, 0.55, 0.55)
s5tox <- c(0.05, 0.10, 0.20, 0.25)

# Scenario 6 - plateau@3, toxic 
s6eff <- c(0.35, 0.55, 0.55, 0.55)
s6tox <- c(0.50, 0.55, 0.65, 0.75)

# Scenario 7 - plateau@4, tolerable 
s7eff <- c(0.25, 0.35, 0.55, 0.55)
s7tox <- c(0.05, 0.10, 0.20, 0.25)

# Scenario 8 - plateau@4, toxic 
s8eff <- c(0.25, 0.35, 0.55, 0.55)
s8tox <- c(0.50, 0.55, 0.65, 0.75)

# Scenario 9 - unimodal@3, tolerable
s9eff <- c(0.35, 0.55, 0.35, 0.25)
s9tox <- c(0.05, 0.10, 0.20, 0.25)

# Scenario 10 - unimodal@3, toxic
s10eff <- c(0.35, 0.55, 0.35, 0.25)
s10tox <- c(0.50, 0.55, 0.65, 0.75)

# Scenario 11 - unimodal@4, tolerable
s11eff <- c(0.25, 0.35, 0.55, 0.35)
s11tox <- c(0.05, 0.10, 0.20, 0.25)

#Scenario 12 - unimodal@4, toxic
s12eff <- c(0.25, 0.35, 0.55, 0.35)
s12tox <- c(0.50, 0.55, 0.65, 0.75)

# Record start time to determine time of simulations 
start.time <- Sys.time()

# Scenario 1 
set.seed(seed); TwoArmsims1 = wt.sim(s1tox, s1eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)
# Scenario 2 
set.seed(seed); TwoArmsims2 = wt.sim(s2tox, s2eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)

# Scenario 3 
set.seed(seed); TwoArmsims3 = wt.sim(s3tox, s3eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)

# Scenario 4 
set.seed(seed); TwoArmsims4 = wt.sim(s4tox, s4eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)

# Scenario 5 
set.seed(seed); TwoArmsims5 = wt.sim(s5tox, s5eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)

# Scenario 6 
set.seed(seed); TwoArmsims6 = wt.sim(s6tox, s6eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)

# Scenario 7 
set.seed(seed); TwoArmsims7 = wt.sim(s7tox, s7eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)

# Scenario 8 
set.seed(seed); TwoArmsims8 = wt.sim(s8tox, s8eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)

# Scenario 9 
set.seed(seed); TwoArmsims9 = wt.sim(s9tox, s9eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)

# Scenario 10 
set.seed(seed); TwoArmsims10 = wt.sim(s10tox, s10eff, p.skel, q.skel, tul, ell,
                                      cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                      ntrial = ntrial, safety.conf = safety.conf, 
                                      futility.conf = futility.conf,
                                      check.tox.at.dose.level = 1)

# Scenario 11 
set.seed(seed); TwoArmsims11 = wt.sim(s11tox, s11eff, p.skel, q.skel, tul, ell,
                                      cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                      ntrial = ntrial, safety.conf = safety.conf, 
                                      futility.conf = futility.conf,
                                      check.tox.at.dose.level = 1)

# Scenario 12 
set.seed(seed); TwoArmsims12 = wt.sim(s12tox, s12eff, p.skel, q.skel, tul, ell,
                                      cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                      ntrial = ntrial, safety.conf = safety.conf, 
                                      futility.conf = futility.conf,
                                      check.tox.at.dose.level = 1)

###############################################################################
###############################################################################
# Wages and Tait design with dose arbitrarily incuded as control
###############################################################################
###############################################################################

# DESIGN SPECIFICATION

# Specify the total number of doses.
d <- 5
# Specify the number of toxicity orderings
s <- 1 
# Specify a set of toxicity skeleton values
p.skel <- c(0.01, 0.1, 0.2, 0.3, 0.4)
# Specify the number of efficacy orderings 
g <- 7
# Specify the possible efficacy orderings of the drug combination
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c(0.01, 0.4, 0.3, 0.2, 0.1)
q.skel[2,] <- c(0.01, 0.3, 0.4, 0.3, 0.2)
q.skel[3,] <- c(0.01, 0.2, 0.3, 0.4, 0.3)
q.skel[4,] <- c(0.01, 0.1, 0.2, 0.3, 0.4)
q.skel[5,] <- c(0.01, 0.2, 0.3, 0.4, 0.4)
q.skel[6,] <- c(0.01, 0.3, 0.4, 0.4, 0.4)
q.skel[7,] <- c(0.01, 0.4, 0.4, 0.4, 0.4)

# Toxicity upper limit 
tul <- 0.35
# Efficacy lower limit 
ell <- 0.15 
# Number of patients 
n <- 60 
# Cohort size for each inclusion 
cohortsize <- 3
# Number of cohorts 
ncohort <- n/cohortsize
# Starting combination 
start.comb <- 2 

# Size of adaptive randomisation phase 
n.ar <- 45 

# Stopping criteria 
safety.conf <- 0.90 
futility.conf <- 0.90 

# Repeatable simulations use seeds
seed <- 123
ntrial <- 10000

# SPECIFY SCENARIOS
# Specify true toxicity and efficay rates for each scenario

# Scenario 1 - good monotonic eff, tolerable
s1eff <- c(0.01, 0.25, 0.35, 0.45, 0.55)
s1tox <- c(0.01, 0.05, 0.10, 0.20, 0.25)

# Scenario 2 - good monotonic eff, toxic 
s2eff <- c(0.01, 0.25, 0.35, 0.45, 0.55)
s2tox <- c(0.01, 0.50, 0.55, 0.65, 0.75)

# Scenario 3 - ineffective, tolerable 
s3eff <- c(0.01, 0.03, 0.06, 0.09, 0.12)
s3tox <- c(0.01, 0.05, 0.10, 0.20, 0.25)

# Scenario 4 - ineffective, toxic 
s4eff <- c(0.01, 0.03, 0.06, 0.09, 0.12)
s4tox <- c(0.01, 0.50, 0.55, 0.65, 0.75)

# Scenario 5 - plateau@3, tolerable 
s5eff <- c(0.01, 0.35, 0.55, 0.55, 0.55)
s5tox <- c(0.01, 0.05, 0.10, 0.20, 0.25)

# Scenario 6 - plateau@3, toxic 
s6eff <- c(0.01, 0.35, 0.55, 0.55, 0.55)
s6tox <- c(0.01, 0.50, 0.55, 0.65, 0.75)

# Scenario 7 - plateau@4, tolerable 
s7eff <- c(0.01, 0.25, 0.35, 0.55, 0.55)
s7tox <- c(0.01, 0.05, 0.10, 0.20, 0.25)

# Scenario 8 - plateau@4, toxic 
s8eff <- c(0.01, 0.25, 0.35, 0.55, 0.55)
s8tox <- c(0.01, 0.50, 0.55, 0.65, 0.75)

# Scenario 9 - unimodal@3, tolerable
s9eff <- c(0.01, 0.35, 0.55, 0.35, 0.25)
s9tox <- c(0.01, 0.05, 0.10, 0.20, 0.25)

# Scenario 10 - unimodal@3, toxic
s10eff <- c(0.01, 0.35, 0.55, 0.35, 0.25)
s10tox <- c(0.01, 0.50, 0.55, 0.65, 0.75)

# Scenario 11 - unimodal@4, tolerable
s11eff <- c(0.01, 0.25, 0.35, 0.55, 0.35)
s11tox <- c(0.01, 0.05, 0.10, 0.20, 0.25)

#Scenario 12 - unimodal@4, toxic
s12eff <- c(0.01, 0.25, 0.35, 0.55, 0.35)
s12tox <- c(0.01, 0.50, 0.55, 0.65, 0.75)

# Scenario 1 
set.seed(seed); WTsims1 = wt.sim(s1tox, s1eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = FALSE)
# Scenario 2 
set.seed(seed); WTsims2 = wt.sim(s2tox, s2eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = FALSE)

# Scenario 3 
set.seed(seed); WTsims3 = wt.sim(s3tox, s3eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = FALSE)

# Scenario 4 
set.seed(seed); WTsims4 = wt.sim(s4tox, s4eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = FALSE)

# Scenario 5 
set.seed(seed); WTsims5 = wt.sim(s5tox, s5eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = FALSE)

# Scenario 6 
set.seed(seed); WTsims6 = wt.sim(s6tox, s6eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = FALSE)

# Scenario 7 
set.seed(seed); WTsims7 = wt.sim(s7tox, s7eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = FALSE)

# Scenario 8 
set.seed(seed); WTsims8 = wt.sim(s8tox, s8eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = FALSE)

# Scenario 9 
set.seed(seed); WTsims9 = wt.sim(s9tox, s9eff, p.skel, q.skel, tul, ell,
                                 cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                 ntrial = ntrial, safety.conf = safety.conf, 
                                 futility.conf = futility.conf,
                                 check.tox.at.dose.level = 2,
                                 lowest.is.placebo = FALSE)

# Scenario 10 
set.seed(seed); WTsims10 = wt.sim(s10tox, s10eff, p.skel, q.skel, tul, ell,
                                 cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                 ntrial = ntrial, safety.conf = safety.conf, 
                                 futility.conf = futility.conf,
                                 check.tox.at.dose.level = 2,
                                 lowest.is.placebo = FALSE)

# Scenario 11 
set.seed(seed); WTsims11 = wt.sim(s11tox, s11eff, p.skel, q.skel, tul, ell,
                                 cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                 ntrial = ntrial, safety.conf = safety.conf, 
                                 futility.conf = futility.conf,
                                 check.tox.at.dose.level = 2,
                                 lowest.is.placebo = FALSE)

# Scenario 12 
set.seed(seed); WTsims12 = wt.sim(s12tox, s12eff, p.skel, q.skel, tul, ell,
                                 cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                 ntrial = ntrial, safety.conf = safety.conf, 
                                 futility.conf = futility.conf,
                                 check.tox.at.dose.level = 2,
                                 lowest.is.placebo = FALSE)



# Calculate duration of the simulations
duration <- Sys.time() - start.time

# Save data 
save.image(file = 'AltDesignsSimsData.RData')
