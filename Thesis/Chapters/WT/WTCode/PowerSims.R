#Load RtC-WT functions 
source('RtC-WT.R')

###############################################################################
###############################################################################
###############################################################################
###############################################################################

# DESIGN SPECIFICATION
# These details remain constant across all scenarios 

# Specify the total number of doses.
d <- 5
# Specify the number of toxicity orderings
s <- 1 
# Specify a set of toxicity skeleton values
p.skel <- c(0.1, 0.15, 0.25, 0.35, 0.45)
# Specify the number of efficacy orderings 
g <- 7
# Specify the possible efficacy orderings of the drug combination
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c(0.3, 0.7, 0.6, 0.5, 0.4)
q.skel[2,] <- c(0.3, 0.6, 0.7, 0.6, 0.5)
q.skel[3,] <- c(0.3, 0.5, 0.6, 0.7, 0.6)
q.skel[4,] <- c(0.3, 0.4, 0.5, 0.6, 0.7)
q.skel[5,] <- c(0.3, 0.5, 0.6, 0.7, 0.7)
q.skel[6,] <- c(0.3, 0.6, 0.7, 0.7, 0.7)
q.skel[7,] <- c(0.3, 0.7, 0.7, 0.7, 0.7)

# Toxicity upper limit 
tul <- 0.35
# Efficacy lower limit 
ell <- 0.5 
# Number of patients 
n <- 60 
# Cohort size for each inclusion 
cohortsize <- 3
# Number of cohorts 
ncohort <- n/cohortsize
# Starting combination 
start.comb <- 2 

# Stopping criteria 
safety.conf <- 0.90 
futility.conf <- 0.90 

# Repeatable simulations use seeds
seed <- 123
ntrial <- 10000

# SPECIFY SCENARIOS
# Specify true toxicity and efficacy rates for each scenario

# Scenario 1 - increasing monotonic eff, tolerable
s1eff <- c(0.3, 0.4, 0.5, 0.6, 0.7)
s1tox <- c(0.1, 0.2, 0.25, 0.3, 0.35)

# Scenario 3 - increasing monotonic eff, high doses toxic 
s3eff <- c(0.3, 0.4, 0.5, 0.6, 0.7)
s3tox <- c(0.1, 0.25, 0.35, 0.45, 0.55)

# Scenario 4 - unimodal, tolerable 
s4eff <- c(0.3, 0.4, 0.7, 0.5, 0.4)
s4tox <- c(0.1, 0.2, 0.25, 0.3, 0.35)

# Scenario 6 - unimodal, high doses toxic  
s6eff <- c(0.3, 0.4, 0.7, 0.5, 0.4)
s6tox <- c(0.1, 0.25, 0.35, 0.45, 0.55)

# Scenario 7 - plateau, tolerable 
s7eff <- c(0.3, 0.4, 0.6, 0.6, 0.6)
s7tox <- c(0.1, 0.2, 0.25, 0.3, 0.35)

# Scenario 9 - plateau, high doses toxic
s9eff <- c(0.3, 0.4, 0.6, 0.6, 0.6)
s9tox <- c(0.1, 0.25, 0.35, 0.45, 0.55)

# Scenario 10 - monotonic decreasing, tolerable
s10eff <- c(0.3, 0.7, 0.6, 0.5, 0.4)
s10tox <- c(0.1, 0.2, 0.25, 0.3, 0.35)

#Scenario 12 - monotonic decreasing, high doses toxic
s12eff <- c(0.3, 0.7, 0.6, 0.5, 0.4)
s12tox <- c(0.1, 0.25, 0.35, 0.45, 0.55)


# Fixed adaptive randomisation probability for control 
placebo.rand.prob <- 0.33 
# Size of Adaptive randomisation phase 
n.ar <- 30 

# Effect sizes for power calculations 
effect.sizes <- c(0.2, ES.h(0.5,0.3), 0.5, 0.8)

# Record the start time of simulations so duration can be calculated
start.time <- Sys.time()

# Scenario 1 
set.seed(seed); pwrsims1 = wt.sim(s1tox, s1eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = TRUE, 
                                    placebo.rand.prob = placebo.rand.prob,
                                    effect.sizes = effect.sizes)


# Scenario 3 
set.seed(seed); pwrsims3 = wt.sim(s3tox, s3eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = TRUE, 
                                    placebo.rand.prob = placebo.rand.prob,
                                    effect.sizes = effect.sizes)

# Scenario 4 
set.seed(seed); pwrsims4 = wt.sim(s4tox, s4eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = TRUE, 
                                    placebo.rand.prob = placebo.rand.prob,
                                    effect.sizes = effect.sizes)



# Scenario 6 
set.seed(seed); pwrsims6 = wt.sim(s6tox, s6eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = TRUE, 
                                    placebo.rand.prob = placebo.rand.prob,
                                    effect.sizes = effect.sizes)

# Scenario 7 
set.seed(seed); pwrsims7 = wt.sim(s7tox, s7eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = TRUE, 
                                    placebo.rand.prob = placebo.rand.prob,
                                    effect.sizes = effect.sizes)


# Scenario 9 
set.seed(seed); pwrsims9 = wt.sim(s9tox, s9eff, p.skel, q.skel, tul, ell,
                                    cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                    ntrial = ntrial, safety.conf = safety.conf, 
                                    futility.conf = futility.conf,
                                    check.tox.at.dose.level = 2,
                                    lowest.is.placebo = TRUE, 
                                    placebo.rand.prob = placebo.rand.prob,
                                    effect.sizes = effect.sizes)
# Scenario 10 
set.seed(seed); pwrsims10 = wt.sim(s10tox, s10eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 2,
                                     lowest.is.placebo = TRUE, 
                                     placebo.rand.prob = placebo.rand.prob,
                                     effect.sizes = effect.sizes)

# Scenario 12 
set.seed(seed); pwrsims12 = wt.sim(s12tox, s12eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 2,
                                     lowest.is.placebo = TRUE, 
                                     placebo.rand.prob = placebo.rand.prob,
                                     effect.sizes = effect.sizes)

# Calculate duration of the simulations
duration <- Sys.time() - start.time

# Save data 
save.image(file = 'PowerSimsData.RData')
