source('WT-original/kb/version2/wagestait.R')

#Re-creating simulations produced by KB 


## Specify the total number of doses.
d <- 3
## Specify the number of toxicity orderings
s <- 1 
## Specify a set of toxicity skeleton values
p.skel <- c(0.01, 0.05, 0.10)
## Specify the nmber of efficacy orderings 
g <- 5
## Specify the possible efficacy orderings of the drug combination
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c(0.5, 0.7, 0.9)
q.skel[2,] <- c(0.5, 0.7, 0.7)
q.skel[3,] <- c(0.5, 0.5, 0.5)
q.skel[4,] <- c(0.5, 0.7, 0.5)
q.skel[5,] <- c(0.9, 0.7, 0.5)
## toxicity upper limit 
tul <- 0.15
## efficacy lower limit 
ell <- 0.5 
## number of patients 
n <- 32 
## cohort size for each inlusion 
cohortsize <- 1
## number of cohorts 
ncohort <- n/cohortsize
## starting combination 
start.comb <- 1 
## size of adaptive randomisation phase 
n.ar <- 16 
safety.conf <- 0.95 
futility.conf <- 0.95 

# Repeatable simulations use seeds
seed <- 123
ntrial <- 1000

# Scenario 1
# Good, monotonic eff; tolerable
eff = c(0.6, 0.8, 1.0)
tox = c(0.01, 0.05, 0.075)
set.seed(seed); sims1 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)

# Scenario 2
# Good, monotonic eff; toxic
eff = c(0.6, 0.8, 1.0)
tox = c(0.35, 0.45, 0.55)
set.seed(seed); sims2 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)



# Scenario 3
# Ineffecive; toxic
eff = c(0.2, 0.3, 0.4)
tox = c(0.35, 0.45, 0.55)
set.seed(seed); sims3 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)



# Scenario 4
# Effective, plateau at 2; tolerable
eff = c(0.6, 0.8, 0.8)
tox = c(0.01, 0.05, 0.075)
set.seed(seed); sims4 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)


# Scenario 5
# Effective, plateau at 1; tolerable
eff = c(0.8, 0.8, 0.8)
tox = c(0.01, 0.05, 0.075)
set.seed(seed); sims5 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)

# Simulations for scenarios where placebo patients are just entered into the dose finding
# Only change that needs to be made is changing the size of N and scaling the AR phase

## number of patients 
n <- 52
## size of adaptive randomisation phase 
n.ar <- 26 

# Scenario 1
# Good, monotonic eff; tolerable
eff = c(0.6, 0.8, 1.0)
tox = c(0.01, 0.05, 0.075)
set.seed(seed); sims1_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)

# Scenario 2
# Good, monotonic eff; toxic
eff = c(0.6, 0.8, 1.0)
tox = c(0.35, 0.45, 0.55)
set.seed(seed); sims2_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)



# Scenario 3
# Ineffecive; toxic
eff = c(0.2, 0.3, 0.4)
tox = c(0.35, 0.45, 0.55)
set.seed(seed); sims3_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)



# Scenario 4
# Effective, plateau at 2; tolerable
eff = c(0.6, 0.8, 0.8)
tox = c(0.01, 0.05, 0.075)
set.seed(seed); sims4_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)


# Scenario 5
# Effective, plateau at 1; tolerable
eff = c(0.8, 0.8, 0.8)
tox = c(0.01, 0.05, 0.075)
set.seed(seed); sims5_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf)

save.image(file = 'Basic_Sims.RData')