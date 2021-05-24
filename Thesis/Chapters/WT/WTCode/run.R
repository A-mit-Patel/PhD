#source('WT-original/kb/version2/wagestait.R')

#Re-creating simulations produced by KB 


## Specify the total number of doses.
d <- 4
## Specify the number of toxicity orderings
s <- 1 
## Specify a set of toxicity skeleton values
p.skel <- c(0.01, 0.05, 0.10, 0.15)
## Specify the nmber of efficacy orderings 
g <- 5
## Specify the possible efficacy orderings of the drug combination
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c(0.1, 0.5, 0.7, 0.9)
q.skel[2,] <- c(0.1, 0.5, 0.7, 0.7)
q.skel[3,] <- c(0.1, 0.5, 0.5, 0.5)
q.skel[4,] <- c(0.1, 0.5, 0.7, 0.5)
q.skel[5,] <- c(0.1, 0.9, 0.7, 0.5)
## toxicity upper limit 
tul <- 0.15
## efficacy lower limit 
ell <- 0.5 
## number of patients 
n <- 52 
## cohort size for each inlusion 
cohortsize <- 1
## number of cohorts 
ncohort <- n/cohortsize
## starting combination 
start.comb <- 1 
## size of adaptive randomisation phase 
n.ar <- 52 
safety.conf <- 0.95 
futility.conf <- 0.95 

# Repeatable simulations use seeds
seed <- 123
ntrial <- 10000

# Scenario 1
# Good, monotonic eff; tolerable
eff = c(0.1, 0.6, 0.8, 1.0)
tox = c(0.01, 0.05, 0.075, 0.1)
set.seed(seed); sims1 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf,
                               check.tox.at.dose.level = 2,
                               lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)

# Scenario 2
# Good, monotonic eff; toxic
eff = c(0.1, 0.6, 0.8, 1.0)
tox = c(0.01, 0.35, 0.45, 0.55)
set.seed(seed); sims2 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf,
                               check.tox.at.dose.level = 2,
                               lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)



# Scenario 3
# Ineffecive; toxic
eff = c(0.1, 0.2, 0.3, 0.4)
tox = c(0.01, 0.35, 0.45, 0.55)
set.seed(seed); sims3 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf,
                               check.tox.at.dose.level = 2,
                               lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)



# Scenario 4
# Effective, plateau at 2; tolerable
eff = c(0.1, 0.6, 0.8, 0.8)
tox = c(0.01, 0.05, 0.075, 0.1)
set.seed(seed); sims4 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf,
                               check.tox.at.dose.level = 2,
                               lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)


# Scenario 5
# Effective, plateau at 1; tolerable
eff = c(0.1, 0.8, 0.8, 0.8)
tox = c(0.01, 0.05, 0.075, 0.1)
set.seed(seed); sims5 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                               ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                               safety.conf=safety.conf, futility.conf=futility.conf,
                               check.tox.at.dose.level = 2,
                               lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)

# Simulations for scenarios where placebo patients are just entered into the dose finding
# Only change that needs to be made is changing the size of N and scaling the AR phase

## number of patients 
n <- 52
## size of adaptive randomisation phase 
n.ar <- 26 

# Scenario 1
# Good, monotonic eff; tolerable
eff = c(0.1, 0.6, 0.8, 1.0)
tox = c(0.01, 0.05, 0.075, 0.1)
set.seed(seed); sims1_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                                   ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                                   safety.conf=safety.conf, futility.conf=futility.conf,
                                   check.tox.at.dose.level = 2,
                                   lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)

# Scenario 2
# Good, monotonic eff; toxic
eff = c(0.1, 0.6, 0.8, 1.0)
tox = c(0.01, 0.35, 0.45, 0.55)
set.seed(seed); sims2_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                                   ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                                   safety.conf=safety.conf, futility.conf=futility.conf,
                                   check.tox.at.dose.level = 2,
                                   lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)



# Scenario 3
# Ineffecive; toxic
eff = c(0.1, 0.2, 0.3, 0.4)
tox = c(0.01, 0.35, 0.45, 0.55)
set.seed(seed); sims3_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                                   ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                                   safety.conf=safety.conf, futility.conf=futility.conf,
                                   check.tox.at.dose.level = 2,
                                   lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)



# Scenario 4
# Effective, plateau at 2; tolerable
eff = c(0.1, 0.6, 0.8, 0.8)
tox = c(0.01, 0.05, 0.075, 0.1)
set.seed(seed); sims4_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                                   ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                                   safety.conf=safety.conf, futility.conf=futility.conf,
                                   check.tox.at.dose.level = 2,
                                   lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)


# Scenario 5
# Effective, plateau at 1; tolerable
eff = c(0.1, 0.8, 0.8, 0.8)
tox = c(0.01, 0.05, 0.075, 0.1)
set.seed(seed); sims5_n52 = wt.sim(tox, eff, p.skel, q.skel, tul, ell, cohortsize, 
                                   ncohort, start.comb, n.ar=n.ar, ntrial=ntrial, 
                                   safety.conf=safety.conf, futility.conf=futility.conf,
                                   check.tox.at.dose.level = 2,
                                   lowest.is.placebo = TRUE, placebo.rand.prob = 0.2)


save.image(file = 'Simsv1.RData')


rbind(sims1$TrueTox, sims1$TrueEff,sims1$ProbSelect, sims1$TreatedAtDose, 
      sims1_n52$ProbSelect, sims1_n52$TreatedAtDose,
      sims2$TrueTox, sims2$TrueEff,sims2$ProbSelect, sims2$TreatedAtDose, 
      sims2_n52$ProbSelect, sims2_n52$TreatedAtDose,
      sims3$TrueTox, sims3$TrueEff,sims3$ProbSelect, sims3$TreatedAtDose, 
      sims3_n52$ProbSelect, sims3_n52$TreatedAtDose,
      sims4$TrueTox, sims4$TrueEff,sims4$ProbSelect, sims4$TreatedAtDose, 
      sims4_n52$ProbSelect, sims4_n52$TreatedAtDose,
      sims5$TrueTox, sims5$TrueEff,sims5$ProbSelect, sims5$TreatedAtDose, 
      sims5_n52$ProbSelect, sims5_n52$TreatedAtDose) %>% 
  data.frame() %>% 
  mutate(scenario = rep(c(1,2,3,4,5), each = 6), 
         details = rep(c('TrueTox', 'TrueEff', 'ar52PS', 'ar52NP', 'ar26PS', 
                         'ar26NP'), times = 5)) %>% 
  select(scenario, details, D1= X1, D2 = X2, D3 = X3, D4 = X4 ) %>% 
  View()
