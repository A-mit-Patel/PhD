source('WT-original/kb/version2/wagestait.R')

#Specify the total number of doses.
d <- 6
# Specify the number of toxicity orderings
s <- 1 
# Specify a set of toxicity skeleton values
p.skel <- c(0.01, 0.08, 0.15, 0.22,0.29,0.36)
# Specify the number of efficacy orderings 
g <- 11
# Specify the possible efficacy orderings of the drug combination
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c( 0.6,0.5,0.4,0.3,0.2,0.1)
q.skel[2,] <- c( 0.5,0.6,0.5,0.4,0.3,0.2)
q.skel[3,] <- c( 0.4,0.5,0.6,0.5,0.4,0.3)
q.skel[4,] <- c( 0.3,0.4,0.5,0.6,0.5,0.4)
q.skel[5,] <- c( 0.2,0.3,0.4,0.5,0.6,0.5)
q.skel[6,] <- c( 0.1,0.2,0.3,0.4,0.5,0.6)

q.skel[7,] <- c( 0.2,0.3,0.4,0.5,0.6,0.6)
q.skel[8,] <- c( 0.3,0.4,0.5,0.6,0.6,0.6)
q.skel[9,] <- c( 0.4,0.5,0.6,0.6,0.6,0.6)
q.skel[10,] <- c( 0.5,0.6,0.6,0.6,0.6,0.6)
q.skel[11,] <- c( 0.6,0.6,0.6,0.6,0.6,0.6)

# Toxicity upper limit 
tul <- 0.33
# Efficacy lower limit 
ell <- 0.05 
# Number of patients 
n <- 64
# Cohort size for each inclusion 
cohortsize <- 3

# Number of cohorts 
ncohort <- n/cohortsize

# Starting combination 
start.comb <- 1 

# Size of adaptive randomisation phase 
n.ar <-  16

# Stopping criteria 
safety.conf <- 0.90 
futility.conf <- 0.90 

# Repeatable simulations use seeds
seed <- 123
ntrial <- 1000

s1eff <- c(0.05, 0.13, 0.25, 0.38, 0.50, 0.63)
s1tox <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.33)

s1eff <- c( 0.25, 0.3, 0.4, 0.5, 0.6, 0.7)
s1tox <- c( 0.10, 0.15, 0.2, 0.25, 0.3, 0.33)


set.seed(seed); WT = wt.sim(s1tox, s1eff, p.skel, q.skel, tul, ell,
                                     cohortsize, ncohort, start.comb, n.ar=n.ar, 
                                     ntrial = ntrial, safety.conf = safety.conf, 
                                     futility.conf = futility.conf,
                                     check.tox.at.dose.level = 1)
