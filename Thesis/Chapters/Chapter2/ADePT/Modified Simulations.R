library(pocrm) 
library(dfcrm)
source('Functions.R')


# Specifiy the possible orderings.
orders<-matrix(nrow=2,ncol=6)
orders[1,]<-c(1,2,3,4,5,6)
orders[2,]<-c(1,2,3,5,4,6)

# Specify the skeleton values.
skeleton <- getprior(0.05,0.25,5,6)
# Initial guesses of toxicity probabilities for each ordering.
alpha <- getwm(orders,skeleton)
# We consider all orders to be equally likely prior to the study.
prior.o <- rep(1/2,2)
# Initial escalation scheme.
x0 <- c(2,3,4,5,6)
# The target toxicity rate
theta <- 0.25
# Number of simulations
nsim <- 2000
# The Observation window specified in days add 49 due too 7 week treatment period
obswin <- 365 + 49
# The minimum follow up period specified in days
minfu <- 56 + 49
# The second observation window in days
win2 <- 84 +49 
# The number of patients recruited per observation window
recrate <- 14 # Roughly 1 patient every 30 days
# Definition of acceptable DLT rates
tox.range<-0.05 
# The value for which the estimated toxicity at the lowest dose is not to exceed
tox.lim <- 0.35
# The probability value to be used when assessing the certainty required that toxicty at the specificed dose exceeds tox_lim
tox.cert <- 0.8

################################################################################
################################################################################
################################################################################
# Modification 1 Sample size of 30 patients - No stop for consensus

# Number of patients used to define stopping rule
stop <- 31
# Maximum sample size.
n <- 30
# The cohort size
cohort <- 3
# The number of patients we need to see at a dose to stop the trial early
mtd.lim <- 30

# True toxicity rates of scenario 1
r <- c(0.25, 0.40, 0.45, 0.50, 0.55, 0.60)
set.seed(101)
mod30_s1 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, obswin, minfu, win2, recrate,
                        mtd.lim, tox.lim, tox.cert)
mod30_s1

# True toxicity rates of scenario 2
r <- c(0.12 ,0.25, 0.40, 0.45, 0.50, 0.55)
set.seed(102)
mod30_s2 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, obswin, minfu, win2, recrate,
                        mtd.lim, tox.lim, tox.cert)
mod30_s2

# True toxicity rates of scenario 3
r <- c(0.09 ,0.12 ,0.25, 0.40, 0.45, 0.50)
set.seed(103)
mod30_s3 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, obswin, minfu, win2, recrate,
                        mtd.lim, tox.lim, tox.cert)
mod30_s3

# True toxicity rates of Scenario 4
r <- c(0.06, 0.09, 0.12, 0.25, 0.40, 0.45)
set.seed(104)
mod30_s4 <-titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                       tox.range, cohort, obswin, minfu, win2, recrate,                                
                       mtd.lim, tox.lim, tox.cert)
mod30_s4

# True toxicity rates of scenario 5
r <- c(0.03, 0.06, 0.09, 0.12 ,0.25, 0.40)
set.seed(105)
mod30_s5 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, obswin, minfu, win2, recrate,
                        mtd.lim, tox.lim, tox.cert)
mod30_s5

# True toxicity rates of scenario 6
r <- c(0.01, 0.03, 0.06, 0.09, 0.12 ,0.25)
set.seed(106)
mod30_s6 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, obswin, minfu, win2, recrate,
                        mtd.lim, tox.lim, tox.cert)
mod30_s6

# True toxicity rates of scenario 7
r <- c(0.05, 0.10, 0.15, 0.20 ,0.25 ,0.30)
set.seed(107)
mod30_s7 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, obswin, minfu, win2, recrate,
                        mtd.lim, tox.lim, tox.cert)
mod30_s7

# True toxicity rates when all too toxic
r <- c(0.50, 0.60, 0.65, 0.70 ,0.75 ,0.8)
set.seed(108)
mod30_s8 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, obswin, minfu, win2, recrate, 
                        mtd.lim, tox.lim, tox.cert)
mod30_s8

################################################################################
################################################################################
################################################################################

# Modification 2 Sample size of 60 patients - No stop for consensus

# Number of patients used to define stopping rule
stop <- 61
# Maximum sample size.
n <- 60
# The cohort size
cohort <- 3
# The number of patients we need to see at a dose to stop the trial early
mtd.lim <- 60

# True toxicity rates of scenario 1
r <- c(0.25, 0.40, 0.45, 0.50, 0.55, 0.60)
set.seed(101)
mod60_s1 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
mod60_s1

# True toxicity rates of scenario 2
r <- c(0.12 ,0.25, 0.40, 0.45, 0.50, 0.55)
set.seed(102)
mod60_s2 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
mod60_s2

# True toxicity rates of scenario 3
r <- c(0.09 ,0.12 ,0.25, 0.40, 0.45, 0.50)
set.seed(103)
mod60_s3 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
mod60_s3

# True toxicity rates of Scenario 4
r <- c(0.06, 0.09, 0.12, 0.25, 0.40, 0.45)
set.seed(104)
mod60_s4 <-titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                         tox.range, cohort, obswin, minfu, win2, recrate,                                
                         mtd.lim, tox.lim, tox.cert)
mod60_s4

# True toxicity rates of scenario 5
r <- c(0.03, 0.06, 0.09, 0.12 ,0.25, 0.40)
set.seed(105)
mod60_s5 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
mod60_s5

# True toxicity rates of scenario 6
r <- c(0.01, 0.03, 0.06, 0.09, 0.12 ,0.25)
set.seed(106)
mod60_s6 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
mod60_s6

# True toxicity rates of scenario 7
r <- c(0.05, 0.10, 0.15, 0.20 ,0.25 ,0.30)
set.seed(107)
mod60_s7 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
mod60_s7

# True toxicity rates when all too toxic
r <- c(0.50, 0.60, 0.65, 0.70 ,0.75 ,0.8)
set.seed(108)
mod60_s8 <- titepocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate, 
                          mtd.lim, tox.lim, tox.cert)
mod60_s8

################################################################################
################################################################################
################################################################################

# Modification 3 Removal of cohorts and minimum follow up  
# Number of patients used to define stopping rule
stop <- 61
# Maximum sample size.
n <- 60
# The cohort size
cohort <- 1
# The number of patients we need to see at a dose to stop the trial early
mtd.lim <- 15

r <- c(0.25, 0.40, 0.45, 0.50, 0.55, 0.60)
set.seed(101)
modco_s1 <- titepocrm_sim_nomin(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
modco_s1

# True toxicity rates of scenario 2
r <- c(0.12 ,0.25, 0.40, 0.45, 0.50, 0.55)
set.seed(102)
modco_s2 <- titepocrm_sim_nomin(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
modco_s2

# True toxicity rates of scenario 3
r <- c(0.09 ,0.12 ,0.25, 0.40, 0.45, 0.50)
set.seed(103)
modco_s3 <- titepocrm_sim_nomin(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
modco_s3

# True toxicity rates of Scenario 4
r <- c(0.06, 0.09, 0.12, 0.25, 0.40, 0.45)
set.seed(104)
modco_s4 <-titepocrm_sim_nomin(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                         tox.range, cohort, obswin, minfu, win2, recrate,                                
                         mtd.lim, tox.lim, tox.cert)
modco_s4

# True toxicity rates of scenario 5
r <- c(0.03, 0.06, 0.09, 0.12 ,0.25, 0.40)
set.seed(105)
modco_s5 <- titepocrm_sim_nomin(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
modco_s5

# True toxicity rates of scenario 6
r <- c(0.01, 0.03, 0.06, 0.09, 0.12 ,0.25)
set.seed(106)
modco_s6 <- titepocrm_sim_nomin(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
modco_s6

# True toxicity rates of scenario 7
r <- c(0.05, 0.10, 0.15, 0.20 ,0.25 ,0.30)
set.seed(107)
modco_s7 <- titepocrm_sim_nomin(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate,
                          mtd.lim, tox.lim, tox.cert)
modco_s7

# True toxicity rates when all too toxic
r <- c(0.50, 0.60, 0.65, 0.70 ,0.75 ,0.8)
set.seed(108)
modco_s8 <- titepocrm_sim_nomin(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                          tox.range, cohort, obswin, minfu, win2, recrate, 
                          mtd.lim, tox.lim, tox.cert)
modco_s8


save.image(file = 'Modified_sims.RData')