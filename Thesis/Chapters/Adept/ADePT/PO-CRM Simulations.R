source('Functions.R')
library(dfcrm)
library(pocrm)

# Specifiy the possible orderings.
orders<-matrix(nrow=2,ncol=6)
orders[1,]<-c(1,2,3,4,5,6)
orders[2,]<-c(1,2,3,5,4,6)

# Specify the skeleton values.
skeleton <- getprior(0.05,0.25,5,6)
skeleton
# Initial guesses of toxicity probabilities for each ordering.
alpha <- getwm(orders,skeleton)
# We consider all orders to be equally likely prior to the study.
prior.o <- rep(1/2,2)
# Initial escalation scheme.
x0 <- c(2,3,4,5,6)
# Number of patients used to define stopping rule
stop <- 61
# Maximum sample size.
n <- 60
# The target toxicity rate
theta <- 0.25
# Number of simulations
nsim <- 2000
# Definition of acceptable DLT rates
tox.range<-0.05 
# The cohort size
cohort <- 3
# The number of patients we need to see at a dose to stop the trial early
mtd.lim <- 15
# The value for which the estimated toxicity at the lowest dose is not to exceed
tox.lim <- 0.35
# The probability value to be used when assessing the certainty required that toxicty at the specificed dose exceeds tox_lim
tox.cert <- 0.8

# True toxicity rates of Scenario 1
r <- c(0.25, 0.40, 0.45, 0.50, 0.55, 0.60)
set.seed(101)
po_s1 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort,  mtd.lim, tox.lim, tox.cert)
po_s1


  # True toxicity rates of scenario 2
r <- c(0.12 ,0.25, 0.40, 0.45, 0.50, 0.55)
set.seed(102)
po_s2 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort,  mtd.lim, tox.lim, tox.cert)
po_s2

# True toxicity rates of scenario 3
r <- c(0.09 ,0.12 ,0.25, 0.40, 0.45, 0.50)
set.seed(103)
po_s3 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, mtd.lim, tox.lim, tox.cert)
po_s3

# True toxicity rates of Scenario 4
r <- c(0.06, 0.09, 0.12, 0.25, 0.40, 0.45)
set.seed(104)
po_s4 <-pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                       tox.range, cohort, mtd.lim, tox.lim, tox.cert)
po_s4

# True toxicity rates of scenario 5
r <- c(0.03, 0.06, 0.09, 0.12 ,0.25, 0.40)
set.seed(105)
po_s5 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, mtd.lim, tox.lim, tox.cert)
po_s5

# True toxicity rates of scenario 6
r <- c(0.01, 0.03, 0.06, 0.09, 0.12 ,0.25)
set.seed(106)
po_s6 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, mtd.lim, tox.lim, tox.cert)
po_s6

# True toxicity rates of scenario 7
r <- c(0.05, 0.10, 0.15, 0.20 ,0.25 ,0.30)
set.seed(107)
po_s7 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort,  mtd.lim, tox.lim, tox.cert)
po_s7

# True toxicity rates when all too toxic
r <- c(0.50, 0.60, 0.65, 0.70 ,0.75 ,0.8)
set.seed(108)
po_s8 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, mtd.lim, tox.lim, tox.cert)
po_s8

save.image(file = 'PO-CRM_sims.RData')

