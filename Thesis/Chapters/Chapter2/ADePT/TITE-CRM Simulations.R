################################################################################
# Updated the two stage tite crm simulation function (applied_titecrmts_sim) 
# from the dtpcrm package to use custom weight function from ADePT. Also corrected
# the simulation of time to event data 
################################################################################
library(dtpcrm)
library(dfcrm)
source('Updated_titets_crm_sim.R')

###############################################################################
# Simulations
###############################################################################
# Specifications

# Specify the skeleton values.
prior <- getprior(0.05,0.25,5,6)
# Initial escalation scheme.
initdes <- c(2,2,2,3,3,3,4,4,4,5,5,5, rep(6,times=18))
# The target toxicity rate
target <- 0.25
# Max sample size 
max_sample_size <- 60
# Number of simulations
num_sims <- 2000
# The cohort size
cohort_size <- 3
# The Observation window specified in days add 49 due too 7 week treatment period
obswin <- 365 + 49
# The minimum follow up period specified in days
minfu <- 56 + 49
# The second observation window in days
win2 <- 84 +49 
# The number of patients recruited per observation window
recrate <- 14
# The value for which the estimated toxicity at the lowest dose is not to exceed
x <- 0.1 
tox_lim <- target + x 
# The probability value to be used when assessing the certainty required that toxicty at the specificed dose exceeds tox_lim
y <- 0.8
# The number of patients we need to see at a dose to stop the trial early
n.mtd <- 15
#stoprule 
stop_func <- function(x) {
  y = stop_for_excess_toxicity_empiric(x, tox_lim = tox_lim, prob_cert = y, 
                                       dose = 1, nsamps = 100000)
  if(length(y$level) == 3){
    y$stop<- FALSE
  }
  if(y$stop){
    x <- y
  } else {x = stop_for_consensus_reached(x, req_at_mtd = n.mtd)}
}

true_tox <- c(0.25, 0.40, 0.45, 0.50, 0.55, 0.60)
true_tox0 <- c(0.12 ,0.25, 0.40, 0.45, 0.50, 0.55)
true_tox1 <- c(0.09 ,0.12 ,0.25, 0.40, 0.45, 0.50)
true_tox2a <- c(0.06, 0.09, 0.12, 0.25, 0.40, 0.45) 
true_tox2b <- c(0.03, 0.06, 0.09, 0.12 ,0.25, 0.40)
true_tox3 <- c(0.01, 0.03, 0.06, 0.09, 0.12 ,0.25)
true_toxEq <-  c(0.05, 0.10, 0.15, 0.20 ,0.25 ,0.30)
true_toxAll <- c(0.50, 0.60, 0.65, 0.70 ,0.75 ,0.8)

set.seed(10)
s1 <- applied_titecrmts_sim_v2(true_tox = true_tox, prior = prior, target = target,
                               max_sample_size = max_sample_size, num_sims = num_sims,
                               cohort_size = cohort_size, obswin = obswin, 
                               minfu = minfu, win2 = win2, recrate = recrate, 
                               initdes = initdes, no_skip_esc = TRUE, 
                               no_skip_deesc = TRUE, stop_func = stop_func)
set.seed(10)
s2 <- applied_titecrmts_sim_v2(true_tox = true_tox0, prior = prior, target = target,
                               max_sample_size = max_sample_size, num_sims = num_sims,
                               cohort_size = cohort_size, obswin = obswin, 
                               minfu = minfu, win2 = win2, recrate = recrate, 
                               initdes = initdes, no_skip_esc = TRUE, 
                               no_skip_deesc = TRUE, stop_func = stop_func)
set.seed(10)
s3 <- applied_titecrmts_sim_v2(true_tox = true_tox1, prior = prior, target = target,
                               max_sample_size = max_sample_size, num_sims = num_sims,
                               cohort_size = cohort_size, obswin = obswin, 
                               minfu = minfu, win2 = win2, recrate = recrate, 
                               initdes = initdes, no_skip_esc = TRUE, 
                               no_skip_deesc = TRUE, stop_func = stop_func)
set.seed(10)
s4 <- applied_titecrmts_sim_v2(true_tox = true_tox2a, prior = prior, target = target,
                               max_sample_size = max_sample_size, num_sims = num_sims,
                               cohort_size = cohort_size, obswin = obswin, 
                               minfu = minfu, win2 = win2, recrate = recrate, 
                               initdes = initdes, no_skip_esc = TRUE, 
                               no_skip_deesc = TRUE, stop_func = stop_func)

set.seed(10)
s5 <- applied_titecrmts_sim_v2(true_tox = true_tox2b, prior = prior, target = target,
                               max_sample_size = max_sample_size, num_sims = num_sims,
                               cohort_size = cohort_size, obswin = obswin, 
                               minfu = minfu, win2 = win2, recrate = recrate, 
                               initdes = initdes, no_skip_esc = TRUE, 
                               no_skip_deesc = TRUE, stop_func = stop_func)
set.seed(10)
s6 <- applied_titecrmts_sim_v2(true_tox = true_tox3, prior = prior, target = target,
                               max_sample_size = max_sample_size, num_sims = num_sims,
                               cohort_size = cohort_size, obswin = obswin, 
                               minfu = minfu, win2 = win2, recrate = recrate, 
                               initdes = initdes, no_skip_esc = TRUE, 
                               no_skip_deesc = TRUE, stop_func = stop_func)
set.seed(10)
s7 <- applied_titecrmts_sim_v2(true_tox = true_toxEq, prior = prior, target = target,
                               max_sample_size = max_sample_size, num_sims = num_sims,
                               cohort_size = cohort_size, obswin = obswin, 
                               minfu = minfu, win2 = win2, recrate = recrate, 
                               initdes = initdes, no_skip_esc = TRUE, 
                               no_skip_deesc = TRUE, stop_func = stop_func)
set.seed(10)
s8 <- applied_titecrmts_sim_v2(true_tox = true_toxAll, prior = prior, target = target,
                               max_sample_size = max_sample_size, num_sims = num_sims,
                               cohort_size = cohort_size, obswin = obswin, 
                               minfu = minfu, win2 = win2, recrate = recrate, 
                               initdes = initdes, no_skip_esc = TRUE, 
                               no_skip_deesc = TRUE, stop_func = stop_func)

save.image(file = 'TITE_sims.RData')