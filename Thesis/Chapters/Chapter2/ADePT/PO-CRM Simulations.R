library(pocrm)

pocrm_sim <- function (r, alpha, prior.o, x0, stop, n, theta, nsim, 
                           tox.range, cohort, 
                       mtd.lim, tox.lim, tox.cert)
{
  
  sim <- sim1 <- apred <- lik <- pord <- ord <- ahat <- rpred <- next.lev <- n1 <- N <- NULL
  d <- ncol(alpha)
  s <- nrow(alpha)
  if (nsim > 1) {
    lpocrm <- function(r, alpha, prior.o, x0, stop, n, theta) {
      if (is.vector(alpha)) 
        alpha = t(as.matrix(alpha))
      nord.tox = nrow(alpha)
      mprior.tox = prior.o
      # Function for calculating the likelihood
      # bcrml <- function(a, p1, y, w) {
      #   lik = 0
      #   for (j in 1:length(y)) {
      #     lik = lik + y[j] * a * log(w[j]*p1[j]) + 
      #       (1 - y[j]) * log((1 - w[j]*p1[j]^a))
      #   }
      #   return(lik)
      # }
      bcrml <- function(a, p1, y, n) {
        lik = 0
        for (j in 1:length(p1)) {
          lik = lik + y[j] * a * log(p1[j]) + (n[j] - 
                                                 y[j]) * log((1 - p1[j]^a))
        }
        return(lik)
      }
      
      
      # specifies the number of doses 
      ncomb = ncol(alpha)
      # Initialise empty vectors for use later on 
      # y (number of dlt's per dose), npts(number of patients per dose)
      y = npts = ptox.hat = numeric(ncomb)
      # The dose level to be selected (add 1 to indicate position to stop)
      comb.select = numeric(ncomb)
      # Starting dose taken from first element of the dose escalation scheme
      comb.curr = x0[1]
      stoprule = 0
      # Specifies the dose escelation scheme 
      stage1 <- c(x0, rep(ncol(alpha), n - length(x0)))
      # Counter for the number of cohorts
      cohort.count <- 1
      # Empty vectors to store dose recived and if dlt was observed
      dose <- dlt <- dlt_time <- fu <- tox <- weights <- vector()
      # Time to recruit new patients
      #rectime <- obswin/recrate
      
      while(length(dlt) < n){
        # Takes draws from a binomial distribution to determine if patients in   the cohort had a toxic outcome
        cohort_tox <- stats::rbinom(n = cohort, size = 1, prob = r[comb.curr])
        # Dummy variable to store DLT which will be updated once the time for           DLT has passed
        cohort_dlt <- cohort_tox
        # Patients in the same cohort recieve the same dose
        cohort_comb <- rep(comb.curr, cohort)
        
        # # Follow up time based on the time taken to recruit a patient
        # # First patient assumed to be recruited instantly
        # # Add the min fu time to indicate when analysis is done
        # cohort_fu <- (rectime*(cohort-1)) - (rectime * 0:(cohort - 1)) + minfu
        # # Time of DLT is recorded and set to be at any time during the obswin
        # cohort_dlt_time <- vector()
        # for (i in 1:cohort) {
        #   if(cohort_tox[i] == 0){ 
        #     cohort_dlt_time[i] <- NA  
        #   }
        #   else {
        #     cohort_dlt_time[i] <- runif(1,0, obswin)
        #   }
        # } 
        # # Stores the time DLT occurs for new cohort
        # dlt_time <- c(dlt_time, cohort_dlt_time)
        
        # Stores new cohorts toxicity 
        tox <- c(tox, cohort_tox)
        # Stores dose level for each new cohort  
        dose <- c(dose, cohort_comb)
        # Store dummy dlt for new cohort
        dlt <- c(dlt, cohort_dlt)
        # # Add on follow up time and recruitment time for previous cohorts
        # fu <- fu + (cohort * rectime) + minfu 
        # # store new cohorts follow up time
        # fu <- c(fu, cohort_fu)
        # # Loop through all the patients who will have a DLT
        # for(i in 1:length(fu)){
        #   if(tox[i] == 1){
        #     # If the follow up time is greater than the time the dlt was                    determined to occur
        #     if(fu[i] >= dlt_time[i]){
        #       # Change the follow up time to the obswin to indicate the dlt                   has now happened
        #       fu[i] <- obswin
        #       # As the dlt has happened it should now be stored for                           calculation in the likelihood
        #       dlt[i] <- 1
        #     }
        #   }
        # }
        # fu <- pmin(fu, obswin)
        # # Calculated weights where the min fu accounts for 60% and is 80% by            window 2
        # for (i in 1:length(fu)) {
        #   weights[i] <- 0.6 + 
        #     0.2*(min(win2 - minfu, fu[i]-minfu)/(win2 - minfu))+                          0.2*(max(fu[i]- win2, 0)/(obswin-win2))
        # }
        # These values are used to calculate patient allocation and %DLT
        y[comb.curr] = y[comb.curr] + sum(cohort_tox)
        npts[comb.curr] = npts[comb.curr] + cohort
        
        
        if (sum(dlt) == length(dlt)) {
          comb.curr <- ifelse(comb.curr == 1, comb.curr, 
                              comb.curr - 1)
        }
        else if (sum(dlt) == 0) {
          comb.curr <- ifelse(comb.curr == ncomb, comb.curr, 
                              stage1[cohort.count + 1])
        }
        else {
          break
        }
        
        # Stopping rule ends stage 1 when the same dose is recommended to a sixth cohort. As this follows the escalation scheme only stops when the max dose is prescribed to sixth cohort. 
        if(sum(dose == comb.curr) == mtd.lim ){
          break
        }
        
        
        cohort.count <- cohort.count +1
        
      } 
      
      
      while (length(dlt) <= n) {
        if (sum(dlt) == 0 ) {
          stop = 0
          break
        }
        else {
          like.tox = est.tox = rep(0, nord.tox)
          for (k in 1:nord.tox) {
            est.tox[k] <- optimize(f = bcrml, interval = c(0,100), 
                                   p1 = alpha[k, ], y = y, n = npts, 
                                   maximum = T)$maximum
            like.tox[k] <- optimize(f = bcrml, interval = c(0,100),
                                    p1 = alpha[k, ], y = y, n = npts, 
                                    maximum = T)$objective
          }
          
          postprob.tox = (exp(like.tox) * mprior.tox)/sum(exp(like.tox) * mprior.tox)
          
          
          if (nord.tox > 1) {
            mtox.sel = which.is.max(round(postprob.tox,8))
          }
          else {
            mtox.sel = 1
          }
          ptox.hat = alpha[mtox.sel, ]^est.tox[mtox.sel]
          loss = abs(ptox.hat - theta)
          comb.curr = which.is.max(round(-loss,8))
          # obtain the last dose level from the data
          last.lev <- tail(dose, n = 1) 
          if (comb.curr %in% dose | comb.curr < last.lev){
            # If the next recommended levl has already been tested or is lower than  
            # the current dose level than recommend next level
            comb.curr <- comb.curr
          }
          else {
            comb.curr <- max(dose) + 1
          }
          
          if (npts[comb.curr] == stop) {
            stoprule <- 0
            break
          }
          # Calculate asymptotic variance 
          beta <- alpha[mtox.sel,][dose]
          inverse.var <- 0
          for (i in 1:length(dlt)) { 
            inverse.var <- inverse.var + ((1-dlt[i])*(beta[i]^est.tox[mtox.sel])*(log((beta[i])))^2)/(1-((beta[i]^est.tox[mtox.sel])))^2
          }
          
          post.alpha.mean <- est.tox[mtox.sel]
          post.alpha.var <- 1/inverse.var
          post.alpha.samp <- rnorm(n = 10000, mean = post.alpha.mean, 
                                   sd = sqrt(post.alpha.var))
          post.prob.tox.samp <- alpha[mtox.sel,1]^post.alpha.samp
          prob.too.toxic <- mean(post.prob.tox.samp > tox.lim)
          
          if(is.na(prob.too.toxic)) {
            prob.too.toxic <- 1
          }
          
          # stopping rule for when lowest dose is too toxic AND lowest dose has been tested ptox.hat[1] > 0.5 & 1 %in% dose
          # 
          # stopping rule if dose is being recommended for a xth time
          if (length(dlt) == n |(prob.too.toxic > tox.cert) & 1 %in% dose |  sum(dose == comb.curr) == mtd.lim) {
            stoprule = 0

            break
          }
          
          
          else {
            # Takes draws from a binomial distribution to determine if patients in the cohort had a toxic outcome
            cohort_tox <- stats::rbinom(n = cohort, size = 1, prob = r[comb.curr])
            cohort_dlt <- rep(0, cohort)
            # Patients in the same cohort recieve the same dose
            cohort_comb <- rep(comb.curr, cohort)
           
            # Stores new cohorts toxicity 
            tox <- c(tox, cohort_tox)
            # Stores dose level for each new cohort  
            dose <- c(dose, cohort_comb)
            # Store dummy dlt for new cohort
            dlt <- c(dlt, cohort_dlt)
            
            y[comb.curr] = y[comb.curr] + sum(cohort_tox)
            npts[comb.curr] = npts[comb.curr] + cohort
          }
        }
      }
     
      if (stoprule == 0) {
        if (!exists("prob.too.toxic")){prob.too.toxic <- 0}
        if((prob.too.toxic > tox.cert)& 1 %in% dose){
          #comb.select[ncomb+1] <- 1
        }
        else {
          comb.select[comb.curr] = comb.select[comb.curr] + 1
          
        }
      }
      stop.count <- 0 
      if((prob.too.toxic > tox.cert) & 1 %in% dose){
        stop.count <- 1
      }
      
      duration <- 474 * length(tox)/3
      return(list(MTD.selection = comb.select, tox.data = y, 
                  patient.allocation = npts, stop = stop.count,
                  duration = duration))
    }
    
    
    
    
  }
  lpocrm.sim <- function(nsim) {
    ncomb = length(r)
    y <- npts <- matrix(nrow = nsim, ncol = ncomb)
    comb.select <- matrix(nrow = nsim, ncol = ncomb)
    duration <- stop.count <- trialsize <- rep(0, nsim)
    nstop = 0
    for (i in 1:nsim) {
      print(i)
      result <- lpocrm(r, alpha, prior.o, x0, stop, n, 
                       theta)
      comb.select[i, ] = result$MTD.selection
      y[i, ] = result$tox.data
      npts[i, ] = result$patient.allocation
      trialsize[i] = sum(result$patient.allocation)
      stop.count[i] = result$stop
      duration[i] = result$duration
    }
    return(list(true.prob = r, 
                MTD.selection = round(colMeans(comb.select),2),
                patient.allocation = 100* round(colMeans(npts)/mean(trialsize),2), 
                percent.DLT = sum(colMeans(y))/mean(trialsize), 
                stop = mean(stop.count), 
                months = 12*mean(duration)/365,
                mean.n = mean(trialsize), 
                mean.n.perdose = colMeans(npts),
                acceptable = sum(colMeans(comb.select)[which(round(abs(r - theta), 2) <= tox.range)])))
  }
  if (nsim == 1) {
    twostgcrm(r, x0, stop, n, theta)
  }
  else {
    lpocrm.sim(nsim)
  }
}

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
set.seed(10)
fit_s1 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort,  mtd.lim, tox.lim, tox.cert)
fit_s1


  # True toxicity rates of scenario 2
r <- c(0.12 ,0.25, 0.40, 0.45, 0.50, 0.55)
set.seed(10)
fit_s2 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort,  mtd.lim, tox.lim, tox.cert)
fit_s2

# True toxicity rates of scenario 3
r <- c(0.09 ,0.12 ,0.25, 0.40, 0.45, 0.50)
set.seed(10)
fit_s3 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, mtd.lim, tox.lim, tox.cert)
fit_s3

# True toxicity rates of Scenario 4
r <- c(0.06, 0.09, 0.12, 0.25, 0.40, 0.45)
set.seed(10)
fit_s4 <-pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                       tox.range, cohort, mtd.lim, tox.lim, tox.cert)
fit_s4

# True toxicity rates of scenario 5
r <- c(0.03, 0.06, 0.09, 0.12 ,0.25, 0.40)
set.seed(10)
fit_s5 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, mtd.lim, tox.lim, tox.cert)
fit_s5

# True toxicity rates of scenario 6
r <- c(0.01, 0.03, 0.06, 0.09, 0.12 ,0.25)
set.seed(10)
fit_s6 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, mtd.lim, tox.lim, tox.cert)
fit_s6

# True toxicity rates of scenario 7
r <- c(0.05, 0.10, 0.15, 0.20 ,0.25 ,0.30)
set.seed(10)
fit_s7 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort,  mtd.lim, tox.lim, tox.cert)
fit_s7

# True toxicity rates when all too toxic
r <- c(0.50, 0.60, 0.65, 0.70 ,0.75 ,0.8)
set.seed(10)
fit_s8 <- pocrm_sim(r, alpha, prior.o, x0, stop, n, theta, nsim, 
                        tox.range, cohort, mtd.lim, tox.lim, tox.cert)
fit_s8

save.image(file = 'PO-CRM sims.RData')

