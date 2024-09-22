# Simulation function for PO-TITE-CRMs
titepocrm_sim <- function (r, alpha, prior.o, x0, stop, n, theta, nsim, 
                           tox.range, cohort, obswin, minfu, win2, recrate, 
                           mtd.lim, tox.lim, tox.cert)
{
  start_time <- Sys.time()
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
      bcrml <- function(a, p1, y, w) {
        lik = 0
        for (j in 1:length(y)) {
          lik = lik + y[j] * a * log(w[j]*p1[j]) + 
            (1 - y[j]) * log((1 - w[j]*p1[j]^a))
        }
        return(lik)
      }
      # specifies the number of doses 
      ncomb = ncol(alpha)
      # Initialise empty vectors for use later on 
      # y (number of dlt's per dose), npts(number of patients per dose)
      y = npts = ptox.hat = numeric(ncomb)
      # The dose level to be selected (add 1 to indicate position to stop)
      comb.select = numeric(ncomb )
      # Starting dose taken from first element of the dose escalation scheme
      comb.curr = x0[1]
      stoprule = 0
      stop.count <- 0
      # Specifies the dose escelation scheme 
      stage1 <- c(x0, rep(ncol(alpha), n - length(x0)))
      # Counter for the number of cohorts
      cohort.count <- 1
      # Empty vectors to store dose recived and if dlt was observed
      duration <- dose <- dlt <- dlt_time <- fu <- tox <- weights <- vector()
      # Time to recruit new patients
      rectime <- obswin/recrate
      
      while(length(tox) < n){
        # Takes draws from a binomial distribution to determine if patients in the cohort had a toxic outcome
        cohort_tox <- stats::rbinom(n = cohort, size = 1, prob = r[comb.curr])
        # Dummy variable to store DLT which will be updated once the time for DLT has passed
        cohort_dlt <- rep(0, cohort)
        # Patients in the same cohort recieve the same dose
        cohort_comb <- rep(comb.curr, cohort)
        # Follow up time based on the time taken to recruit a patient
        # First patient assumed to be recruited instantly
        # Add the min fu time to indicate when analysis is done
        cohort_fu <- (rectime*(cohort-1)) - (rectime * 0:(cohort - 1)) + minfu
        # Time of DLT is recorded and set to be at any time during the obswin
        cohort_dlt_time <- vector()
        for (i in 1:cohort) {
          if(cohort_tox[i] == 0){ 
            cohort_dlt_time[i] <- NA  
          }
          else {
            cohort_dlt_time[i] <- runif(1,0, obswin)
          }
        } 
        # Stores the time DLT occurs for new cohort
        dlt_time <- c(dlt_time, cohort_dlt_time)
        # Stores new cohorts toxicity 
        tox <- c(tox, cohort_tox)
        # Stores dose level for each new cohort  
        dose <- c(dose, cohort_comb)
        # Store dummy dlt for new cohort
        dlt <- c(dlt, cohort_dlt)
        # Add on follow up time and recruitment time for previous cohorts
        fu <- fu + (rectime*(cohort-1)) + minfu 
        #duration <- duration + (cohort * rectime) + minfu 
        # store new cohorts follow up time
        fu <- c(fu, cohort_fu)
        #duration <- c(duration, cohort_fu)
        # Loop through all the patients who will have a DLT
        for(i in 1:length(fu)){
          if(tox[i] == 1){
            # If the follow up time is greater than the time the dlt was determined to occur
            if(fu[i] >= dlt_time[i]){
              # Change the follow up time to the obswin to indicate the dlt has now happened
              fu[i] <- obswin
              # As the dlt has happened it should now be stored for calculation in the likelihood
              dlt[i] <- 1
            }
          }
        }
        
        fu <- pmin(fu, obswin)
        # Calculated weights where the min fu accounts for 60% and is 80% by            window 2
        for (i in 1:length(fu)) {
          weights[i] <- 0.6 + 
            0.2*(min(win2 - minfu, fu[i]-minfu)/(win2 - minfu))+ 0.2*(max(fu[i]- win2, 0)/(obswin-win2))
        }
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
            beta <- alpha[k,][dose[1:length(dose)]]
            est.tox[k] <- optimize(f = bcrml, interval = c(0,500), 
                                   p1 = beta, y = dlt, w = weights, 
                                   maximum = T)$maximum
            like.tox[k] <- optimize(f = bcrml, interval = c(0,500), 
                                    p1 = beta, y = dlt, w = weights,
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
            inverse.var <- inverse.var + ((1-dlt[i])*(weights[i]*beta[i]^est.tox[mtox.sel])*(log((weights[i]*beta[i])))^2)/(1-((weights[i]*beta[i]^est.tox[mtox.sel])))^2
          }
          
          post.alpha.mean <- est.tox[mtox.sel]
          post.alpha.var <- ifelse(inverse.var == 0, 0 , 1/inverse.var)
          post.alpha.samp <- rnorm(n = 100000, mean = post.alpha.mean, 
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
            
            # once all patients recruited calculate likelihoods at max weight
            like.tox = est.tox = rep(0, nord.tox)
            for (k in 1:nord.tox) {
              
              #set all the weights at 1
              weights <- rep(1, length(tox))
              #ensures that all those predicted to have toxic event is included in dlt data
              # as we are assuming full follow up once the last patients are recruited
              dlt <- tox
              beta <- alpha[k,][dose[1:length(dose)]]
              est.tox[k] <- optimize(f = bcrml, interval = c(0,500), 
                                     p1 = beta, y = tox, w = weights, 
                                     maximum = T)$maximum
              like.tox[k] <- optimize(f = bcrml, interval = c(0,500), 
                                      p1 = beta, y = tox, w = weights,
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
            
            # Calculate asymptotic variance 
            beta <- alpha[mtox.sel,][dose]
            inverse.var <- 0
            
          
              for (i in 1:length(tox)) {
                inverse.var <- inverse.var + ((1-tox[i])*(weights[i]*beta[i]^est.tox[mtox.sel])*(log((weights[i]*beta[i])))^2)/(1-((weights[i]*beta[i]^est.tox[mtox.sel])))^2
              }
              
              post.alpha.mean <- est.tox[mtox.sel]
              post.alpha.var <- ifelse(inverse.var == 0, 0 , 1/inverse.var)
              post.alpha.samp <- rnorm(n = 100000, mean = post.alpha.mean, 
                                       sd = sqrt(post.alpha.var))
              post.prob.tox.samp <- alpha[mtox.sel,1]^post.alpha.samp
              prob.too.toxic <- mean(post.prob.tox.samp > tox.lim)
              

              if(is.na(prob.too.toxic)) {
                prob.too.toxic <- 1
              }
           

            break
          }
          
          
          else {
            # Takes draws from a binomial distribution to determine if patients in the cohort had a toxic outcome
            cohort_tox <- stats::rbinom(n = cohort, size = 1, prob = r[comb.curr])
            cohort_dlt <- rep(0, cohort)
            # Patients in the same cohort recieve the same dose
            cohort_comb <- rep(comb.curr, cohort)
            # Follow up time based on the time taken to recruit a patient
            # First patient assumed to be recruited instantly
            # Add the min fu time to indicate when analysis is done
            cohort_fu <- (rectime*(cohort-1))-(rectime * 0:(cohort - 1))+minfu
            # Time of DLT is recorded and set between the time the patient has been followed up and the remaining observation window
            cohort_dlt_time <- vector()
            for (i in 1:cohort) {
              if(cohort_tox[i] == 0){ 
                cohort_dlt_time[i] <- NA  
              }
              else {
                cohort_dlt_time[i] <- runif(1,0, obswin)
              }
            } 
            # Stores the time DLT occurs for new cohort
            dlt_time <- c(dlt_time, cohort_dlt_time)
            # Stores new cohorts toxicity 
            tox <- c(tox, cohort_tox)
            # Stores dose level for each new cohort  
            dose <- c(dose, cohort_comb)
            # Store dummy dlt for new cohort
            dlt <- c(dlt, cohort_dlt)
            # Add on follow up time and recruitment time for previous cohorts
            fu <- fu + (rectime*(cohort-1)) + minfu 
            #duration <- duration + (cohort * rectime) + minfu
            # store new cohorts follow up time
            fu <- c(fu, cohort_fu)
            #duration <- c(duration, cohort_fu)
            for(i in 1:length(fu)){
              if(tox[i] == 1){
                if(fu[i] >= dlt_time[i]){
                  fu[i] <- obswin
                  dlt[i] <- 1
                }
              }
            }
            
            fu <- pmin(fu, obswin)
            # Calculated weights where the min fu accounts for 60% and 80% by win2
            for (i in 1:length(fu)) {
              weights[i] <- 0.6 + 
                0.2*(min(win2 - minfu, fu[i]-minfu)/(win2 - minfu))+ 0.2*(max(fu[i]- win2, 0)/(obswin-win2))
            }
            
            y[comb.curr] = y[comb.curr] + sum(cohort_tox)
            npts[comb.curr] = npts[comb.curr] + cohort
          }
        }
      }
      
      if (stoprule == 0) {
        if (!exists("prob.too.toxic")){prob.too.toxic <- 0}
        if((prob.too.toxic > tox.cert)& 1 %in% dose){
          stop.count <- 1 
        }
        else {
          comb.select[comb.curr] = comb.select[comb.curr] + 1
          
        }
      }
      
      # Calculate the duration of the trial 
      
      num_cohorts <- length(tox) / cohort
      duration <- cohort_fu[1] + (rectime*(cohort-1) + minfu) * (num_cohorts-1) + (obswin - minfu)
      return(list(MTD.selection = comb.select, tox.data = y, 
                  patient.allocation = npts, duration = duration,
                  stop = stop.count))
    }
    
    
    
    
  }
  lpocrm.sim <- function(nsim) {
    ncomb = length(r)
    y <- npts <- matrix(nrow = nsim, ncol = ncomb)
    comb.select <- matrix(nrow = nsim, ncol = ncomb )
    duration <- stop.count <- trialsize <- rep(0, nsim)
    nstop = 0
    for (i in 1:nsim) {
      #print(i)
      result <- lpocrm(r, alpha, prior.o, x0, stop, n, 
                       theta)
      comb.select[i, ] = result$MTD.selection
      y[i, ] = result$tox.data
      npts[i, ] = result$patient.allocation
      trialsize[i] = sum(result$patient.allocation)
      duration[i] = result$duration # convert duration to months
      stop.count[i] = result$stop
    }
    return(list(true.prob = r, 
                time = Sys.time() - start_time,
                MTD.selection = round(colMeans(comb.select),2),
                patient.allocation = 100* round(colMeans(npts)/mean(trialsize),2), 
                percent.DLT = sum(colMeans(y))/mean(trialsize), 
                months = 12*mean(duration)/365, 
                stop = mean(stop.count), 
                max.n.count = length(trialsize[trialsize == max(trialsize)]),
                summary.trialsize = summary(trialsize),
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


# Simulation function for PO-CRMs
pocrm_sim <- function (r, alpha, prior.o, x0, stop, n, theta, nsim, 
                       tox.range, cohort, 
                       mtd.lim, tox.lim, tox.cert)
{
  start_time <- Sys.time()
  sim <- sim1 <- apred <- lik <- pord <- ord <- ahat <- rpred <- next.lev <- n1 <- N <- NULL
  d <- ncol(alpha)
  s <- nrow(alpha)
  if (nsim > 1) {
    lpocrm <- function(r, alpha, prior.o, x0, stop, n, theta) {
      if (is.vector(alpha)) 
        alpha = t(as.matrix(alpha))
      nord.tox = nrow(alpha)
      mprior.tox = prior.o

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
        
        # Stores new cohorts toxicity 
        tox <- c(tox, cohort_tox)
        # Stores dose level for each new cohort  
        dose <- c(dose, cohort_comb)
        # Store dummy dlt for new cohort
        dlt <- c(dlt, cohort_dlt)
        
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
          post.alpha.samp <- rnorm(n = 100000, mean = post.alpha.mean, 
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
      
      duration <- 474 + 504*((length(tox)-1)/3)
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
                time = Sys.time()- start_time,
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

# Simulation function for two-stage TITE-CRM 
applied_titecrmts_sim_v2 <- function (true_tox, prior, target, max_sample_size,
                                      num_sims, win2, cohort = 1, obswin, minfu, 
                                      recrate, initdes, n.mtd,
                                      dose_func = applied_titecrm, ...) 
{
  iterations <- list()
  start_time <- Sys.time()
  for (s in 1:num_sims) {

    stop <- FALSE
    stop_reason <- NULL
    rectime <- obswin/recrate

    level <- initdes[1]
    stage1 <- c(initdes, rep(length(prior), max_sample_size - length(initdes)))
    duration <- dose <- dlt <- dlt_time <- fu <- tox <- weights <- vector()
    # Counter for the number of cohorts
    cohort.count <- 1
    # specifies the number of doses 
    ncomb <- length(true_tox)
    while (length(tox) < max_sample_size) {
      # Takes draws from a binomial distribution to determine if patients in the cohort had a toxic outcome
      cohort_tox <- stats::rbinom(n = cohort, size = 1, prob = true_tox[level])
      # Dummy variable to store DLT which will be updated once the time for           DLT has passed
      cohort_dlt <- rep(0, cohort)
      # Patients in the same cohort recieve the same dose
      cohort_level <- rep(level, cohort)
      # Follow up time based on the time taken to recruit a patient
      # First patient assumed to be recruited instantly
      # Add the min fu time to indicate when analysis is done
      cohort_fu <- (rectime*(cohort-1)) - (rectime * 0:(cohort - 1)) + minfu
      # Time of DLT is recorded and set to be at any time during the obswin
      cohort_dlt_time <- vector()
      for (i in 1:cohort) {
        if(cohort_tox[i] == 0){ 
          cohort_dlt_time[i] <- NA  
        }
        else {
          cohort_dlt_time[i] <- runif(1,0, obswin)
        }
      } 
      # Stores the time DLT occurs for new cohort
      dlt_time <- c(dlt_time, cohort_dlt_time)
      # Stores new cohorts toxicity 
      tox <- c(tox, cohort_tox)
      # Stores dose level for each new cohort  
      dose <- c(dose, cohort_level)
      # Store dummy dlt for new cohort
      dlt <- c(dlt, cohort_dlt)
      # Add on follow up time and recruitment time for previous cohorts
      fu <- fu + (rectime*(cohort-1)) + minfu 
      #duration <- duration + (cohort * rectime) + minfu 
      # store new cohorts follow up time
      fu <- c(fu, cohort_fu)
      #duration <- c(duration, cohort_fu)
      # Loop through all the patients who will have a DLT
      for(k in 1:length(fu)){
        if(tox[k] == 1){
          # If the follow up time is greater than the time the dlt was determined to occur
          if(fu[k] >= dlt_time[k]){
            # Change the follow up time to the obswin to indicate the dlt has now happened
            fu[k] <- obswin
            # As the dlt has happened it should now be stored for calculation in the likelihood
            dlt[k] <- 1
          }
        }
      }
      
      fu <- pmin(fu, obswin)
      # Calculated weights where the min fu accounts for 60% and is 80% bywindow 2
      for (a in 1:length(fu)) {
        weights[a] <- 0.6 + 
          0.2*(min(win2 - minfu, fu[a]-minfu)/(win2 - minfu))+ 
          0.2*(max(fu[a]- win2, 0)/(obswin-win2))
      }
      
      
      if (sum(dlt) == length(dlt)) {
        level <- ifelse(level == 1, level, 
                        level - 1)
      }
      else if (sum(dlt) == 0) {
        level <- ifelse(level == ncomb, level, 
                        stage1[cohort.count + 1])
      }
      else {
        break
      }
      
      # Stopping rule ends stage 1 when the same dose is recommended to a sixth cohort. As this follows the escalation scheme only stops when the max dose is prescribed to sixth cohort. 
      if(sum(dose == level) == n.mtd ){
        break
      }
      
      cohort.count <- cohort.count +1
    } 
    
    while ( !stop & length(tox) <= max_sample_size) {
      if (length(dlt) == max_sample_size & sum(dlt) == 0 ) {
        stop = TRUE
        break
      }
      else{
        x <- dose_func(prior = prior, target = target, weights = weights, 
                       tox = dlt, level = dose, followup = fu, obswin = obswin, 
                       ...)
        fdose <-  level <- x$mtd
        
        stop <- ifelse(is.null(x$stop), FALSE, x$stop)
        stop_reason <- x$stop_reason
        
        if(length(dlt) == max_sample_size | stop == TRUE ){
          x <- dose_func(prior = prior, target = target, tox = tox, 
                         level = dose, followup = rep(obswin, length(tox)), 
                         obswin = obswin, weights = rep(1, length(tox)),
                         ...)
          
          fdose <- x$mtd
          stop <- ifelse(is.null(x$stop), FALSE, x$stop)
          stop_reason <- x$stop_reason
          break
        }
        else{
          cohort_tox = stats::rbinom(n = cohort, size = 1, 
                                     prob = true_tox[level])
          cohort_dlt <- rep(0, cohort) ## 
          cohort_level = rep(level, cohort)
          cohort_fu = (rectime * (cohort - 1)) - (rectime * 
                                                    c(0:(cohort - 1))) + minfu
          cohort_dlt_time <- c()
          for (j in 1:cohort) {
            if(cohort_tox[j] == 0){
              cohort_dlt_time[j] <- NA
            }
            else {
              cohort_dlt_time[j] <- runif(1,0, obswin)
            }
          }
          # cohort_fu[cohort_tox == 1] <- obswin
          # Stores the time DLT occurs for new cohort
          dlt_time <- c(dlt_time, cohort_dlt_time) ##
          # Store dummy dlt for new cohort
          dlt <- c(dlt, cohort_dlt) ##
          tox <- c(tox, cohort_tox)
          dose <- c(dose, cohort_level)
          fu <- fu + (rectime*(cohort-1)) + minfu
          fu <- c(fu, cohort_fu)
          #duration <- duration + (cohort * rectime) + minfu
          
          for (l in 1:length(tox)) {
            if(tox[l] == 1 & fu[l] >= dlt_time[l]){
              fu[l] <- obswin
              dlt[l] <- 1
            }
          }
          fu <- pmin(fu, obswin)
          
          for (b in 1:length(fu)) {
            weights[b] <- 0.6 + 
              0.2*(min(win2 - minfu, fu[b]-minfu)/(win2 - minfu))+ 
              0.2*(max(fu[b]- win2, 0)/(obswin-win2))
          }
          cohort.count <- cohort.count+1

        }
      }
      
      
    }
    
    print(s)
    num_cohorts <- length(tox) / cohort
    duration <- cohort_fu[1] + (rectime*(cohort-1) + minfu) * (cohort.count-1) +(obswin - minfu)
    iterations[[s]] <- list(tox = tox, level = dose, mtd = fdose, 
                            stop = stop, stop_reason = stop_reason, 
                            duration = duration, cohorts = cohort.count)
  }
  dose_selections = sapply(iterations, function(x) x$mtd)
  doses_given = unlist(sapply(iterations, function(x) x$level))
  duration = sapply(iterations, function(x) x$duration)
  summary = list(true_tox = true_tox, prior = prior, target = target, 
                 max_sample_size = max_sample_size, initdes = initdes, 
                 months = 12*mean(duration)/365,
                 time = Sys.time()- start_time,
                 num_sims = num_sims, cohort = cohort,
                 prob_stop = table(substr(unlist(sapply(iterations, function(x) 
                   x$stop_reason)), 1, 15))/num_sims, 
                 mtd = sapply(1:length(prior),  function(d)
                   sum(dose_selections == d, na.rm = TRUE)/num_sims), 
                 
                 doses_given = sapply(1:length(prior), function(d)
                   sum(doses_given == d, na.rm = TRUE)/num_sims), prob_dose_given = sapply(1:length(prior), 
                                                                                           function(d) sum(doses_given == d, na.rm = TRUE)/length(doses_given)))
  return(list(summary = summary, iterations = iterations))
}


# Simulations for no cohorts and removal of minimum followup 
# Not used
titepocrm_sim_nomin <- function (r, alpha, prior.o, x0, stop, n, theta, nsim, 
                           tox.range, cohort, obswin, minfu, win2, recrate, 
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
      bcrml <- function(a, p1, y, w) {
        lik = 0
        for (j in 1:length(y)) {
          lik = lik + y[j] * a * log(w[j]*p1[j]) + 
            (1 - y[j]) * log((1 - w[j]*p1[j]^a))
        }
        return(lik)
      }
      # specifies the number of doses 
      ncomb = ncol(alpha)
      # Initialise empty vectors for use later on 
      # y (number of dlt's per dose), npts(number of patients per dose)
      y = npts = ptox.hat = numeric(ncomb)
      # The dose level to be selected (add 1 to indicate position to stop)
      comb.select = numeric(ncomb )
      # Starting dose taken from first element of the dose escalation scheme
      comb.curr = x0[1]
      stoprule = 0
      stop.count <- 0
      # Specifies the dose escelation scheme 
      stage1 <- c(x0, rep(ncol(alpha), n - length(x0)))
      # Counter for the number of cohorts
      cohort.count <- 1
      # Empty vectors to store dose recived and if dlt was observed
      duration <- dose <- dlt <- dlt_time <- fu <- tox <- weights <- vector()
      # Time to recruit new patients
      rectime <- obswin/recrate
      
      while(length(tox) < n){
        # Takes draws from a binomial distribution to determine if patients in the cohort had a toxic outcome
        cohort_tox <- stats::rbinom(n = cohort, size = 1, prob = r[comb.curr])
        # Dummy variable to store DLT which will be updated once the time for DLT has passed
        cohort_dlt <- rep(0, cohort)
        # Patients in the same cohort recieve the same dose
        cohort_comb <- rep(comb.curr, cohort)
        # Follow up time based on the time taken to recruit a patient
        # First patient assumed to be recruited instantly
        # Add the min fu time to indicate when analysis is done
        cohort_fu <- (rectime*(cohort-1)) - (rectime * 0:(cohort - 1)) + rectime
        # Time of DLT is recorded and set to be at any time during the obswin
        cohort_dlt_time <- vector()
        for (i in 1:cohort) {
          if(cohort_tox[i] == 0){ 
            cohort_dlt_time[i] <- NA  
          }
          else {
            cohort_dlt_time[i] <- runif(1,0, obswin)
          }
        } 
        # Stores the time DLT occurs for new cohort
        dlt_time <- c(dlt_time, cohort_dlt_time)
        # Stores new cohorts toxicity 
        tox <- c(tox, cohort_tox)
        # Stores dose level for each new cohort  
        dose <- c(dose, cohort_comb)
        # Store dummy dlt for new cohort
        dlt <- c(dlt, cohort_dlt)
        # Add on follow up time and recruitment time for previous cohorts
        fu <- fu + (cohort * rectime) 
        #duration <- duration + (cohort * rectime) + minfu 
        # store new cohorts follow up time
        fu <- c(fu, cohort_fu)
        #duration <- c(duration, cohort_fu)
        # Loop through all the patients who will have a DLT
        for(i in 1:length(fu)){
          if(tox[i] == 1){
            # If the follow up time is greater than the time the dlt was determined to occur
            if(fu[i] >= dlt_time[i]){
              # Change the follow up time to the obswin to indicate the dlt has now happened
              fu[i] <- obswin
              # As the dlt has happened it should now be stored for  calculation in the likelihood
              dlt[i] <- 1
            }
          }
        }
        
        fu <- pmin(fu, obswin)
        # Calculated weights where the min fu accounts for 60% and is 80% by window 2
        for (i in 1:length(fu)) {
          weights[i] <- 0.6*(min(fu[i],minfu)/minfu) + 
            0.2*max((min(win2 - minfu, fu[i]-minfu)/(win2 - minfu)),0)+ 
            0.2*max((max(fu[i]- win2, 0)/(obswin-win2)),0)
        }
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
            beta <- alpha[k,][dose[1:length(dose)]]
            est.tox[k] <- optimize(f = bcrml, interval = c(0,500), 
                                   p1 = beta, y = dlt, w = weights, 
                                   maximum = T)$maximum
            like.tox[k] <- optimize(f = bcrml, interval = c(0,500), 
                                    p1 = beta, y = dlt, w = weights,
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
            inverse.var <- inverse.var + ((1-dlt[i])*(weights[i]*beta[i]^est.tox[mtox.sel])*(log((weights[i]*beta[i])))^2)/(1-((weights[i]*beta[i]^est.tox[mtox.sel])))^2
          }
          
          post.alpha.mean <- est.tox[mtox.sel]
          post.alpha.var <- ifelse(inverse.var == 0, 0 , 1/inverse.var)
          post.alpha.samp <- rnorm(n = 100000, mean = post.alpha.mean, 
                                   sd = sqrt(post.alpha.var))
          post.prob.tox.samp <- alpha[mtox.sel,1]^post.alpha.samp
          prob.too.toxic <- mean(post.prob.tox.samp > tox.lim)
          
          
          if(is.na(prob.too.toxic)) {
            prob.too.toxic <- 1
          }
          
          
          
          # stopping rule for when lowest dose is too toxic AND lowest dose has been tested ptox.hat[1] > 0.5 & 1 %in% dose
          # 
          # stopping rule if dose is being recommended for a xth time
          if (length(dlt) == n |(prob.too.toxic > tox.cert) & 1 %in% dose | sum(dose == comb.curr) == mtd.lim) {
            stoprule = 0
            
            # once all patients recruited calculate likelihoods at max weight
            like.tox = est.tox = rep(0, nord.tox)
            for (k in 1:nord.tox) {
              
              #set all the weights at 1
              weights <- rep(1, length(tox))
              #ensures that all those predicted to have toxic event is included in dlt data
              # as we are assuming full follow up once the last patients are recruited
              dlt <- tox
              beta <- alpha[k,][dose[1:length(dose)]]
              est.tox[k] <- optimize(f = bcrml, interval = c(0,500), 
                                     p1 = beta, y = tox, w = weights, 
                                     maximum = T)$maximum
              like.tox[k] <- optimize(f = bcrml, interval = c(0,500), 
                                      p1 = beta, y = tox, w = weights,
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
            
            # Calculate asymptotic variance 
            beta <- alpha[mtox.sel,][dose]
            inverse.var <- 0
            
            
            for (i in 1:length(tox)) {
              inverse.var <- inverse.var + ((1-tox[i])*(weights[i]*beta[i]^est.tox[mtox.sel])*(log((weights[i]*beta[i])))^2)/(1-((weights[i]*beta[i]^est.tox[mtox.sel])))^2
            }
            
            post.alpha.mean <- est.tox[mtox.sel]
            post.alpha.var <- ifelse(inverse.var == 0, 0 , 1/inverse.var)
            post.alpha.samp <- rnorm(n = 100000, mean = post.alpha.mean, 
                                     sd = sqrt(post.alpha.var))
            post.prob.tox.samp <- alpha[mtox.sel,1]^post.alpha.samp
            prob.too.toxic <- mean(post.prob.tox.samp > tox.lim)
            
            
            if(is.na(prob.too.toxic)) {
              prob.too.toxic <- 1
            }
            
            
            break
          }
          
          
          else {
            # Takes draws from a binomial distribution to determine if patients in the cohort had a toxic outcome
            cohort_tox <- stats::rbinom(n = cohort, size = 1, prob = r[comb.curr])
            cohort_dlt <- rep(0, cohort)
            # Patients in the same cohort recieve the same dose
            cohort_comb <- rep(comb.curr, cohort)
            # Follow up time based on the time taken to recruit a patient
            # First patient assumed to be recruited instantly
            # Add the min fu time to indicate when analysis is done
            cohort_fu <- (rectime*(cohort-1))-(rectime * 0:(cohort - 1)) +rectime
            # Time of DLT is recorded and set between the time the patient has been followed up and the remaining observation window
            cohort_dlt_time <- vector()
            for (i in 1:cohort) {
              if(cohort_tox[i] == 0){ 
                cohort_dlt_time[i] <- NA  
              }
              else {
                cohort_dlt_time[i] <- runif(1,0, obswin)
              }
            } 
            # Stores the time DLT occurs for new cohort
            dlt_time <- c(dlt_time, cohort_dlt_time)
            # Stores new cohorts toxicity 
            tox <- c(tox, cohort_tox)
            # Stores dose level for each new cohort  
            dose <- c(dose, cohort_comb)
            # Store dummy dlt for new cohort
            dlt <- c(dlt, cohort_dlt)
            # Add on follow up time and recruitment time for previous cohorts
            fu <- fu + (cohort * rectime)  
            #duration <- duration + (cohort * rectime) + minfu
            # store new cohorts follow up time
            fu <- c(fu, cohort_fu)
            #duration <- c(duration, cohort_fu)
            for(i in 1:length(fu)){
              if(tox[i] == 1){
                if(fu[i] >= dlt_time[i]){
                  fu[i] <- obswin
                  dlt[i] <- 1
                }
              }
            }
            
            fu <- pmin(fu, obswin)
            # Calculated weights where the min fu accounts for 60% and 80% by win2
            for (i in 1:length(fu)) {
              weights[i] <- 0.6*(min(fu[i],minfu)/minfu) + 
                0.2*max((min(win2 - minfu, fu[i]-minfu)/(win2 - minfu)),0)+ 
                0.2*max((max(fu[i]- win2, 0)/(obswin-win2)),0)
            }
            
            y[comb.curr] = y[comb.curr] + sum(cohort_tox)
            npts[comb.curr] = npts[comb.curr] + cohort
          }
        }
      }
      
      if (stoprule == 0) {
        if (!exists("prob.too.toxic")){prob.too.toxic <- 0}
        if((prob.too.toxic > tox.cert)& 1 %in% dose){
          stop.count <- 1 
        }
        else {
          comb.select[comb.curr] = comb.select[comb.curr] + 1
          
        }
      }
      
      # Calculate the duration of the trial 
      
      num_cohorts <- length(tox) / cohort
      duration <- cohort_fu[1] + (rectime*(cohort)) * (num_cohorts-1) + (obswin)
      return(list(MTD.selection = comb.select, tox.data = y, 
                  patient.allocation = npts, duration = duration,
                  stop = stop.count))
    }
    
  }
  lpocrm.sim <- function(nsim) {
    ncomb = length(r)
    y <- npts <- matrix(nrow = nsim, ncol = ncomb)
    comb.select <- matrix(nrow = nsim, ncol = ncomb )
    duration <- stop.count <- trialsize <- rep(0, nsim)
    nstop = 0
    for (i in 1:nsim) {
      #print(i)
      result <- lpocrm(r, alpha, prior.o, x0, stop, n, 
                       theta)
      comb.select[i, ] = result$MTD.selection
      y[i, ] = result$tox.data
      npts[i, ] = result$patient.allocation
      trialsize[i] = sum(result$patient.allocation)
      duration[i] = result$duration # convert duration to months
      stop.count[i] = result$stop
    }
    return(list(true.prob = r, 
                MTD.selection = round(colMeans(comb.select),2),
                patient.allocation = 100* round(colMeans(npts)/mean(trialsize),2), 
                percent.DLT = sum(colMeans(y))/mean(trialsize), 
                months = 12*mean(duration)/365, 
                stop = mean(stop.count), 
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



