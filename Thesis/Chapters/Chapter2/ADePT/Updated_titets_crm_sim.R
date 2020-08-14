applied_titecrmts_sim_v2 <- function (true_tox, prior, target, max_sample_size, num_sims, win2,
          cohort_size = 1, obswin, minfu, recrate, initdes, dose_func = applied_titecrm, 
          ...) 
{
  iterations <- list()
  for (i in 1:num_sims) {
    tox <- c()
    level <- c()
    fu <- c()
    dlt <- c() ##
    dlt_time <- c() ##
    stop <- FALSE
    stop_reason <- NULL
    rectime <- obswin/recrate
    tox <- stats::rbinom(n = length(initdes), size = 1, prob = true_tox[initdes])
    level <- initdes
   # temp <- win2
    dlt_time <- rep(0,length(tox))
    # Generate time of dlt for those if it occured
    #############################################
    for (j in 1:length(tox)) {
      if(tox[j] == 0){
        dlt_time[j] <- NA
      }
      else {
        dlt_time[j] <- runif(1,0, obswin)
      }
    }
    ############################################
    if (any(tox == 1)) {
      fu = (rectime * (length(tox) - 1)) - (rectime * c(0:(length(tox) - 1))) + 
        minfu * (((length(tox)/cohort_size)):1)
      for (k in 1:length(tox)) {
        if(tox[k] == 1 & fu[k] >= dlt_time[k]){
          fu[k] <- obswin
          dlt[k] <- 1
        }
      }
      
      pos <- cohort_size * ceiling(which(dlt == 1)[1]/cohort_size)
      pos <- ifelse(is.na(pos),length(tox),pos)
      tox <- tox[1:pos]
      level <- level[1:pos]
      dlt <- dlt[1:pos]
      dlt_time <- dlt_time[1:pos]
      fu <- (rectime * (length(tox) - 1)) - (rectime * c(0:(length(tox) - 1))) + 
        minfu * (((length(tox)/cohort_size)):1)
      duration <- fu
      for (a in 1:length(tox)) {
        if(tox[a] == 1 & fu[a] >= dlt_time[a]){
          fu[a] <- obswin
          dlt[a] <- 1
        }
      }
      

      #fu[tox == 1] <- obswin
      fu <- pmin(fu, obswin)
      weights <-  0.6 + 0.2*(pmin(win2 - minfu, fu-minfu)/(win2 - minfu))+
        0.2*(pmax(fu- win2, 0)/(obswin-win2))
      x <- dose_func(prior = prior, target = target, tox = tox, 
                     level = level, followup = fu, obswin = obswin, weights = weights,
                     ...)
      dose <- x$mtd
      stop <- ifelse(is.null(x$stop), FALSE, x$stop)
      stop_reason <- x$stop_reason
    }
    if (all(tox == 0)) {
      dose = utils::tail(level, 1)
    }
    else {
      while (!stop & length(tox) < max_sample_size) {
        cohort_tox = stats::rbinom(n = cohort_size, size = 1, 
                                   prob = true_tox[dose])
        cohort_dlt <- rep(0, cohort_size) ## 
        cohort_level = rep(dose, cohort_size)
        cohort_fu = (rectime * (cohort_size - 1)) - (rectime * 
                                                       c(0:(cohort_size - 1))) + minfu
        cohort_dlt_time <- c()
        for (j in 1:cohort_size) {
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
        level <- c(level, cohort_level)
        fu <- fu + (cohort_size * rectime) + minfu
        fu <- c(fu, cohort_fu)
        duration <- duration + (cohort_size * rectime) + minfu
       
        for (l in 1:length(tox)) {
          if(tox[l] == 1 & fu[l] >= dlt_time[l]){
            fu[l] <- obswin
            dlt[l] <- 1
          }
        }
        fu <- pmin(fu, obswin)
        weights <-  0.6 + 0.2*(pmin(win2 - minfu, fu-minfu)/(win2 - minfu))+
          0.2*(pmax(fu- win2, 0)/(obswin-win2)) ##
        
        x <- dose_func(prior = prior, target = target, weights = weights, 
                       tox = tox, level = level, followup = fu, obswin = obswin, 
                       ...)
        dose <- x$mtd
        stop <- ifelse(is.null(x$stop), FALSE, x$stop)
        stop_reason <- x$stop_reason
      }
    }
    if (!stop) {
      x <- dose_func(prior = prior, target = target, tox = tox, 
                     level = level, followup = rep(obswin, length(tox)), 
                     obswin = obswin, weights = rep(1, length(tox)),
                     ...)
      dose <- x$mtd
      stop <- ifelse(is.null(x$stop), FALSE, x$stop)
    }
    print(i)
    duration <- 12*duration[1]/365
    iterations[[i]] <- list(tox = tox, level = level, mtd = dose, 
                            stop = stop, stop_reason = stop_reason, 
                            duration = duration )
  }
  dose_selections = sapply(iterations, function(x) x$mtd)
  doses_given = unlist(sapply(iterations, function(x) x$level))
  duration = sapply(iterations, function(x) x$duration)
  summary = list(true_tox = true_tox, prior = prior, target = target, 
                 max_sample_size = max_sample_size, initdes = initdes, 
                 months = sum(duration)/num_sims,
                 num_sims = num_sims, cohort_size = cohort_size,
                 prob_stop = table(substr(unlist(sapply(iterations,
                                                        function(x) x$stop_reason)), 1, 15))/num_sims, 
                 mtd = sapply(1:length(prior),  function(d)
                   sum(dose_selections == d, na.rm = TRUE)/num_sims), 
                 
                 doses_given = sapply(1:length(prior), function(d)
                   sum(doses_given == d, na.rm = TRUE)/num_sims), prob_dose_given = sapply(1:length(prior), 
                                                                                                                              function(d) sum(doses_given == d, na.rm = TRUE)/length(doses_given)))
  return(list(summary = summary, iterations = iterations))
}



