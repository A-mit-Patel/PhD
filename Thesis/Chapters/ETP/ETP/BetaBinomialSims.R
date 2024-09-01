# Operating Characteristics - Beta-Binomial Posterior -------------------------------------------------

# prob_true = underlying true probabilty of response
# npat = number of patients
# prior_par = vector of parameters for beta(a,b) prior distribution
# crit_rate = required repsonse rate to be greater than e.g. 0.1
# accept_prob = probability required to be greater than the required response rate with e.g. 0.8
# nsims = number of simulations



PPOS <- function(n2, apost, bpost, obj.resp.rate, accept.prob){
  # If remaining number of patients is 0 don't need to calculate PPoS
  if(n2 == 0){
    PPOS = NA
  }
  else{
    Pr2 <- sapply(0:n2, 
                  function(i) (gamma(apost+bpost)*gamma(n2+1)*gamma(i+apost)*
                                 gamma(n2-i+bpost)) / 
                    (gamma(apost)*gamma(bpost)*gamma(i+1)*gamma(n2-i+1)*
                       gamma(n2+apost+bpost)) )
    
    B <- sapply(0:n2, function(i) 1-pbeta(obj.resp.rate, apost+i, bpost+n2-i))
    
    temp <- cbind(Pr2, B) %>% 
      data.frame() %>% 
      mutate(GO = if_else(B > accept.prob, 1, 0),
             PPP = Pr2*GO) 
    PPOS = sum(temp$PPP)
  }
  return(PPOS)
}


oc_betabinom <- function(prob_true, npat, prior_par = c(0.5,0.5), crit_rate, accept_prob = 0.8, nsims, nint) {
  prob <- c(rep(0, nsims))
  ppc <- npat/(nint+1)
  cohorts <- npat/ppc
  a0 <- prior_par[1]
  b0 <- prior_par[2]
  
  for(i in 1:nsims) {
    data <- replicate(npat, rbinom(1,1,prob_true))
    
    post_prob <- pbeta(crit_rate, shape1 = (prior_par[1] + sum(data)),
                       shape2 = (prior_par[2] + npat - sum(data)),
                       lower.tail = FALSE) 
    
    ifelse(post_prob >= accept_prob, prob[i] <- 1, prob[i] <- 0)
    
    for(j in 1:nint) {
      n <- ppc*j
      r <- data[1:n]
      ppos <- PPOS(n2 = npat-n, apost = a0 + sum(r), 
                   bpost = b0 + n - sum(r),
                   obj.resp.rate = crit_rate, 
                   accept.prob = accept_prob)
      
      if(ppos < 0.05){
        prob[i] <- 0
        break
      }
    }
  }
 
  Acceptance_Rate <- (sum(prob) / (nsims)) * 100
  
  return(Acceptance_Rate)
}

# Sims for original design for ETPs
sims1 <- oc_betabinom(prob_true = 0.1, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.9, nsims = 10000, nint = 5)

sims2 <- oc_betabinom(prob_true = 0.2, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.9, nsims = 10000, nint = 5)

sims3 <- oc_betabinom(prob_true = 0.3, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.9, nsims = 10000, nint = 5)

sims4 <- oc_betabinom(prob_true = 0.4, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.9, nsims = 10000, nint = 5)

sims5 <- oc_betabinom(prob_true = 0.5, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.9, nsims = 10000, nint = 5)

# Sims for new design of ETPs
sims6 <- oc_betabinom(prob_true = 0.1, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.5, nsims = 10000, nint = 5)

sims7 <- oc_betabinom(prob_true = 0.2, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.5, nsims = 10000, nint = 5)

sims8 <- oc_betabinom(prob_true = 0.3, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.5, nsims = 10000, nint = 5)

sims9 <- oc_betabinom(prob_true = 0.4, npat = 30, prior_par = c(1,1), 
                      crit_rate = 0.3, accept_prob = 0.5, nsims = 10000, nint = 5)

sims10 <- oc_betabinom(prob_true = 0.5, npat = 30, prior_par = c(1,1), 
                       crit_rate = 0.3, accept_prob = 0.5, nsims = 10000, nint = 5)



save.image(file = 'BetaBinomialSimsData.RData')


load('BetaBinomialSimsData.RData')

library(kableExtra)

data.frame(true = c("10%", "20%", "30%", "40%", "50%"),
           sims = c(sims1, sims2, sims3, sims4, sims5)/100) %>% 
  kable('latex', booktabs = T, linesep = "", align = "c",
        col.names = c('True response rate', 'Probability of GO decision'),
        caption = '\\label{tab_etp:exampleBBsims}Simulations for example Beta-Binomial design.') %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center") %>%
  cat()

data.frame(true = c("10%", "20%", "30%", "40%", "50%"),
           sims = c(sims6, sims7, sims8, sims9, sims10)/100) %>% 
  kable('latex', booktabs = T, linesep = "", align = "c",
        col.names = c('True response rate', 'Probability of GO decision'),
        caption = '\\label{tab_etp:exampleBBsims}Simulations for example Beta-Binomial design with new decision rule.') %>%
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center") %>%
  cat()
