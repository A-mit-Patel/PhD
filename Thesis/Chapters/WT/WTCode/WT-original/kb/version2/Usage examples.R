
##### Specify the total number of doses
d <- 6
##### Specify the number of possible toxicity orderings
s <- 1   
### Specify a set of toxicity skeleton values
p.skel <- c(0.01, 0.08, 0.15, 0.22, 0.29, 0.36)
##### Specify the number of possible efficacy orderings
g <- 11   
###Specifiy the possible efficacy orderings of the drug combinations
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c(0.60, 0.50, 0.40, 0.30, 0.20, 0.10)  
q.skel[2,] <- c(0.50, 0.60, 0.50, 0.40, 0.30, 0.20)  
q.skel[3,] <- c(0.40, 0.50, 0.60, 0.50, 0.40, 0.30)  
q.skel[4,] <- c(0.30, 0.40, 0.50, 0.60, 0.50, 0.40)  
q.skel[5,] <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.50)  
q.skel[6,] <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60)  
q.skel[7,] <- c(0.20, 0.30, 0.40, 0.50, 0.60, 0.60)  
q.skel[8,] <- c(0.30, 0.40, 0.50, 0.60, 0.60, 0.60)  
q.skel[9,] <- c(0.40, 0.50, 0.60, 0.60, 0.60, 0.60)  
q.skel[10,] <- c(0.50, 0.60, 0.60, 0.60, 0.60, 0.60)  
q.skel[11,] <- c(rep(0.60, 6))  

# Limits
tul <- 0.33 ## toxicity upper limit 
ell <- 0.05 ## efficacy lower limit



source('wagestait/kb/version2/wagestait.R')



# Example 1a ----
n = c(3, 3, 2, 0, 0, 0)  # Dose 1 (d1) given to 3 patients; d2 to 3; d3 to 2 pts; etc
y = c(1, 0, 1, 0, 0, 0) # 1 tox at d1, 0 @ d2, 1 @ d3, etc
z = c(0, 1, 2, 0, 0, 0) # 0 eff at d1; 1 @ d2; 2 @ d3; etc
set.seed(1)  # Set seed for reproducible randomness
dose_decision <- wages.tait(y, z, n, p.skel, q.skel, n.ar=16, comb.curr=3)

dose_decision$AdmissibleSet  # Only doses 1 and 2 are currently admissible...
dose_decision$ProbTox < tul  # ...because only those doses have Pr(Tox) below the upper limit
dose_decision$EffBayesFactor  # Efficacy model likelihoods to select most likely, i.e. Bayes Factors
which.max(dose_decision$EffBayesFactor)  # Efficacy skeleton 6 is the most likely, i.e. monontonic increasing
dose_decision$ProbEff
dose_decision$AdaptiveRandProb
dose_decision$RecommendedDose  # Remember, in the adaptive randomisation phase, this is random!
dose_decision$OptimalDose
dose_decision$Stop  # Do not stop yet...




# Update 1b ----
n = c(3, 6, 6, 3, 3, 0)  # 21 patients treated in total, at doses d1 - d4
y = c(1, 0, 1, 0, 0, 0)
z = c(0, 2, 4, 2, 3, 0)
set.seed(1)  # Set seed for reproducible randomness
dose_decision <- wages.tait(y, z, n, p.skel, q.skel, n.ar=16, comb.curr=3)

dose_decision$AdmissibleSet  # Now doses 1 - 5 are admissible...
dose_decision$ProbTox < tul
which.max(dose_decision$EffBayesFactor)
dose_decision$ProbEff
dose_decision$AdaptiveRandProb
dose_decision$RecommendedDose
dose_decision$OptimalDose
dose_decision$Stop




# Further examples needed: stopping, avoidance of skipping, etc.
