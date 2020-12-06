

source('wagestait/kb/version2/wagestait.R')


####################################
#
#	Input
#
#	p0 = true toxicity probabilities 
#	q0 = true efficacy probabilities
#	p.skel = toxicity skeleton values
# 	q.skel = efficacy skeleton values
#	tul = toxicity upper limit allowed
# 	ell = efficacy lower limit allowed
#	cohortsize = size of each cohort inclusion
# 	nchort = number of cohorts in a sinlge trial
#	start.comb = starting combination
#	n.ar = size of adaptive randomization phase
#
#	ntrial = number of simulated trials
# 
#	
#
####################################


######################################
#
#        Generate results in Table 2
#
######################################


#####Specify the total number of combinations
d <- 6

#####Specify the number of possible toxicity orderings
s <- 1   

###Specify a set of toxicity skeleton values
p.skel<-c(0.01,0.08,0.15,0.22,0.29,0.36)

#####Specify the number of possible toxicity orderings
g <- 11   #efficacy

###Specifiy the possible efficacy orderings of the drug combinations
q.skel<-matrix(nrow=g,ncol=d)
q.skel[1,]<-c(0.60,0.50,0.40,0.30,0.20,0.10)  
q.skel[2,]<-c(0.50,0.60,0.50,0.40,0.30,0.20)  
q.skel[3,]<-c(0.40,0.50,0.60,0.50,0.40,0.30)  
q.skel[4,]<-c(0.30,0.40,0.50,0.60,0.50,0.40)  
q.skel[5,]<-c(0.20,0.30,0.40,0.50,0.60,0.50)  
q.skel[6,]<-c(0.10,0.20,0.30,0.40,0.50,0.60)  
q.skel[7,]<-c(0.20,0.30,0.40,0.50,0.60,0.60)  
q.skel[8,]<-c(0.30,0.40,0.50,0.60,0.60,0.60)  
q.skel[9,]<-c(0.40,0.50,0.60,0.60,0.60,0.60)  
q.skel[10,]<-c(0.50,0.60,0.60,0.60,0.60,0.60)  
q.skel[11,]<-c(rep(0.60,6))  


## true toxicity probability
t1 <- c(0.05, 0.10, 0.20, 0.28, 0.50, 0.50)   
t2 <- c(0.05, 0.10, 0.20, 0.28, 0.40, 0.55)   
t3 <- c(0.05, 0.10, 0.15, 0.20, 0.35, 0.40)   
t4 <- c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05)   
## true efficacy probability 
r1 <- c(0.05, 0.13, 0.25, 0.38, 0.50, 0.63)   
r2 <- c(0.05, 0.23, 0.47, 0.70, 0.70, 0.70)   
r3 <- c(0.70, 0.70, 0.70, 0.70, 0.70, 0.70)  


tul <- 0.33 ## toxicity upper limit 
ell <- 0.05 ## efficacy lower limit
cohortsize = 1 ## cohort size for each inclusion
ncohort = 64   ## number of cohorts
start.comb = 1 ## starting combination
n.ar = 16      ## size of AR phase

library(jsonlite)
out.dir = 'H:/sims/wagestait/'

# Test
set.seed(580)  ## random seed
bpocrm.sim(t1, r1, p.skel, q.skel, tul, ell, cohortsize, ncohort, 10)
set.seed(1)  ## random seed
bpocrm.sim(t1, r1, p.skel, q.skel, tul, ell, cohortsize, ncohort, 100)


# Simulations
res.t1.r1 = bpocrm.sim(t1, r1, p.skel, q.skel, tul, ell, cohortsize, ncohort, 1000)
write(toJSON(res.t1.r1, pretty = T, auto_unbox = T), file = paste0(out.dir, "V2.T1.R1.json"))

res.t1.r2 = bpocrm.sim(t1, r2, p.skel, q.skel, tul, ell, cohortsize, ncohort, 1000)
write(toJSON(res.t1.r2, pretty = T, auto_unbox = T), file = paste0(out.dir, "V2.T1.R2.json"))

res.t1.r3 = bpocrm.sim(t1, r3, p.skel, q.skel, tul, ell, cohortsize, ncohort, 1000)
write(toJSON(res.t1.r3, pretty = T, auto_unbox = T), file = paste0(out.dir, "V2.T1.R3.json"))






######################################
#
#        Generate results in Table 3
#
######################################

#####Specify the total number of doses
d<-5

#####Specify the number of possible toxicity orderings
s<-1   

###Specify a set of toxicity skeleton values
p.skel<-c(0.01,0.08,0.15,0.22,0.29)

#####Specify the number of possible toxicity orderings
g<-9   #efficacy

###Specifiy the possible efficacy orderings of the doses
q.skel<-matrix(nrow=g,ncol=d)
q.skel[1,]<-c(0.60,0.70,0.60,0.50,0.40)
q.skel[2,]<-c(0.70,0.60,0.50,0.40,0.30)
q.skel[3,]<-c(0.50,0.60,0.70,0.60,0.50)
q.skel[4,]<-c(0.40,0.50,0.60,0.70,0.60)
q.skel[5,]<-c(0.30,0.40,0.50,0.60,0.70)
q.skel[6,]<-c(0.70,0.70,0.70,0.70,0.70) 
q.skel[7,]<-c(0.60,0.70,0.70,0.70,0.70)
q.skel[8,]<-c(0.50,0.60,0.70,0.70,0.70) 
q.skel[9,]<-c(0.40,0.50,0.60,0.70,0.70)

p1<-c(0.01,0.05,0.10,0.15,0.20)
q1<-c(0.30,0.50,0.60,0.40,0.25)

p2<-c(0.02,0.06,0.12,0.30,0.40)
q2<-c(0.38,0.50,0.40,0.30,0.25)

p3<-c(0.03,0.09,0.16,0.28,0.42)
q3<-c(0.25,0.35,0.48,0.65,0.52)

p4<-c(0.02,0.05,0.07,0.09,0.11)
q4<-c(0.68,0.56,0.49,0.40,0.33)

tul<-0.33 ##toxicity upper limit 
ell<-0.20 ##efficacy lower limit
cohortsize=1 ##cohort size for each inclusion
ncohort=48   ##number of cohorts
start.comb=1 ##starting dose
n.ar=36      ##size of AR phase
ntrial=1000   ##number of simulated trials 
set.seed(580)  ##random seed


p0<-p1
q0<-q1
##simulate many trials
bpocrm.sim(p0,q0,p.skel,q.skel,tul,ell,cohortsize,ncohort,ntrial)
