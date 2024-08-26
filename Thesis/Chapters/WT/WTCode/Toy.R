source('RtC-WT.R')

# Specify the total number of doses.
d <- 5
# Specify the number of efficacy orderings 
g <- 7

# Specify a set of toxicity skeleton values
p.skel <- c(0.1, 0.15, 0.25, 0.35, 0.45)

# Specify the possible efficacy orderings of the drug combination
q.skel <- matrix(nrow=g, ncol=d)
q.skel[1,] <- c(0.3, 0.7, 0.6, 0.5, 0.4)
q.skel[2,] <- c(0.3, 0.6, 0.7, 0.6, 0.5)
q.skel[3,] <- c(0.3, 0.5, 0.6, 0.7, 0.6)
q.skel[4,] <- c(0.3, 0.4, 0.5, 0.6, 0.7)
q.skel[5,] <- c(0.3, 0.5, 0.6, 0.7, 0.7)
q.skel[6,] <- c(0.3, 0.6, 0.7, 0.7, 0.7)
q.skel[7,] <- c(0.3, 0.7, 0.7, 0.7, 0.7)

# Toxicity upper limit 
tul <- 0.35
# Efficacy lower limit 
ell <- 0.5 

# Size of AR phase 
n.ar = 30
# Placebo rand prob 
placebo.rand.prob = 0.33


c(0.10, 0.20, 0.25 ,0.30, 0.35)#tox 
c(0.30, 0.40, 0.70, 0.50, 0.40)#eff

rbinom(3, 1, 0.25)
rbinom(3, 1, 0.70)
################################################################################

### Cohort 1 
# Start at dose-level 2 (1st experimental dose)
# Outcomes for these patients 2EEN (2 eff only 1 nothing)

y <- c(0, 0, 0, 0, 0) #tox
z <- c(0, 2, 0, 0, 0) #eff
n <- c(0, 3, 0, 0, 0) #n 

C1 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)
# Recommends 3 -note all addrandprob are equal so is essentialy jsut rolling a dice

### Cohort 2 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3EEE (3 eff)

y <- c(0, 0, 0, 0, 0) #tox
z <- c(0, 2, 3, 0, 0) #eff
n <- c(0, 3, 3, 0, 0) #n 

C2 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)

# Recommends 3

### Cohort 3 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3EEE (3 eff)

y <- c(0, 0, 0, 0, 0) #tox
z <- c(0, 2, 6, 0, 0) #eff
n <- c(0, 3, 6, 0, 0) #n 

C3 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)

# Recommends 4

### Cohort 4 
# Dose-level 4 (3rd experimental dose)
# Outcomes 4NNE (2 neither, 1 eff)

y <- c(0, 0, 0, 0, 0) #tox
z <- c(0, 2, 6, 1, 0) #eff
n <- c(0, 3, 6, 3, 0) #n 

C4 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)

# Recommends 1 (control)

### Cohort 5 
# Dose-level 1 (control dose)
# Outcomes 1NNN (3 neither)

y <- c(0, 0, 0, 0, 0) #tox
z <- c(0, 2, 6, 1, 0) #eff
n <- c(3, 3, 6, 3, 0) #n 

C5 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)

# Recommends 2 

# cohort 6 
# Dose-level 2 (1st experimental dose)
# Outcomes 2NEN (2 neither, 1 eff)

y <- c(0, 0, 0, 0, 0) #tox
z <- c(0, 3, 6, 1, 0) #eff
n <- c(3, 6, 6, 3, 0) #n 

C6 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)
# recommends 4 

# cohort 7 
# Dose-level 4 (3rd experimental dose)
# Outcomes 4TEN (1 neither, 1 eff, 1 tox)

y <- c(0, 0, 0, 1, 0) #tox
z <- c(0, 3, 6, 2, 0) #eff
n <- c(3, 6, 6, 6, 0) #n 

C7 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)

# recommends 1

# cohort 8 
# Dose-level 1 (control dose)
# Outcomes 1NNE (2 neither, 1 eff)

y <- c(0, 0, 0, 1, 0) #tox
z <- c(1, 3, 6, 2, 0) #eff
n <- c(6, 6, 6, 6, 0) #n 

C8 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)

# recommends 5 

# cohort 9 
# Dose-level 5 (4th experimental dose)
# Outcomes 5ETT (1 eff, 2 tox)

y <- c(0, 0, 0, 1, 2) #tox
z <- c(1, 3, 6, 2, 1) #eff
n <- c(6, 6, 6, 6, 3) #n 

C9 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)
# recommends 1 

# cohort 10 
# Dose-level 1 (control dose)
# Outcomes 1NNE (2 neither, 1 eff)

y <- c(0, 0, 0, 1, 2) #tox
z <- c(2, 3, 6, 2, 1) #eff
n <- c(9, 6, 6, 6, 3) #n 

C10 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                 check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                 placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 11 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3EEN (1 neither, 2 eff)

y <- c(0, 0, 0, 1, 2) #tox
z <- c(2, 3, 8, 2, 1) #eff
n <- c(9, 6, 9, 6, 3) #n 

C11 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 12 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3EBB (1 eff, 2 both)

y <- c(0, 0, 2, 1, 2) #tox
z <- c(2, 3, 11, 2, 1) #eff
n <- c(9, 6, 12, 6, 3) #n 

C12 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 13 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3EEB (2 eff, 1 both)

y <- c(0, 0, 3, 1, 2) #tox
z <- c(2, 3, 14, 2, 1) #eff
n <- c(9, 6, 15, 6, 3) #n 

C13 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 14 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3EET (2 eff, 1 tox)

y <- c(0, 0, 4, 1, 2) #tox
z <- c(2, 3, 16, 2, 1) #eff
n <- c(9, 6, 18, 6, 3) #n 

C14 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 15 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3NET (1neither, 1 eff, 1 tox)

y <- c(0, 0, 5, 1, 2) #tox
z <- c(2, 3, 17, 2, 1) #eff
n <- c(9, 6, 21, 6, 3) #n 

C15 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 16 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3EEB (2 eff, 1 both)

y <- c(0, 0, 6, 1, 2) #tox
z <- c(2, 3, 20, 2, 1) #eff
n <- c(9, 6, 24, 6, 3) #n 

C16 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 17 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3ETB (1 eff, 1tox , 1 both)

y <- c(0, 0, 8, 1, 2) #tox
z <- c(2, 3, 22, 2, 1) #eff
n <- c(9, 6, 27, 6, 3) #n 

C17 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 18 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3NBN (2 neither, 1 both)

y <- c(0, 0, 9, 1, 2) #tox
z <- c(2, 3, 23, 2, 1) #eff
n <- c(9, 6, 30, 6, 3) #n 

C18 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 19 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3EEN (2 eff, 1 neither)

y <- c(0, 0, 9, 1, 2) #tox
z <- c(2, 3, 25, 2, 1) #eff
n <- c(9, 6, 33, 6, 3) #n 

C19 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

# recommends 3 

# cohort 20 
# Dose-level 3 (2nd experimental dose)
# Outcomes 3NEN (1 eff, 2 neither)

y <- c(0, 0, 9, 1, 2) #tox
z <- c(2, 3, 26, 2, 1) #eff
n <- c(9, 6, 36, 6, 3) #n 

C20 <- wages.tait(y = y, z = z, n = n, n.ar = n.ar, p.skel = p.skel, q.skel = q.skel, 
                  check.tox.at.dose.level = 2, lowest.is.placebo = TRUE,
                  placebo.rand.prob = placebo.rand.prob)

###############################################################################
library(dplyr)
library(kableExtra)
# Table code 

# Had to manually change some values to add asterisk so collapse rows could work 

#AR phase
cbind(
  rep(1:10, each = 3),
  rep(c(2,3,"3*",4,1,2,4,1,5,1), each = 3), 
  rep(c("EEN", "EEE", "EEE*", "ENN", "NNN", "ENN", "ENT", "ENN", "ETT", "ENN"),
      each = 3),
  rep(c("tox", "eff", "rand."), times = 10),
  rbind(
    C1$ProbTox, C1$ProbEff, C1$AdaptiveRandProb,
    C2$ProbTox, C2$ProbEff, C2$AdaptiveRandProb,
    C3$ProbTox, C3$ProbEff, C3$AdaptiveRandProb,
    C4$ProbTox, C4$ProbEff, C4$AdaptiveRandProb,
    C5$ProbTox, C5$ProbEff, C5$AdaptiveRandProb,
    C6$ProbTox, C6$ProbEff, C6$AdaptiveRandProb,
    C7$ProbTox, C7$ProbEff, C7$AdaptiveRandProb,
    C8$ProbTox, C8$ProbEff, C8$AdaptiveRandProb,
    C9$ProbTox, C9$ProbEff, C9$AdaptiveRandProb,
    C10$ProbTox, C10$ProbEff, C10$AdaptiveRandProb
  ) %>% round(2), 
  rep(c(C1$RecommendedDose, C2$RecommendedDose * -1, C3$RecommendedDose,
        C4$RecommendedDose, C5$RecommendedDose, C6$RecommendedDose,
        C7$RecommendedDose, C8$RecommendedDose, C9$RecommendedDose,
        C10$RecommendedDose), each = 3), 
  rep(c(2, 3, "3*", 3, "3*",3, "3*", 3, "3*", 3), each = 3)
  ) %>% 
  data.frame() -> tabdata

kable(tabdata, "latex", booktabs = T, linesep = "", align = "c",
      col.names = c("Cohort", "Dose", "Outcome", " ", "1", "2", "3", "4", "5",
                    "Recommended", "Optimal")) %>% 
  kable_styling(latex_options = c("scale_down"),
                position = "center", font_size = 9) %>% 
  add_header_above(c(" " = 4, "Estimated probabilities" = 5, " " = 2)) %>% 
  collapse_rows(columns = c(1, 2, 3, 10, 11), latex_hline = "major") -> Toytable

cat(Toytable)

# Max phase 
cbind(
  rep(11:20, each = 2),
  rep(c("3*",3,"3*",3,"3*",3,"3*",3,"3*",3), each = 2), 
  rep(c("EEN", "BBE", "BEE", "EET", "ENT", "BEE", "BET", "BNN","EEN", "ENN"),
      each = 2),
  rep(c("tox", "eff"), times = 10),
  rbind(
    C11$ProbTox, C11$ProbEff, 
    C12$ProbTox, C12$ProbEff, 
    C13$ProbTox, C13$ProbEff, 
    C14$ProbTox, C14$ProbEff, 
    C15$ProbTox, C15$ProbEff, 
    C16$ProbTox, C16$ProbEff, 
    C17$ProbTox, C17$ProbEff, 
    C18$ProbTox, C18$ProbEff, 
    C19$ProbTox, C19$ProbEff, 
    C20$ProbTox, C20$ProbEff 
  ) %>% round(2), 
  rep(c(3, "3*", 3, "3*",3, "3*", 3, "3*", 3, "3*"), each = 2)
) %>% 
  data.frame() -> tabdata

kable(tabdata, "latex", booktabs = T, linesep = "", align = "c",
      col.names = c("Cohort", "Dose", "Outcome", " ", "1", "2", "3", "4", "5",
                    "Recommended/Optimal")) %>% 
  kable_styling(latex_options = c("scale_down"),
                position = "center", font_size = 9) %>% 
  add_header_above(c(" " = 4, "Estimated probabilities" = 5, " " = 1)) %>% 
  collapse_rows(columns = c(1, 2, 3, 10), latex_hline = "major") -> Toytable

cat(Toytable)

###############################################################################

data.frame(d = 1:5, n, y, z) %>% 
  mutate(ypct = paste(y, " (", round((100*y/n),2), "%)", sep = "" ),
         zpct = paste(z, " (", round((100*z/n),2), "%)", sep = "" )) %>% 
  select(-y, -z) %>% 
  kable("latex", booktabs = T, linesep = "", align = "c",
        col.names = c("Dose", "No. of patientes", "Toxicity", "Efficacy")) %>% 
    kable_styling(latex_options = c("striped"), position = "center") %>% 
    add_header_above(c(" " = 2, "N (%)" = 2))
