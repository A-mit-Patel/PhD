library(dfcrm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(gtools)
library(rgl)
library(magick)
library(ggrepel)
library(ggpubr)

skeleton <- getprior(target = 0.25, nu =4, nlevel =5, halfwidth = 0.05)
target <- 0.25 
obswin <- 35 
###############################################################################
# Cohorts of 1 
# T 
level <- 2
tox <- 1 
followup <- 35
mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
               obswin = obswin, scheme = 'linear', followup = followup)
mod$mtd

# N
level <- 2
tox <- 0 
followup <- 1:obswin
results <- data.frame('Observed' = followup, 'Rec' = rep(0,obswin))
for (i in 1:obswin) {
  followup <- i 
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, scheme = 'linear', followup = followup)
  results$Rec[i] <- mod$mtd
}

DTP_c1 <- results %>% 
  mutate( Dose = 2,
          Pathway = Observed + 1,
          Outcome = paste0('N','(', Observed, ')'),
          Outcome = if_else(Outcome == 'N(35)', 'N', Outcome)) %>% 
  select(Pathway, Dose, Outcome, Rec) %>% 
  add_row(Pathway = 1, Dose = 2, Outcome = 'T', Rec = 1) %>% 
  arrange(Pathway)

DTP_c1 %>% 
  kable('latex', booktabs = T, linesep = "", align = "c", 
             col.names = c('Pathway', 'Dose', 'Outcomes', 'Dose'),
             caption = '\\label{tab_tite-dtp:TITEDTP_c1}TITE-DTP for a cohort of 1.') %>% 
  kable_styling(latex_options = c("striped", "HOLD_position",  "scale_down"),
                position = "center", font_size = 5) %>% 
  add_header_above(c('', 'Cohort 1' = 2, 'Cohort 2' = 1)) %>% 
  cat()
################################################################################
# Cohorts of 2  
## TT
level <- c(2,2)
tox <- c(1,1) 
followup <- c(35,35)
mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
               obswin = obswin, scheme = 'linear', followup = followup)
mod$mtd

## NT 
level <- c(2,2)
tox <- c(1,0)
followupcombo <- cbind(rep(obswin, obswin), 1:obswin)
results <- data.frame('Observed' = 1:obswin, 'Rec' = rep(0,obswin))
for (i in 1:obswin) {
  followup <- followupcombo[i,]
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, scheme = 'linear', followup = followup)
  results$Rec[i] <- mod$mtd
}

DTP_c2NT <- results %>% 
  mutate( Dose = 2,
          Pathway = Observed,
          Outcome = paste0('N','(', Observed, ')', 'T'),
          Outcome = if_else(Outcome == 'N(35)T', 'NT', Outcome)) %>% 
  select(Pathway, Dose, Outcome, Rec) %>% 
  arrange(Pathway)

DTP_c2NT %>% 
  kable('latex', booktabs = T, linesep = "", align = "c", 
        col.names = c('Pathway', 'Dose', 'Outcomes', 'Dose'),
        caption = '\\label{tab_tite-dtp:TITEDTP_c2NT}TITE-DTP for a cohort of 2 for scenario NT.') %>% 
  kable_styling(latex_options = c("striped", "HOLD_position",  "scale_down"),
                position = "center", font_size = 5) %>% 
  add_header_above(c('', 'Cohort 1' = 2, 'Cohort 2' = 1)) %>% 
  cat()

## NN
level <- c(2,2)
tox <- c(0,0)

combos <- combinations(n = 35, r = 2, repeats.allowed = T, v = 1:35)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+1)]
for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- followup / obswin 
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, weights = weights)
  
  for (j in 1:ncol(results)) {
    results[,j][i] <- followup[j]
    results[,ncol(results)][i] <- mod$mtd
  }
}

results <- data.frame(results)
colnames(results) <- c('Patient1', 'Patient2', 'Rec')

results %>% mutate(TotalFollow = Patient1+Patient2) %>% 
  group_by(Rec) %>% 
  summarise(n = n(), min = min(TotalFollow), max = max(TotalFollow))

results %>% mutate(TotalFollow = Patient1+Patient2) %>%
  filter(TotalFollow == 20 | TotalFollow == 21) %>% 
  select(Patient1, Patient2, TotalFollow, Rec) %>% 
  arrange(desc(TotalFollow)) %>% 
  kable('latex', booktabs = T, linesep = "", align = "c", 
        col.names = c('Patient 1', 'Patient 2', 'Combined Follow-up', 'Dose Recommendation'),
        caption = '\\label{tab_tite-dtp:TITEDTP_c2NNprob}Different dose recommendations with overlapping combinded follow-up times.') %>% 
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center", font_size = 11) %>% 
  add_header_above(c('Follow-up' = 2, ' ' = 2)) %>% 
  cat()

# calculate exact numbe rof pathways for each specific outcome

results <- results %>% 
  mutate(TF = Patient1 + Patient2)

results %>% 
  filter(TF <= 19) %>% 
  nrow()

results %>% 
  filter(TF >= 22) %>% 
  nrow()

results %>% 
  filter(TF == 20 & Patient2 <= 17) %>% 
  nrow()

results %>% 
  filter(TF == 20 & Patient2 >= 18) %>% 
  nrow()

results %>% 
  filter(TF == 21 & Patient2 <= 14) %>% 
  nrow()

results %>% 
  filter(TF == 21 & Patient2 >= 15) %>% 
  nrow()


 # To produce a plot need to run on all possible combinations not just unique
level <- c(2,2)
tox <- c(0,0)
combos <- 1:obswin
combos <- expand.grid(combos, combos)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+1)]
for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- followup / obswin 
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, weights = weights)
  
  for (j in 1:ncol(results)) {
    results[,j][i] <- followup[j]
    results[,ncol(results)][i] <- mod$mtd
  }
}

results <- data.frame(results)
colnames(results) <- c('Patient1', 'Patient2', 'Rec')
  
  results %>% 
    ggplot(aes(x = Patient1, y = Patient2, fill = as.factor(Rec))) +
    geom_tile() +
    scale_fill_brewer(palette = 'Paired') + 
    theme_bw() + 
    geom_abline(intercept = 19, slope = -1, col = 'red', 
                linetype = 'dashed') +
    geom_abline(intercept = 22, slope = -1, col = 'red', 
                linetype = 'dashed') +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black")) +
    labs(fill = 'Recommended Dose', x = 'Patient 1 follow-up (Days)',
         y = 'Patient 2 follow-up (Days)')+ 
    scale_x_continuous(breaks = seq(0, 60, by = 05), expand = c(0, 0))+
    scale_y_continuous(breaks = seq(0, 60, by = 05), expand = c(0, 0))

################################################################################
# cohorts of 3
# TTT
level <- c(2,2,2)  
tox <- c(1,1,1)  
followup <- c(35,35,35)
mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
               obswin = obswin, scheme = 'linear', followup = followup)
mod$mtd

# NTT 
level <- c(2,2,2)
tox <- c(1,1,0)
followupcombo <- cbind(rep(obswin, obswin) ,rep(obswin, obswin), 1:obswin)
results <- data.frame('Observed' = 1:obswin, 'Rec' = rep(0,obswin))
for (i in 1:obswin) {
  followup <- followupcombo[i,]
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, scheme = 'linear', followup = followup)
  results$Rec[i] <- mod$mtd
}

DTP_c3NTT <- results %>% 
  mutate( Dose = 2,
          Pathway = Observed,
          Outcome = paste0('N','(', Observed, ')', 'TT'),
          Outcome = if_else(Outcome == 'N(35)TT', 'NTT', Outcome)) %>% 
  select(Pathway, Dose, Outcome, Rec) %>% 
  arrange(Pathway)

DTP_c3NTT %>% 
  kable('latex', booktabs = T, linesep = "", align = "c", 
        col.names = c('Pathway', 'Dose', 'Outcomes', 'Dose'),
        caption = '\\label{tab_tite-dtp:TITEDTP_c3NTT}TITE-DTP for a cohort of three for scenario NTT.') %>% 
  kable_styling(latex_options = c("striped", "HOLD_position",  "scale_down"),
                position = "center", font_size = 5) %>% 
  add_header_above(c('', 'Cohort 1' = 2, 'Cohort 2' = 1)) %>% 
  cat()

## NNT
level <- c(2,2,2)
tox <- c(1,0,0)
combos <- combinations(n = 35, r = 2, repeats.allowed = T, v = 1:35)
combos <- cbind(35, combos)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+1)]
for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- followup / obswin 
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, weights = weights)
  
  for (j in 1:ncol(results)) {
    results[,j][i] <- followup[j]
    results[,ncol(results)][i] <- mod$mtd
  }
}

results <- data.frame(results)
colnames(results) <- c('Patient1', 'Patient2', 'Patient3', 'Rec')

results %>% mutate(TotalFollow = Patient2+Patient3) %>% 
  group_by(Rec) %>% 
  summarise(n = n(), min = min(TotalFollow), max = max(TotalFollow))

results %>% mutate(TotalFollow = Patient2+Patient3) %>% View()

# NNN
  
level <- c(2,2,2)
tox <- c(0,0,0)
combos <- combinations(n = 35, r = 3, repeats.allowed = T, v = 1:35)

pos <- cbind(rep(0,nrow(combos)))

results <-  pos[, rep(1, each=length(tox)+1)]
for (i in 1:nrow(combos)) {
  if(i == 1){
    start.time <- Sys.time()
    print(start.time)
  }
  if(i == nrow(combos)){
    end.time <- Sys.time()
    print(end.time)
  }
  followup <- as.numeric(combos[i,])
  weights <- followup / obswin 
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, weights = weights)
  
  for (j in 1:ncol(results)) {
    results[,j][i] <- followup[j]
    results[,ncol(results)][i] <- mod$mtd
  }
  
}


results <- data.frame(results)
colnames(results) <- c('Patient1', 'Patient2','Patient3', 'Rec')
results %>% group_by(Rec) %>% 
  summarise(n=n())

results %>% mutate(TotalFollow = Patient1+Patient2+Patient3) %>% 
  group_by(Rec) %>% 
  summarise(n = n(), min = min(TotalFollow), max = max(TotalFollow))

results %>% 
  mutate(TotalFollow = Patient1+Patient2+Patient3) %>%
  filter(TotalFollow %in% c(24,25,26)) %>% 
  group_by(Rec) %>% 
  summarise(n = n())
  #filter(Patient3 == 15)

# Need to run all combinations for the plot 
level <- c(2,2,2)
tox <- c(0,0,0)
combos <- 1:obswin
combos <- expand.grid(combos, combos, combos)
pos <- cbind(rep(0,nrow(combos)))

results <-  pos[, rep(1, each=length(tox)+1)]
for (i in 1:nrow(combos)) {
  if(i == 1){
    start.time <- Sys.time()
    print(start.time)
  }
  if(i == nrow(combos)){
    end.time <- Sys.time()
    print(end.time)
  }
  followup <- as.numeric(combos[i,])
  weights <- followup / obswin 
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, weights = weights)
  
  for (j in 1:ncol(results)) {
    results[,j][i] <- followup[j]
    results[,ncol(results)][i] <- mod$mtd
  }
  
}
results <- data.frame(results)
colnames(results) <- c('Patient1', 'Patient2','Patient3', 'Rec')
plot3d(x = results$Patient1, y = results$Patient2, z = results$Patient3,
       col = as.factor(results$Rec), xlab = 'Patient 1 follow-up',
       ylab = 'Patient 2 follow-up', zlab = 'Patient 3 follow-up')

rgl.postscript("c3NNN", "pdf")
outputFile = sub(".gif$","",outputFile)
movie3d(spin3d(axis=c(0,0,1), rpm=5), duration=12, dir=getwd(), movie="c3NNN")
################################################################################

# Investigating combinations 
# With a combined follow-up of 20 2, 18 leads to 5 but 3, 17 leads to 4
# With a combined follow-up of 21 6, 15 leads to 5 but 7, 14 leads to 4
level <- c(2,2)
tox <- c(0,0)
followup <- c(2, 18)
weights <- followup / obswin 
mod1 <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
               obswin = obswin, weights = weights)

followup <- c(3, 17)
weights <- followup / obswin 
mod2 <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                obswin = obswin, weights = weights)

# Re run analysis but record estimates and variance 
level <- c(2,2)
tox <- c(0,0)

combos <- combinations(n = 35, r = 2, repeats.allowed = T, v = 1:35)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+3)]
for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- followup / obswin 
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, weights = weights)
  
  for (j in 1:ncol(results)) {
    results[,j][i] <- followup[j]
    results[,3][i] <- mod$mtd
    results[,4][i] <- mod$estimate
    results[,5][i] <- mod$post.var
  }
}

# Rename data frame 
results <- data.frame(results)
colnames(results) <- c('Patient1', 'Patient2', 'Rec', 'Est', 'Var')

# Check matches previous results 
results %>% mutate(TotalFollow = Patient1+Patient2) %>% 
  group_by(Rec) %>% 
  summarise(n = n(), min = min(TotalFollow), max = max(TotalFollow))

# Create table with beta estimate and variance included
results %>% mutate(TotalFollow = Patient1+Patient2, 
                   Est = Est %>% round(4),
                   Var = Var %>% round(4)) %>%
  filter(TotalFollow == 20 | TotalFollow == 21) %>% 
  select(Patient1, Patient2, TotalFollow, Rec, Est, Var) %>% 
  arrange(desc(TotalFollow)) %>% 
  kable('latex', booktabs = T, linesep = "", align = "c", 
        col.names = c('Patient 1', 'Patient 2', 'Combined', 'Dose Recommendation',
                      'Beta', 'Variance'),
        caption = '\\label{tab_tite-dtp:TITEDTP_c2NNprob}Different dose recommendations with overlapping combined follow-up times.') %>% 
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center", font_size = 11) %>% 
  add_header_above(c('Follow-up' = 3, ' ' = 1, 'Posterior Estimates' = 2)) %>% 
  cat()

# Overall plot 
results %>% mutate(TotalFollow = Patient1+Patient2) %>%
  ggplot(aes(x = TotalFollow, y = Est, col = factor(Rec))) + 
  geom_point() +
  geom_hline(yintercept = 0.149, col = "red", linetype = "longdash") +
  labs(x = "Combined Total Follow-up", y = expression(Posterior ~ Estimate ~ beta),
       col = "Recommended Dose")+
  scale_color_manual(values = c("#994d1a","#1a6699"))+
  scale_x_continuous(breaks = seq(0, 70, 5))+
  theme_bw()+
  theme(legend.position = "bottom") 

# Plot for 20 & 21 days 
results %>% mutate(TotalFollow = Patient1+Patient2) %>%
  filter(TotalFollow == 20 | TotalFollow == 21) %>% 
  ggplot(aes(x = factor(TotalFollow), y = Est, col = factor(Rec))) + 
  geom_point() +
  geom_hline(yintercept = 0.149, col = "red", linetype = "longdash") +
  geom_text_repel(mapping = aes(label = paste0("(", Patient1, ", ", Patient2, ")")),
                  show.legend = FALSE)+
  labs(x = "Combined Total Follow-up", y = expression(Posterior ~ Estimate ~ beta),
       col = "Recommended Dose")+
  scale_color_manual(values = c("#994d1a","#1a6699"))+
  theme_bw()+
  theme(legend.position = "bottom") 


skeleton
beta <- seq(0,1, 0.0001)
df <- data.frame(beta, rec = rep(NA, times = length(beta)))
for (i in 1:length(beta)) {
  ptox <- skeleton^exp(beta[i])
  if (all(ptox <= target)) {
    df$rec[i] <- length(skeleton)
  }
  else if (all(ptox >= target)) {
    df$rec[i] <- 1
  }
  else {
    df$rec[i] <- order(abs(ptox - target))[1]
  } 
}
df %>% group_by(rec) %>% 
  summarise(min = min(beta) %>% round(4),
            max = max(beta) %>% round(4))

# 0.1492 -> 4, 0.1493 ->5 

####################################
# For 3 patients 
#################################### 

level <- c(2,2,2)
tox <- c(0,0,0)
combos <- combinations(n = 35, r = 3, repeats.allowed = T, v = 1:35)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+3)]

for (i in 1:nrow(combos)) {
  if(i == 1){
    start.time <- Sys.time()
    print(start.time)
  }
  if(i == nrow(combos)){
    end.time <- Sys.time()
    print(end.time)
  }
  followup <- as.numeric(combos[i,])
  weights <- followup / obswin 
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, weights = weights)
  
  for (j in 1:ncol(results)) {
    results[,j][i] <- followup[j]
    results[,4][i] <- mod$mtd
    results[,5][i] <- mod$estimate
    results[,6][i] <- mod$post.var
  }
  
}


results <- data.frame(results)
colnames(results) <- c('Patient1', 'Patient2','Patient3', 'Rec',  'Est', 'Var')
results %>% group_by(Rec) %>% 
  summarise(n=n())

results %>% mutate(TotalFollow = Patient1+Patient2+Patient3) %>% 
  group_by(Rec) %>% 
  summarise(n = n(), min = min(TotalFollow), max = max(TotalFollow))

results %>% 
  mutate(TotalFollow = Patient1+Patient2+Patient3) %>%
  filter(TotalFollow %in% c(20,21)) %>% 
  group_by(Rec) %>% 
  summarise(n = n())


results %>% 
  mutate(TotalFollow = Patient1+Patient2+Patient3) %>%
  filter(TotalFollow %in% c(20,21)) %>% 
  arrange(Est)

# Create table with beta estimate and variance included for 9 pathways
results %>% mutate(TotalFollow = Patient1+Patient2+Patient3, 
                   Est = Est %>% round(4),
                   Var = Var %>% round(4)) %>%
  filter(TotalFollow %in% c(20,21) & Rec == 5) %>% 
  select(Patient1, Patient2, Patient3, TotalFollow, Rec, Est, Var) %>% 
  arrange(desc(TotalFollow)) %>% 
  kable('latex', booktabs = T, linesep = "", align = "c",
        col.names = c('Patient 1', 'Patient 2', 'Patient 3', 'Combined', 'Dose Recommendation',
                      'Beta', 'Variance'),
        caption = '\\label{tab_tite-dtp:TITEDTP_c3NNNprob}Follow-up combinations totalling 20 or 21 days leading to dose-level 5.') %>%
  kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"),
                position = "center", font_size = 11) %>%
  add_header_above(c('Follow-up' = 4, ' ' = 1, 'Posterior Estimates' = 2)) %>%
  cat()

# Overall plot 
results %>% mutate(TotalFollow = Patient1+Patient2+Patient3) %>%
  ggplot(aes(x = TotalFollow, y = Est, col = factor(Rec))) + 
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0.1492, col = "red", linetype = "longdash") +
  labs(x = "Combined Total Follow-up", y = expression(Posterior ~ Estimate ~ beta),
       col = "Recommended Dose")+
  scale_color_manual(values = c("#994d1a","#1a6699"))+
  scale_x_continuous(breaks = seq(0, 105, 5))+
  theme_bw()+
  theme(legend.position = "bottom") 

# Plot for 20 & 21 days 
results %>% mutate(TotalFollow = Patient1+Patient2+Patient3) %>%
  filter(TotalFollow == 20 | TotalFollow == 21) %>% 
  ggplot(aes(x = factor(TotalFollow), y = Est, col = factor(Rec))) + 
  geom_point() +
  geom_hline(yintercept = 0.1492, col = "red", linetype = "longdash") +
  geom_text_repel(mapping = aes(label = paste0("(", Patient1, ", ",
                                               Patient2, ", ", 
                                               Patient3, ")")),
                  show.legend = FALSE, max.overlaps = Inf)+
  labs(x = "Combined Total Follow-up", y = expression(Posterior ~ Estimate ~ beta),
       col = "Recommended Dose")+
  scale_color_manual(values = c("#994d1a","#1a6699"))+
  theme_bw()+
  theme(legend.position = "bottom") 

# Calculate DTP
results <- results %>% 
  mutate(TF = Patient1 + Patient2+ Patient3)

results %>% 
  filter(TF <= 19) %>% 
  nrow()

results %>% 
  filter(TF >= 22) %>% 
  nrow()

results %>% 
  filter(TF == 20 & Patient3 <= 17) 

results %>% 
  filter(TF == 20 & Patient3 >= 18) 

results %>% 
  filter(TF == 21 & ((Patient3 <= 15 & Patient2 <=3) | Patient3 <= 14)) 

results %>% 
  filter(TF == 21 & ((Patient3 >= 15 & Patient2 >=4) | Patient3 >= 16)) 
