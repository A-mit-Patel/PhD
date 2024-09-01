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

# Alternative weight functions plot

data.frame(x = seq(from = 0, to = 35, by = 0.1)) %>% 
  mutate(y = x/35,
         w1 = 1 - exp(-(x/7)^2),
         w2 = 1 - exp(-(x/22)^6)) %>% 
  pivot_longer(cols = c("y", "w1", "w2"))%>% 
  ggplot(aes(x = x, y = value, col = name)) +
  geom_line(linewidth = 1) +
  labs(x = "Time (days)", y = "Weight", col = "Function") +
  scale_x_continuous(breaks = seq(0, 35, 5))+
  scale_color_brewer(palette = "Set1", labels = c("Weight 1", "Weight 2",
                                                  "Linear"))+
  theme_bw()+
  theme(legend.position = c(0.92, 0.2))

data.frame(x = seq(from = 0, to = 35, by = 5)) %>% 
  mutate(y = x/35,
         w1 = 1 - exp(-(x/7)^2),
         w2 = 1 - exp(-(x/22)^6)) %>% 
  round(2) %>% 
  kable('latex', booktabs = T, linesep = "", align = "c", 
        col.names = c('Time (Days)', 'Linear', 'Weight 1', 'Weight 2'),
        caption = '\\label{tab_tite-dtp:AltWeightSumm}Summary of weight value for different weight functions.') %>% 
  kable_styling(latex_options = c("striped", "HOLD_position"),
                position = "center") %>% 
  add_header_above(c('', 'Weight functions' = 3)) %>% 
  cat()
################################################################################

# TITE-DTPs

# General set-up 
skeleton <- getprior(target = 0.25, nu =4, nlevel =5, halfwidth = 0.05)
target <- 0.25 
obswin <- 35 

################################################################################

### NT Scenario 

### Quick weight function 
level <- c(2,2)
tox <- c(1,0)
followupcombo <- cbind(rep(obswin, obswin), 1:obswin)
results <- data.frame('Observed' = 1:obswin, 'Rec' = rep(0,obswin))
for (i in 1:obswin) {
  followup <- followupcombo[i,]
  weights <- 1 - exp(-(followup/7)^2)
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 scheme = 'linear', weights = weights)
  results$Rec[i] <- mod$mtd
}

DTP_c2NT <- results %>% 
  mutate( Dose = 2,
          Pathway = Observed,
          Outcome = paste0('N','(', Observed, ')', 'T'),
          Outcome = if_else(Outcome == 'N(35)T', 'NT', Outcome)) %>% 
  select(Pathway, Dose, Outcome, Rec) %>% 
  arrange(Pathway)

### Slow weight function 
level <- c(2,2)
tox <- c(1,0)
followupcombo <- cbind(rep(obswin, obswin), 1:obswin)
results <- data.frame('Observed' = 1:obswin, 'Rec' = rep(0,obswin))
for (i in 1:obswin) {
  followup <- followupcombo[i,]
  weights <- 1 - exp(-(followup/22)^6)
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 scheme = 'linear', weights = weights)
  results$Rec[i] <- mod$mtd
}

DTP_c2NT <- results %>% 
  mutate( Dose = 2,
          Pathway = Observed,
          Outcome = paste0('N','(', Observed, ')', 'T'),
          Outcome = if_else(Outcome == 'N(35)T', 'NT', Outcome)) %>% 
  select(Pathway, Dose, Outcome, Rec) %>% 
  arrange(Pathway)

################################################################################

### NN Scenario 

### Quick weight function 
level <- c(2,2)
tox <- c(0,0)

combos <- combinations(n = 35, r = 2, repeats.allowed = T, v = 1:35)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+1)]
for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- 1 - exp(-(followup/7)^2)
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

results <- results %>% 
  mutate(TF = Patient1 + Patient2)

results %>% 
  filter(TF == 8)

level <- c(2,2)
tox <- c(0,0)
combos <- combinations(n = 35, r = 2, repeats.allowed = T, v = 1:35)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+3)]
for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- 1 - exp(-(followup/7)^2)
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
  filter(TotalFollow == 8) %>% 
  select(Patient1, Patient2, TotalFollow, Rec, Est, Var) %>% 
  arrange(desc(TotalFollow)) %>% 
  kable('latex', booktabs = T, linesep = "", align = "c",
        col.names = c('Patient 1', 'Patient 2', 'Combined', 'Dose Recommendation',
                      'Beta', 'Variance'),
        caption = '\\label{tab_tite-dtp:AltWeightW1prob}Follow-up combinations totalling 8 days for scenario NN using Weight 1.') %>%
  kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"),
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

results %>% mutate(TotalFollow = Patient1+Patient2) %>%
  filter(TotalFollow %in% c(7,8,9)) %>% 
  ggplot(aes(x = TotalFollow, y = Est, col = factor(Rec))) + 
  geom_point() +
  geom_hline(yintercept = 0.149, col = "red", linetype = "longdash") +
  labs(x = "Combined Total Follow-up", y = expression(Posterior ~ Estimate ~ beta),
       col = "Recommended Dose")+
  geom_text_repel(mapping = aes(label = paste0("(", Patient1, ", ",
                                               Patient2, ")")),
                  show.legend = FALSE, max.overlaps = Inf)+
  scale_color_manual(values = c("#994d1a","#1a6699"))+
  scale_x_continuous(breaks = c(7,8,9))+
  theme_bw()+
  theme(legend.position = "bottom") 


### Slow weight function 
level <- c(2,2)
tox <- c(0,0)

combos <- combinations(n = 35, r = 2, repeats.allowed = T, v = 1:35)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+1)]
for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- 1 - exp(-(followup/22)^6)
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

results <- results %>% 
  mutate(TF = Patient1 + Patient2)


level <- c(2,2)
tox <- c(0,0)
combos <- combinations(n = 35, r = 2, repeats.allowed = T, v = 1:35)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+3)]
for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- 1 - exp(-(followup/22)^6)
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
  filter(TotalFollow <= 37 & TotalFollow >=23) %>% 
  select(Patient1, Patient2, TotalFollow, Rec, Est, Var) %>% 
  arrange(desc(TotalFollow)) -> resultsW2

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

