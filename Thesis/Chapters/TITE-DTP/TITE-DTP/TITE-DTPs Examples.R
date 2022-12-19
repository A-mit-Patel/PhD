library(dfcrm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)

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

results %>% 
  data.frame() %>% 
  select(V1, V2, V3 = 'Rec')
  group_by('Next Dose' = Rec) %>% 
  summarise('Follow up data' = paste(min(Observed), max(Observed), 
                                     sep ='-'))

################################################################################
# cohorts of 3 
level <- c(1,1,1)
tox <- c(0,0,0)
combos <- 1:obswin
combos <- combn(35,3) %>% data.frame() %>% transpose()

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
colnames(results) <- c('Patient1', 'Patient2','Patient3', 'TD20')
results %>% group_by(TD20) %>% 
  summarise(n=n())

save.image('TITE_DTPs_3.RData')

################################################################################
results %>% group_by('Next Dose' = Rec) %>% 
  summarise('Follow up data' = paste(min(Observed), max(Observed), 
                                     sep ='-'))

level <- c(2,2)
tox <- c(1,0)
followupcombo <- cbind(rep(56, obswin), 1:56)
results <- data.frame('Observed' = 1:56, 'Rec' = rep(0,obswin))
for (i in 1:obswin) {
  followup <- followupcombo[i,]
  mod <- titecrm(prior = skeleton, target = target, tox = tox, level = level, 
                 obswin = obswin, scheme = 'linear', followup = followup)
  results$Rec[i] <- mod$mtd
}

obswin <- 35 
level <- c(2,2)
tox <- c(0,0)
combos <- 1:obswin
combos <- 1:obswin
combos <- expand.grid(combos, combos)
combos <- combn(35,2) %>% data.frame() %>% transpose()
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
colnames(results) <- c('Patient1', 'Patient2', 'TD20')
results

results %>% 
  mutate(TotalFollow = Patient1+Patient2) %>% 
  group_by(TD20) %>%
  summarise(n = n(), min = min(TotalFollow), max = max(TotalFollow))

results %>%
  mutate(TotalFollow = Patient1+Patient2) %>% 
  filter(TotalFollow == 21) 

results %>% 
  ggplot(aes(x = Patient1, y = Patient2, fill = as.factor(TD20))) +
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
  labs(fill = 'TD20', x = 'Patient 1 follow-up',
       y = 'Patient 2 follow-up')+ 
  scale_x_continuous(breaks = seq(0, 60, by = 05), expand = c(0, 0))+
  scale_y_continuous(breaks = seq(0, 60, by = 05), expand = c(0, 0))
