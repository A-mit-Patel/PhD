library(dtpcrm)
library(dfcrm)
library(dplyr)

# General specifications 
prior <- c(0.1, 0.2, 0.3)
target <- 0.2
obswin <- 56

# Example of 1 patient only 
level <- 1 
tox <- 0 
results <- data.frame('Outcome' = rep(0,56), 'Rec' = rep(0,56))
for (i in 1:56) {
    
    followup <- i 
    mod <- titecrm(prior = prior, target = target, tox = tox, level = level, 
                    obswin = obswin, scheme = 'linear', followup = followup)
    results$Outcome[i] <- mod$followup
    results$Rec[i] <- mod$mtd
}

results %>%  group_by(Rec) %>% 
  summarise(min = min(Outcome), max = max(Outcome), Range = paste(min, max, sep ='-'))


# Example for 2 patients 
prior <- c(0.1, 0.2, 0.3,)
target <- 0.2
level <- c(1,1)
tox <- c(0,0)
combos <- 1:obswin
combos <- expand.grid(combos, combos)
pos <- cbind(rep(0,nrow(combos)))
results <-  pos[, rep(1, each=length(tox)+1)]


for (i in 1:nrow(combos)) {
  followup <- as.numeric(combos[i,])
  weights <- followup / obswin 
  mod <- titecrm(prior = prior, target = target, tox = tox, level = level, 
                 obswin = obswin, weights = weights)

  for (j in 1:ncol(results)) {
    results[,j][i] <- followup[j]
    results[,ncol(results)][i] <- mod$mtd
  }
}

results <- data.frame(results)

head(results)
results %>%  group_by(X3) %>% 
  summarise(num = n(),
            minx1 = min(X1), 
            minx2 = min(X2),
            maxx1 = max(X1),
            maxx2 = max(X2))

results %>% mutate(Total = X1 + X2) %>% 
  group_by(X3) %>% 
  summarise(min = min(Total), 
            max = max(Total))
  
results %>% mutate(Total = X1 + X2) %>% filter(Total == 33)
results %>% mutate(Total = X1 + X2) %>% filter(Total == 30 & X3 == 3)

results %>% 
  mutate(Total = X1 + X2, Crit = round((1-X1/obswin)*(1-X2/obswin),3)) %>% 
  group_by(X3) %>% 
  summarise(min = min(Crit), max = max(Crit))


library(ggplot2)
results %>% 
  ggplot(aes(x = X2, y = X1, fill = as.factor(X3))) +
  geom_tile() +
  geom_abline(intercept = 30, slope = -1, col = 'red', linetype = 'dashed', 
              size = 1.25) +
  geom_abline(intercept = 34, slope = -1, col = 'red', linetype = 'dashed', 
              size = 1.2) +
  geom_abline(intercept = 93, slope = -1, col = 'red', linetype = 'dashed',
              size = 1.2)+
  geom_abline(intercept = 96, slope = -1, col = 'red', linetype = 'dashed',
              size = 1.2)+
  scale_fill_brewer(palette = 'Paired') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
  

### cohorts of 3 
  prior <- c(0.1, 0.2, 0.3)
  target <- 0.2
  obswin <- 56
  
  level <- c(1,1,1)
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
    mod <- titecrm(prior = prior, target = target, tox = tox, level = level, 
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
