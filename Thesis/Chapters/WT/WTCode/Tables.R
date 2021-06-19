library(dplyr)
library(tibble)
library(kableExtra)

# Table for selection probabilities for different combos of AR size and 
# randomisation to control probabilities 
load("H:/PhD/Thesis/Chapters/WT/WTCode/RProbComboSimsData.RData")

data <- rbind(
  c(s1tox, 0, s7tox, 0),
  c(s1eff, 0, s7eff, 0),
  c(n0p20sims1$ProbSelect, n0p20sims1$ProbStop, 
    n0p20sims7$ProbSelect, n0p20sims7$ProbStop),
  c(n15p20sims1$ProbSelect, n15p20sims1$ProbStop, 
    n15p20sims7$ProbSelect, n15p20sims7$ProbStop),
  c(n30p20sims1$ProbSelect, n30p20sims1$ProbStop, 
    n30p20sims7$ProbSelect, n30p20sims7$ProbStop),
  c(n45p20sims1$ProbSelect, n45p20sims1$ProbStop, 
    n45p20sims7$ProbSelect, n45p20sims7$ProbStop),
  c(n60p20sims1$ProbSelect, n60p20sims1$ProbStop, 
    n60p20sims7$ProbSelect, n60p20sims7$ProbStop),
  c(n0p33sims1$ProbSelect, n0p33sims1$ProbStop, 
    n0p33sims7$ProbSelect, n0p33sims7$ProbStop),
  c(n15p33sims1$ProbSelect, n15p33sims1$ProbStop, 
    n15p33sims7$ProbSelect, n15p33sims7$ProbStop),
  c(n30p33sims1$ProbSelect, n30p33sims1$ProbStop, 
    n30p33sims7$ProbSelect, n30p33sims7$ProbStop),
  c(n45p33sims1$ProbSelect, n45p33sims1$ProbStop, 
    n45p33sims7$ProbSelect, n45p33sims7$ProbStop),
  c(n60p33sims1$ProbSelect, n60p33sims1$ProbStop, 
    n60p33sims7$ProbSelect, n60p33sims7$ProbStop),
  
  c(s2tox, 0, s8tox, 0),
  c(s2eff, 0, s8eff, 0),
  c(n0p20sims2$ProbSelect, n0p20sims2$ProbStop, 
    n0p20sims8$ProbSelect, n0p20sims8$ProbStop),
  c(n15p20sims2$ProbSelect, n15p20sims2$ProbStop, 
    n15p20sims8$ProbSelect, n15p20sims8$ProbStop),
  c(n30p20sims2$ProbSelect, n30p20sims2$ProbStop, 
    n30p20sims8$ProbSelect, n30p20sims8$ProbStop),
  c(n45p20sims2$ProbSelect, n45p20sims2$ProbStop, 
    n45p20sims8$ProbSelect, n45p20sims8$ProbStop),
  c(n60p20sims2$ProbSelect, n60p20sims2$ProbStop, 
    n60p20sims8$ProbSelect, n60p20sims8$ProbStop),
  c(n0p33sims2$ProbSelect, n0p33sims2$ProbStop, 
    n0p33sims8$ProbSelect, n0p33sims8$ProbStop),
  c(n15p33sims2$ProbSelect, n15p33sims2$ProbStop, 
    n15p33sims8$ProbSelect, n15p33sims8$ProbStop),
  c(n30p33sims2$ProbSelect, n30p33sims2$ProbStop, 
    n30p33sims8$ProbSelect, n30p33sims8$ProbStop),
  c(n45p33sims2$ProbSelect, n45p33sims2$ProbStop, 
    n45p33sims8$ProbSelect, n45p33sims8$ProbStop),
  c(n60p33sims2$ProbSelect, n60p33sims2$ProbStop, 
    n60p33sims8$ProbSelect, n60p33sims8$ProbStop),
  
  c(s3tox, 0, s9tox, 0),
  c(s3eff, 0, s9eff, 0),
  c(n0p20sims3$ProbSelect, n0p20sims3$ProbStop, 
    n0p20sims9$ProbSelect, n0p20sims9$ProbStop),
  c(n15p20sims3$ProbSelect, n15p20sims3$ProbStop, 
    n15p20sims9$ProbSelect, n15p20sims9$ProbStop),
  c(n30p20sims3$ProbSelect, n30p20sims3$ProbStop, 
    n30p20sims9$ProbSelect, n30p20sims9$ProbStop),
  c(n45p20sims3$ProbSelect, n45p20sims3$ProbStop, 
    n45p20sims9$ProbSelect, n45p20sims9$ProbStop),
  c(n60p20sims3$ProbSelect, n60p20sims3$ProbStop, 
    n60p20sims9$ProbSelect, n60p20sims9$ProbStop),
  c(n0p33sims3$ProbSelect, n0p33sims3$ProbStop, 
    n0p33sims9$ProbSelect, n0p33sims9$ProbStop),
  c(n15p33sims3$ProbSelect, n15p33sims3$ProbStop, 
    n15p33sims9$ProbSelect, n15p33sims9$ProbStop),
  c(n30p33sims3$ProbSelect, n30p33sims3$ProbStop, 
    n30p33sims9$ProbSelect, n30p33sims9$ProbStop),
  c(n45p33sims3$ProbSelect, n45p33sims3$ProbStop, 
    n45p33sims9$ProbSelect, n45p33sims9$ProbStop),
  c(n60p33sims3$ProbSelect, n60p33sims3$ProbStop, 
    n60p33sims9$ProbSelect, n60p33sims9$ProbStop),
  
  c(s4tox, 0, s10tox, 0),
  c(s4eff, 0, s10eff, 0),
  c(n0p20sims4$ProbSelect, n0p20sims4$ProbStop, 
    n0p20sims10$ProbSelect, n0p20sims10$ProbStop),
  c(n15p20sims4$ProbSelect, n15p20sims4$ProbStop, 
    n15p20sims10$ProbSelect, n15p20sims10$ProbStop),
  c(n30p20sims4$ProbSelect, n30p20sims4$ProbStop, 
    n30p20sims10$ProbSelect, n30p20sims10$ProbStop),
  c(n45p20sims4$ProbSelect, n45p20sims4$ProbStop, 
    n45p20sims10$ProbSelect, n45p20sims10$ProbStop),
  c(n60p20sims4$ProbSelect, n60p20sims4$ProbStop, 
    n60p20sims10$ProbSelect, n60p20sims10$ProbStop),
  c(n0p33sims4$ProbSelect, n0p33sims4$ProbStop, 
    n0p33sims10$ProbSelect, n0p33sims10$ProbStop),
  c(n15p33sims4$ProbSelect, n15p33sims4$ProbStop, 
    n15p33sims10$ProbSelect, n15p33sims10$ProbStop),
  c(n30p33sims4$ProbSelect, n30p33sims4$ProbStop, 
    n30p33sims10$ProbSelect, n30p33sims10$ProbStop),
  c(n45p33sims4$ProbSelect, n45p33sims4$ProbStop, 
    n45p33sims10$ProbSelect, n45p33sims10$ProbStop),
  c(n60p33sims4$ProbSelect, n60p33sims4$ProbStop, 
    n60p33sims10$ProbSelect, n60p33sims10$ProbStop),
  
  c(s5tox, 0, s11tox, 0),
  c(s5eff, 0, s11eff, 0),
  c(n0p20sims5$ProbSelect, n0p20sims5$ProbStop, 
    n0p20sims11$ProbSelect, n0p20sims11$ProbStop),
  c(n15p20sims5$ProbSelect, n15p20sims5$ProbStop, 
    n15p20sims11$ProbSelect, n15p20sims11$ProbStop),
  c(n30p20sims5$ProbSelect, n30p20sims5$ProbStop, 
    n30p20sims11$ProbSelect, n30p20sims11$ProbStop),
  c(n45p20sims5$ProbSelect, n45p20sims5$ProbStop, 
    n45p20sims11$ProbSelect, n45p20sims11$ProbStop),
  c(n60p20sims5$ProbSelect, n60p20sims5$ProbStop, 
    n60p20sims11$ProbSelect, n60p20sims11$ProbStop),
  c(n0p33sims5$ProbSelect, n0p33sims5$ProbStop, 
    n0p33sims11$ProbSelect, n0p33sims11$ProbStop),
  c(n15p33sims5$ProbSelect, n15p33sims5$ProbStop, 
    n15p33sims11$ProbSelect, n15p33sims11$ProbStop),
  c(n30p33sims5$ProbSelect, n30p33sims5$ProbStop, 
    n30p33sims11$ProbSelect, n30p33sims11$ProbStop),
  c(n45p33sims5$ProbSelect, n45p33sims5$ProbStop, 
    n45p33sims11$ProbSelect, n45p33sims11$ProbStop),
  c(n60p33sims5$ProbSelect, n60p33sims5$ProbStop, 
    n60p33sims11$ProbSelect, n60p33sims11$ProbStop),
  
  c(s6tox, 0, s12tox, 0),
  c(s6eff, 0, s12eff, 0),
  c(n0p20sims6$ProbSelect, n0p20sims6$ProbStop, 
    n0p20sims12$ProbSelect, n0p20sims12$ProbStop),
  c(n15p20sims6$ProbSelect, n15p20sims6$ProbStop, 
    n15p20sims12$ProbSelect, n15p20sims12$ProbStop),
  c(n30p20sims6$ProbSelect, n30p20sims6$ProbStop, 
    n30p20sims12$ProbSelect, n30p20sims12$ProbStop),
  c(n45p20sims6$ProbSelect, n45p20sims6$ProbStop, 
    n45p20sims12$ProbSelect, n45p20sims12$ProbStop),
  c(n60p20sims6$ProbSelect, n60p20sims6$ProbStop, 
    n60p20sims12$ProbSelect, n60p20sims12$ProbStop),
  c(n0p33sims6$ProbSelect, n0p33sims6$ProbStop, 
    n0p33sims12$ProbSelect, n0p33sims12$ProbStop),
  c(n15p33sims6$ProbSelect, n15p33sims6$ProbStop, 
    n15p33sims12$ProbSelect, n15p33sims12$ProbStop),
  c(n30p33sims6$ProbSelect, n30p33sims6$ProbStop, 
    n30p33sims12$ProbSelect, n30p33sims12$ProbStop),
  c(n45p33sims6$ProbSelect, n45p33sims6$ProbStop, 
    n45p33sims12$ProbSelect, n45p33sims12$ProbStop),
  c(n60p33sims6$ProbSelect, n60p33sims6$ProbStop, 
    n60p33sims12$ProbSelect, n60p33sims12$ProbStop)
)

data <- round(data, digits = 2)

phiR <- rep(c(99,99,rep(c(0.2,0.33), each = 5)), times = 6)
nAR <- rep(c(99,99, rep(c(0,15,30,45,60), times = 2)), times = 6) 

scenarioL <- c('P(tox)', 'P(eff)', rep(1, times = 10),
               'P(tox)', 'P(eff)', rep(2, times = 10),
               'P(tox)', 'P(eff)', rep(3, times = 10),
               'P(tox)', 'P(eff)', rep(4, times = 10),
               'P(tox)', 'P(eff)', rep(5, times = 10),
               'P(tox)', 'P(eff)', rep(6, times = 10))
  
scenarioR <- c('P(tox)', 'P(eff)', rep(7, times = 10),
               'P(tox)', 'P(eff)', rep(8, times = 10),
               'P(tox)', 'P(eff)', rep(9, times = 10),
               'P(tox)', 'P(eff)', rep(10, times = 10),
               'P(tox)', 'P(eff)', rep(11, times = 10),
               'P(tox)', 'P(eff)', rep(12, times = 10))

data <- data.frame(cbind(scenarioL, phiR, nAR, data))
data <- add_column(data, nAR2 = nAR, .after = 9 )
data <- add_column(data, phiR2 = phiR, .after = 9 )
data <- add_column(data, scenarioR = scenarioR, .after = 9 )

data <- data %>% 
  mutate(nAR = if_else(nAR == 99, ' ', nAR),
         nAR2 = if_else(nAR2 == 99, ' ', nAR),
         phiR = if_else(phiR == 99, ' ', phiR),
         phiR2 = if_else(phiR2 == 99, ' ', phiR),
         V9 = if_else(scenarioL == 'P(eff)', ' ', V9),
         V9 = if_else(scenarioL == 'P(tox)', ' ', V9),
         V15 = if_else(scenarioL == 'P(eff)', ' ', V15),
         V15 = if_else(scenarioL == 'P(tox)', ' ', V15)
         )

colnames(data) <- c('Scenario', '$phi_R$', '$j_R$', 'Dose 1', 'Dose 2', 'Dose 3', 'Dose 4', 'Dose 5', 'Stop',
                    'Scenario', '$phi_R$', '$j_R$', 'Dose 1', 'Dose 2', 'Dose 3', 'Dose 4', 'Dose 5', 'Stop')

#View(data)

datacode <- kable(data, "latex", booktabs = T, linesep = "", align = "c", 
                  caption = '\\label{tab_wt:SelectProbCombos}Selection probabilities for multiple combinations of AR phase size and probabilities for randomisation to control.')%>%
  column_spec(9, border_right = T) %>% 
  kable_styling(latex_options = c('scale_down'), position = 'center',
                font_size = 9 ) %>%
  collapse_rows(columns = c(1,10), latex_hline = "major",  valign = "middle")

cat(datacode)

rm(list = ls(all.names = TRUE))

# Table for MEAN Number of patients for different combos of AR size and 
# randomisation to control probabilities 
load("H:/PhD/Thesis/Chapters/WT/WTCode/RProbComboSimsData.RData")

data <- rbind(
  c(s1tox, 0, s7tox, 0),
  c(s1eff, 0, s7eff, 0),
  c(n0p20sims1$TreatedAtDose, sum(n0p20sims1$TreatedAtDose), 
    n0p20sims7$TreatedAtDose, sum(n0p20sims7$TreatedAtDose)),
  c(n15p20sims1$TreatedAtDose, sum(n15p20sims1$TreatedAtDose), 
    n15p20sims7$TreatedAtDose, sum(n15p20sims7$TreatedAtDose)),
  c(n30p20sims1$TreatedAtDose, sum(n30p20sims1$TreatedAtDose), 
    n30p20sims7$TreatedAtDose, sum(n30p20sims7$TreatedAtDose)),
  c(n45p20sims1$TreatedAtDose, sum(n45p20sims1$TreatedAtDose), 
    n45p20sims7$TreatedAtDose, sum(n45p20sims7$TreatedAtDose)),
  c(n60p20sims1$TreatedAtDose, sum(n60p20sims1$TreatedAtDose), 
    n60p20sims7$TreatedAtDose, sum(n60p20sims7$TreatedAtDose)),
  c(n0p33sims1$TreatedAtDose, sum(n0p33sims1$TreatedAtDose), 
    n0p33sims7$TreatedAtDose, sum(n0p33sims7$TreatedAtDose)),
  c(n15p33sims1$TreatedAtDose, sum(n15p33sims1$TreatedAtDose), 
    n15p33sims7$TreatedAtDose, sum(n15p33sims7$TreatedAtDose)),
  c(n30p33sims1$TreatedAtDose, sum(n30p33sims1$TreatedAtDose), 
    n30p33sims7$TreatedAtDose, sum(n30p33sims7$TreatedAtDose)),
  c(n45p33sims1$TreatedAtDose, sum(n45p33sims1$TreatedAtDose), 
    n45p33sims7$TreatedAtDose, sum(n45p33sims7$TreatedAtDose)),
  c(n60p33sims1$TreatedAtDose, sum(n60p33sims1$TreatedAtDose), 
    n60p33sims7$TreatedAtDose, sum(n60p33sims7$TreatedAtDose)),
  
  c(s2tox, 0, s8tox, 0),
  c(s2eff, 0, s8eff, 0),
  c(n0p20sims2$TreatedAtDose, sum(n0p20sims2$TreatedAtDose), 
    n0p20sims8$TreatedAtDose, sum(n0p20sims8$TreatedAtDose)),
  c(n15p20sims2$TreatedAtDose, sum(n15p20sims2$TreatedAtDose), 
    n15p20sims8$TreatedAtDose, sum(n15p20sims8$TreatedAtDose)),
  c(n30p20sims2$TreatedAtDose, sum(n30p20sims2$TreatedAtDose), 
    n30p20sims8$TreatedAtDose, sum(n30p20sims8$TreatedAtDose)),
  c(n45p20sims2$TreatedAtDose, sum(n45p20sims2$TreatedAtDose), 
    n45p20sims8$TreatedAtDose, sum(n45p20sims8$TreatedAtDose)),
  c(n60p20sims2$TreatedAtDose, sum(n60p20sims2$TreatedAtDose), 
    n60p20sims8$TreatedAtDose, sum(n60p20sims8$TreatedAtDose)),
  c(n0p33sims2$TreatedAtDose, sum(n0p33sims2$TreatedAtDose), 
    n0p33sims8$TreatedAtDose, sum(n0p33sims8$TreatedAtDose)),
  c(n15p33sims2$TreatedAtDose, sum(n15p33sims2$TreatedAtDose), 
    n15p33sims8$TreatedAtDose, sum(n15p33sims8$TreatedAtDose)),
  c(n30p33sims2$TreatedAtDose, sum(n30p33sims2$TreatedAtDose), 
    n30p33sims8$TreatedAtDose, sum(n30p33sims8$TreatedAtDose)),
  c(n45p33sims2$TreatedAtDose, sum(n45p33sims2$TreatedAtDose), 
    n45p33sims8$TreatedAtDose, sum(n45p33sims8$TreatedAtDose)),
  c(n60p33sims2$TreatedAtDose, sum(n60p33sims2$TreatedAtDose), 
    n60p33sims8$TreatedAtDose, sum(n60p33sims8$TreatedAtDose)),
  
  c(s3tox, 0, s9tox, 0),
  c(s3eff, 0, s9eff, 0),
  c(n0p20sims3$TreatedAtDose, sum(n0p20sims3$TreatedAtDose), 
    n0p20sims9$TreatedAtDose, sum(n0p20sims9$TreatedAtDose)),
  c(n15p20sims3$TreatedAtDose, sum(n15p20sims3$TreatedAtDose), 
    n15p20sims9$TreatedAtDose, sum(n15p20sims9$TreatedAtDose)),
  c(n30p20sims3$TreatedAtDose, sum(n30p20sims3$TreatedAtDose), 
    n30p20sims9$TreatedAtDose, sum(n30p20sims9$TreatedAtDose)),
  c(n45p20sims3$TreatedAtDose, sum(n45p20sims3$TreatedAtDose), 
    n45p20sims9$TreatedAtDose, sum(n45p20sims9$TreatedAtDose)),
  c(n60p20sims3$TreatedAtDose, sum(n60p20sims3$TreatedAtDose), 
    n60p20sims9$TreatedAtDose, sum(n60p20sims9$TreatedAtDose)),
  c(n0p33sims3$TreatedAtDose, sum(n0p33sims3$TreatedAtDose), 
    n0p33sims9$TreatedAtDose, sum(n0p33sims9$TreatedAtDose)),
  c(n15p33sims3$TreatedAtDose, sum(n15p33sims3$TreatedAtDose), 
    n15p33sims9$TreatedAtDose, sum(n15p33sims9$TreatedAtDose)),
  c(n30p33sims3$TreatedAtDose, sum(n30p33sims3$TreatedAtDose), 
    n30p33sims9$TreatedAtDose, sum(n30p33sims9$TreatedAtDose)),
  c(n45p33sims3$TreatedAtDose, sum(n45p33sims3$TreatedAtDose), 
    n45p33sims9$TreatedAtDose, sum(n45p33sims9$TreatedAtDose)),
  c(n60p33sims3$TreatedAtDose, sum(n60p33sims3$TreatedAtDose), 
    n60p33sims9$TreatedAtDose, sum(n60p33sims9$TreatedAtDose)),
  
  c(s4tox, 0, s10tox, 0),
  c(s4eff, 0, s10eff, 0),
  c(n0p20sims4$TreatedAtDose, sum(n0p20sims4$TreatedAtDose), 
    n0p20sims10$TreatedAtDose, sum(n0p20sims10$TreatedAtDose)),
  c(n15p20sims4$TreatedAtDose, sum(n15p20sims4$TreatedAtDose), 
    n15p20sims10$TreatedAtDose, sum(n15p20sims10$TreatedAtDose)),
  c(n30p20sims4$TreatedAtDose, sum(n30p20sims4$TreatedAtDose), 
    n30p20sims10$TreatedAtDose, sum(n30p20sims10$TreatedAtDose)),
  c(n45p20sims4$TreatedAtDose, sum(n45p20sims4$TreatedAtDose), 
    n45p20sims10$TreatedAtDose, sum(n45p20sims10$TreatedAtDose)),
  c(n60p20sims4$TreatedAtDose, sum(n60p20sims4$TreatedAtDose), 
    n60p20sims10$TreatedAtDose, sum(n60p20sims10$TreatedAtDose)),
  c(n0p33sims4$TreatedAtDose, sum(n0p33sims4$TreatedAtDose), 
    n0p33sims10$TreatedAtDose, sum(n0p33sims10$TreatedAtDose)),
  c(n15p33sims4$TreatedAtDose, sum(n15p33sims4$TreatedAtDose), 
    n15p33sims10$TreatedAtDose, sum(n15p33sims10$TreatedAtDose)),
  c(n30p33sims4$TreatedAtDose, sum(n30p33sims4$TreatedAtDose), 
    n30p33sims10$TreatedAtDose, sum(n30p33sims10$TreatedAtDose)),
  c(n45p33sims4$TreatedAtDose, sum(n45p33sims4$TreatedAtDose), 
    n45p33sims10$TreatedAtDose, sum(n45p33sims10$TreatedAtDose)),
  c(n60p33sims4$TreatedAtDose, sum(n60p33sims4$TreatedAtDose), 
    n60p33sims10$TreatedAtDose, sum(n60p33sims10$TreatedAtDose)),
  
  c(s5tox, 0, s11tox, 0),
  c(s5eff, 0, s11eff, 0),
  c(n0p20sims5$TreatedAtDose, sum(n0p20sims5$TreatedAtDose), 
    n0p20sims11$TreatedAtDose, sum(n0p20sims11$TreatedAtDose)),
  c(n15p20sims5$TreatedAtDose, sum(n15p20sims5$TreatedAtDose), 
    n15p20sims11$TreatedAtDose, sum(n15p20sims11$TreatedAtDose)),
  c(n30p20sims5$TreatedAtDose, sum(n30p20sims5$TreatedAtDose), 
    n30p20sims11$TreatedAtDose, sum(n30p20sims11$TreatedAtDose)),
  c(n45p20sims5$TreatedAtDose, sum(n45p20sims5$TreatedAtDose), 
    n45p20sims11$TreatedAtDose, sum(n45p20sims11$TreatedAtDose)),
  c(n60p20sims5$TreatedAtDose, sum(n60p20sims5$TreatedAtDose), 
    n60p20sims11$TreatedAtDose, sum(n60p20sims11$TreatedAtDose)),
  c(n0p33sims5$TreatedAtDose, sum(n0p33sims5$TreatedAtDose), 
    n0p33sims11$TreatedAtDose, sum(n0p33sims11$TreatedAtDose)),
  c(n15p33sims5$TreatedAtDose, sum(n15p33sims5$TreatedAtDose), 
    n15p33sims11$TreatedAtDose, sum(n15p33sims11$TreatedAtDose)),
  c(n30p33sims5$TreatedAtDose, sum(n30p33sims5$TreatedAtDose), 
    n30p33sims11$TreatedAtDose, sum(n30p33sims11$TreatedAtDose)),
  c(n45p33sims5$TreatedAtDose, sum(n45p33sims5$TreatedAtDose), 
    n45p33sims11$TreatedAtDose, sum(n45p33sims11$TreatedAtDose)),
  c(n60p33sims5$TreatedAtDose, sum(n60p33sims5$TreatedAtDose), 
    n60p33sims11$TreatedAtDose, sum(n60p33sims11$TreatedAtDose)),
  
  c(s6tox, 0, s12tox, 0),
  c(s6eff, 0, s12eff, 0),
  c(n0p20sims6$TreatedAtDose, sum(n0p20sims6$TreatedAtDose), 
    n0p20sims12$TreatedAtDose, sum(n0p20sims12$TreatedAtDose)),
  c(n15p20sims6$TreatedAtDose, sum(n15p20sims6$TreatedAtDose), 
    n15p20sims12$TreatedAtDose, sum(n15p20sims12$TreatedAtDose)),
  c(n30p20sims6$TreatedAtDose, sum(n30p20sims6$TreatedAtDose), 
    n30p20sims12$TreatedAtDose, sum(n30p20sims12$TreatedAtDose)),
  c(n45p20sims6$TreatedAtDose, sum(n45p20sims6$TreatedAtDose), 
    n45p20sims12$TreatedAtDose, sum(n45p20sims12$TreatedAtDose)),
  c(n60p20sims6$TreatedAtDose, sum(n60p20sims6$TreatedAtDose), 
    n60p20sims12$TreatedAtDose, sum(n60p20sims12$TreatedAtDose)),
  c(n0p33sims6$TreatedAtDose, sum(n0p33sims6$TreatedAtDose), 
    n0p33sims12$TreatedAtDose, sum(n0p33sims12$TreatedAtDose)),
  c(n15p33sims6$TreatedAtDose, sum(n15p33sims6$TreatedAtDose), 
    n15p33sims12$TreatedAtDose, sum(n15p33sims12$TreatedAtDose)),
  c(n30p33sims6$TreatedAtDose, sum(n30p33sims6$TreatedAtDose), 
    n30p33sims12$TreatedAtDose, sum(n30p33sims12$TreatedAtDose)),
  c(n45p33sims6$TreatedAtDose, sum(n45p33sims6$TreatedAtDose), 
    n45p33sims12$TreatedAtDose, sum(n45p33sims12$TreatedAtDose)),
  c(n60p33sims6$TreatedAtDose, sum(n60p33sims6$TreatedAtDose), 
    n60p33sims12$TreatedAtDose, sum(n60p33sims12$TreatedAtDose))
)



phiR <- rep(c(99,99,rep(c(0.2,0.33), each = 5)), times = 6)
nAR <- rep(c(99,99, rep(c(0,15,30,45,60), times = 2)), times = 6) 

scenarioL <- c('P(tox)', 'P(eff)', rep(1, times = 10),
               'P(tox)', 'P(eff)', rep(2, times = 10),
               'P(tox)', 'P(eff)', rep(3, times = 10),
               'P(tox)', 'P(eff)', rep(4, times = 10),
               'P(tox)', 'P(eff)', rep(5, times = 10),
               'P(tox)', 'P(eff)', rep(6, times = 10))

scenarioR <- c('P(tox)', 'P(eff)', rep(7, times = 10),
               'P(tox)', 'P(eff)', rep(8, times = 10),
               'P(tox)', 'P(eff)', rep(9, times = 10),
               'P(tox)', 'P(eff)', rep(10, times = 10),
               'P(tox)', 'P(eff)', rep(11, times = 10),
               'P(tox)', 'P(eff)', rep(12, times = 10))

data <- data.frame(cbind(scenarioL, phiR, nAR, data))
data <- add_column(data, nAR2 = nAR, .after = 9 )
data <- add_column(data, phiR2 = phiR, .after = 9 )
data <- add_column(data, scenarioR = scenarioR, .after = 9 )

data <- data %>% 
  mutate(nAR = if_else(nAR == 99, ' ', nAR),
         nAR2 = if_else(nAR2 == 99, ' ', nAR),
         phiR = if_else(phiR == 99, ' ', phiR),
         phiR2 = if_else(phiR2 == 99, ' ', phiR),
         V9 = if_else(scenarioL == 'P(eff)', ' ', V9),
         V9 = if_else(scenarioL == 'P(tox)', ' ', V9),
         V15 = if_else(scenarioL == 'P(eff)', ' ', V15),
         V15 = if_else(scenarioL == 'P(tox)', ' ', V15)
         )

colnames(data) <- c('Scenario', '$phi_R$', '$j_R$', 'Dose 1', 'Dose 2', 'Dose 3', 'Dose 4', 'Dose 5', 'Total',
                    'Scenario', '$phi_R$', '$j_R$', 'Dose 1', 'Dose 2', 'Dose 3', 'Dose 4', 'Dose 5', 'Total')

#View(data)

datacode <- kable(data, "latex", booktabs = T, linesep = "", align = "c", 
                  caption = '\\label{tab_wt:MeanNCombos}Mean number of patients treated at each dose for multiple combinations of AR phase size and probabilities for randomisation to control.')%>%
  column_spec(9, border_right = T) %>% 
  kable_styling(latex_options = c('scale_down'), position = 'center',
                font_size = 9 ) %>%
  collapse_rows(columns = c(1,10), latex_hline = "major",  valign = "middle")

cat(datacode)

rm(list = ls(all.names = TRUE))

# Table of selection probabilities comparing three alternative designs 
load("H:/PhD/Thesis/Chapters/WT/WTCode/AltDesignsSimsData.RData")
load("H:/PhD/Thesis/Chapters/WT/WTCode/RProbComboSimsData.RData")

data <- rbind(
  c(s1tox, 0, s7tox, 0),
  c(s1eff, 0, s7eff, 0),
  c(n45p20sims1$ProbSelect, n45p20sims1$ProbStop,
    n45p20sims7$ProbSelect, n45p20sims7$ProbStop),
  c(WTsims1$ProbSelect, WTsims1$ProbStop,
    WTsims7$ProbSelect, WTsims7$ProbStop),
  c(99, TwoArmsims1$ProbSelect, TwoArmsims1$ProbStop,
    99, TwoArmsims7$ProbSelect, TwoArmsims7$ProbStop),
  
  c(s2tox, 0, s8tox, 0),
  c(s2eff, 0, s8eff, 0),
  c(n45p20sims2$ProbSelect, n45p20sims2$ProbStop,
    n45p20sims8$ProbSelect, n45p20sims8$ProbStop),
  c(WTsims2$ProbSelect, WTsims2$ProbStop,
    WTsims8$ProbSelect, WTsims8$ProbStop),
  c(99, TwoArmsims2$ProbSelect, TwoArmsims2$ProbStop,
    99, TwoArmsims8$ProbSelect, TwoArmsims8$ProbStop),
  
  c(s3tox, 0, s9tox, 0),
  c(s3eff, 0, s9eff, 0),
  c(n45p20sims3$ProbSelect, n45p20sims3$ProbStop,
    n45p20sims9$ProbSelect, n45p20sims9$ProbStop),
  c(WTsims3$ProbSelect, WTsims3$ProbStop,
    WTsims9$ProbSelect, WTsims9$ProbStop),
  c(99, TwoArmsims3$ProbSelect, TwoArmsims3$ProbStop,
    99, TwoArmsims9$ProbSelect, TwoArmsims9$ProbStop),
  
  c(s4tox, 0, s10tox, 0),
  c(s4eff, 0, s1eff, 0),
  c(n45p20sims4$ProbSelect, n45p20sims4$ProbStop,
    n45p20sims10$ProbSelect, n45p20sims10$ProbStop),
  c(WTsims4$ProbSelect, WTsims4$ProbStop,
    WTsims10$ProbSelect, WTsims10$ProbStop),
  c(99, TwoArmsims4$ProbSelect, TwoArmsims4$ProbStop,
    99, TwoArmsims10$ProbSelect, TwoArmsims10$ProbStop),
  
  c(s5tox, 0, s11tox, 0),
  c(s5eff, 0, s11eff, 0),
  c(n45p20sims5$ProbSelect, n45p20sims5$ProbStop,
    n45p20sims11$ProbSelect, n45p20sims11$ProbStop),
  c(WTsims5$ProbSelect, WTsims5$ProbStop,
    WTsims11$ProbSelect, WTsims11$ProbStop),
  c(99, TwoArmsims5$ProbSelect, TwoArmsims5$ProbStop,
    99, TwoArmsims11$ProbSelect, TwoArmsims11$ProbStop),
  
  c(s6tox, 0, s12tox, 0),
  c(s6eff, 0, s12eff, 0),
  c(n45p20sims6$ProbSelect, n45p20sims6$ProbStop,
    n45p20sims12$ProbSelect, n45p20sims12$ProbStop),
  c(WTsims6$ProbSelect, WTsims6$ProbStop,
    WTsims12$ProbSelect, WTsims12$ProbStop),
  c(99, TwoArmsims6$ProbSelect, TwoArmsims6$ProbStop,
    99, TwoArmsims12$ProbSelect, TwoArmsims12$ProbStop)
  
  )

data <- round(data, digits = 2)

design <- rep(c(99, 99, 'RtC-WT', 'WT', 'TwoArm'), times = 6) 

scenarioL <- c('P(tox)', 'P(eff)', rep(1, times = 3),
               'P(tox)', 'P(eff)', rep(2, times = 3),
               'P(tox)', 'P(eff)', rep(3, times = 3),
               'P(tox)', 'P(eff)', rep(4, times = 3),
               'P(tox)', 'P(eff)', rep(5, times = 3),
               'P(tox)', 'P(eff)', rep(6, times = 3))

scenarioR <- c('P(tox)', 'P(eff)', rep(7, times = 3),
               'P(tox)', 'P(eff)', rep(8, times = 3),
               'P(tox)', 'P(eff)', rep(9, times = 3),
               'P(tox)', 'P(eff)', rep(10, times = 3),
               'P(tox)', 'P(eff)', rep(11, times = 3),
               'P(tox)', 'P(eff)', rep(12, times = 3))

data <- data.frame(cbind(scenarioL, design, data))
data <- add_column(data, design = design, .after = 8 )
data <- add_column(data, scenarioR = scenarioR, .after = 8 )

data <- data %>% 
  mutate(V8 = if_else(scenarioL == 'P(eff)', ' ', V8),
         V8 = if_else(scenarioL == 'P(tox)', ' ', V8),
         V14 = if_else(scenarioL == 'P(eff)', ' ', V14),
         V14 = if_else(scenarioL == 'P(tox)', ' ', V14)
  )
data[data == 99] <- c(' ')

colnames(data) <- c('Scenario', 'Design', 'Dose 1', 'Dose 2', 'Dose 3', 'Dose 4', 'Dose 5', 'Stop',
                    'Scenario', 'Design', 'Dose 1', 'Dose 2', 'Dose 3', 'Dose 4', 'Dose 5', 'Stop')
#View(data)

datacode <- kable(data, "latex", booktabs = T, linesep = "", align = "c", 
                  caption = '\\label{tab_wt:SelectProbAlt}Selection probabilities for alternative trial designs.')%>%
  column_spec(8, border_right = T) %>% 
  kable_styling(latex_options = c('scale_down'), position = 'center',
                font_size = 12 ) %>%
  collapse_rows(columns = c(1,9), latex_hline = "major",  valign = "middle")

cat(datacode)

rm(list = ls(all.names = TRUE))

# Table of mean number of patients comparing three alternative designs 
load("H:/PhD/Thesis/Chapters/WT/WTCode/AltDesignsSimsData.RData")
load("H:/PhD/Thesis/Chapters/WT/WTCode/RProbComboSimsData.RData")

data <- rbind(
  c(s1tox, 0, s7tox, 0),
  c(s1eff, 0, s7eff, 0),
  c(n45p20sims1$TreatedAtDose, sum(n45p20sims1$TreatedAtDose),
    n45p20sims7$TreatedAtDose, sum(n45p20sims7$TreatedAtDose)),
  c(WTsims1$TreatedAtDose, sum(WTsims1$TreatedAtDose),
    WTsims7$TreatedAtDose, sum(WTsims7$TreatedAtDose)),
  c(99, TwoArmsims1$TreatedAtDose, sum(TwoArmsims1$TreatedAtDose),
    99, TwoArmsims7$TreatedAtDose, sum(TwoArmsims7$TreatedAtDose)),
  
  c(s2tox, 0, s8tox, 0),
  c(s2eff, 0, s8eff, 0),
  c(n45p20sims2$TreatedAtDose, sum(n45p20sims2$TreatedAtDose),
    n45p20sims8$TreatedAtDose, sum(n45p20sims8$TreatedAtDose)),
  c(WTsims2$TreatedAtDose, sum(WTsims2$TreatedAtDose),
    WTsims8$TreatedAtDose, sum(WTsims8$TreatedAtDose)),
  c(99, TwoArmsims2$TreatedAtDose, sum(TwoArmsims2$TreatedAtDose),
    99, TwoArmsims8$TreatedAtDose, sum(TwoArmsims8$TreatedAtDose)),
  
  c(s3tox, 0, s9tox, 0),
  c(s3eff, 0, s9eff, 0),
  c(n45p20sims3$TreatedAtDose, sum(n45p20sims3$TreatedAtDose),
    n45p20sims9$TreatedAtDose, sum(n45p20sims9$TreatedAtDose)),
  c(WTsims3$TreatedAtDose, sum(WTsims3$TreatedAtDose),
    WTsims9$TreatedAtDose, sum(WTsims9$TreatedAtDose)),
  c(99, TwoArmsims3$TreatedAtDose, sum(TwoArmsims3$TreatedAtDose),
    99, TwoArmsims9$TreatedAtDose, sum(TwoArmsims9$TreatedAtDose)),
  
  c(s4tox, 0, s10tox, 0),
  c(s4eff, 0, s1eff, 0),
  c(n45p20sims4$TreatedAtDose, sum(n45p20sims4$TreatedAtDose),
    n45p20sims10$TreatedAtDose, sum(n45p20sims10$TreatedAtDose)),
  c(WTsims4$TreatedAtDose, sum(WTsims4$TreatedAtDose),
    WTsims10$TreatedAtDose, sum(WTsims10$TreatedAtDose)),
  c(99, TwoArmsims4$TreatedAtDose, sum(TwoArmsims4$TreatedAtDose),
    99, TwoArmsims10$TreatedAtDose, sum(TwoArmsims10$TreatedAtDose)),
  
  c(s5tox, 0, s11tox, 0),
  c(s5eff, 0, s11eff, 0),
  c(n45p20sims5$TreatedAtDose, sum(n45p20sims5$TreatedAtDose),
    n45p20sims11$TreatedAtDose, sum(n45p20sims11$TreatedAtDose)),
  c(WTsims5$TreatedAtDose, sum(WTsims5$TreatedAtDose),
    WTsims11$TreatedAtDose, sum(WTsims11$TreatedAtDose)),
  c(99, TwoArmsims5$TreatedAtDose, sum(TwoArmsims5$TreatedAtDose),
    99, TwoArmsims11$TreatedAtDose, sum(TwoArmsims11$TreatedAtDose)),
  
  c(s6tox, 0, s12tox, 0),
  c(s6eff, 0, s12eff, 0),
  c(n45p20sims6$TreatedAtDose, sum(n45p20sims6$TreatedAtDose),
    n45p20sims12$TreatedAtDose, sum(n45p20sims12$TreatedAtDose)),
  c(WTsims6$TreatedAtDose, sum(WTsims6$TreatedAtDose),
    WTsims12$TreatedAtDose, sum(WTsims12$TreatedAtDose)),
  c(99, TwoArmsims6$TreatedAtDose, sum(TwoArmsims6$TreatedAtDose),
    99, TwoArmsims12$TreatedAtDose, sum(TwoArmsims12$TreatedAtDose))
  
)

design <- rep(c(99, 99, 'RtC-WT', 'WT', 'TwoArm'), times = 6) 

scenarioL <- c('P(tox)', 'P(eff)', rep(1, times = 3),
               'P(tox)', 'P(eff)', rep(2, times = 3),
               'P(tox)', 'P(eff)', rep(3, times = 3),
               'P(tox)', 'P(eff)', rep(4, times = 3),
               'P(tox)', 'P(eff)', rep(5, times = 3),
               'P(tox)', 'P(eff)', rep(6, times = 3))

scenarioR <- c('P(tox)', 'P(eff)', rep(7, times = 3),
               'P(tox)', 'P(eff)', rep(8, times = 3),
               'P(tox)', 'P(eff)', rep(9, times = 3),
               'P(tox)', 'P(eff)', rep(10, times = 3),
               'P(tox)', 'P(eff)', rep(11, times = 3),
               'P(tox)', 'P(eff)', rep(12, times = 3))

data <- data.frame(cbind(scenarioL, design, data))
data <- add_column(data, design = design, .after = 8 )
data <- add_column(data, scenarioR = scenarioR, .after = 8 )

data <- data %>% 
  mutate(V8 = if_else(scenarioL == 'P(eff)', ' ', V8),
         V8 = if_else(scenarioL == 'P(tox)', ' ', V8),
         V14 = if_else(scenarioL == 'P(eff)', ' ', V14),
         V14 = if_else(scenarioL == 'P(tox)', ' ', V14)
  )
data[data == 99] <- c(' ')

colnames(data) <- c('Scenario', 'Design', 'Dose 1', 'Dose 2', 'Dose 3', 'Dose 4', 'Dose 5', 'Total',
                    'Scenario', 'Design', 'Dose 1', 'Dose 2', 'Dose 3', 'Dose 4', 'Dose 5', 'Total')
#View(data)

datacode <- kable(data, "latex", booktabs = T, linesep = "", align = "c", 
                  caption = '\\label{tab_wt:MeanNAlt}Mean number of patients allocated to each dose-level for alternative trial designs.')%>%
  column_spec(8, border_right = T) %>% 
  kable_styling(latex_options = c('scale_down'), position = 'center',
                font_size = 12 ) %>%
  collapse_rows(columns = c(1,9), latex_hline = "major",  valign = "middle")

cat(datacode)

rm(list = ls(all.names = TRUE))