library(dplyr)
library(tibble)
library(kableExtra)

# Combination of parameters


# Table for operating characteristics for different combos of AR size and 
# randomisation to control probabilities looking at best/good doses 
# number of patients at those doses and number of controls
load("RProbComboSimsData.RData")

data <- 
  rbind(
    # Scenario 1
        # placebo.rand.prob = 0.2
    c(1, 0.2, 0, 5, '3-5', n0p20sims1$ProbSelect[5], # n.ar = 0
      sum(n0p20sims1$ProbSelect[3:5]), n0p20sims1$TreatedAtDose[5],
      sum(n0p20sims1$TreatedAtDose[3:5]), n0p20sims1$TreatedAtDose[1]),
    c(1, 0.2, 15, 5, '3-5', n15p20sims1$ProbSelect[5], #n.ar = 15
      sum(n15p20sims1$ProbSelect[3:5]), n15p20sims1$TreatedAtDose[5],
      sum(n15p20sims1$TreatedAtDose[3:5]), n15p20sims1$TreatedAtDose[1]),
    c(1, 0.2, 30, 5, '3-5', n30p20sims1$ProbSelect[5], #n.ar = 30
      sum(n30p20sims1$ProbSelect[3:5]), n30p20sims1$TreatedAtDose[5],
      sum(n30p20sims1$TreatedAtDose[3:5]), n30p20sims1$TreatedAtDose[1]),
    c(1, 0.2, 45, 5, '3-5', n45p20sims1$ProbSelect[5], #n.ar = 45
      sum(n45p20sims1$ProbSelect[3:5]), n45p20sims1$TreatedAtDose[5],
      sum(n45p20sims1$TreatedAtDose[3:5]), n45p20sims1$TreatedAtDose[1]),
    c(1, 0.2, 60, 5, '3-5', n60p20sims1$ProbSelect[5], #n.ar = 60
      sum(n60p20sims1$ProbSelect[3:5]), n60p20sims1$TreatedAtDose[5],
      sum(n60p20sims1$TreatedAtDose[3:5]), n60p20sims1$TreatedAtDose[1]),
        # placebo.rand.prob = 0.33
    c(1, 0.33, 0, 5, '3-5', n0p33sims1$ProbSelect[5], # n.ar = 0
      sum(n0p33sims1$ProbSelect[3:5]), n0p33sims1$TreatedAtDose[5],
      sum(n0p33sims1$TreatedAtDose[3:5]), n0p33sims1$TreatedAtDose[1]),
    c(1, 0.33, 15, 5, '3-5', n15p33sims1$ProbSelect[5], #n.ar = 15
      sum(n15p33sims1$ProbSelect[3:5]), n15p33sims1$TreatedAtDose[5],
      sum(n15p33sims1$TreatedAtDose[3:5]), n15p33sims1$TreatedAtDose[1]),
    c(1, 0.33, 30, 5, '3-5', n30p33sims1$ProbSelect[5], #n.ar = 30
      sum(n30p33sims1$ProbSelect[3:5]), n30p33sims1$TreatedAtDose[5],
      sum(n30p33sims1$TreatedAtDose[3:5]), n30p33sims1$TreatedAtDose[1]),
    c(1, 0.33, 45, 5, '3-5', n45p33sims1$ProbSelect[5], #n.ar = 45
      sum(n45p33sims1$ProbSelect[3:5]), n45p33sims1$TreatedAtDose[5],
      sum(n45p33sims1$TreatedAtDose[3:5]), n45p33sims1$TreatedAtDose[1]),
    c(1, 0.33, 60, 5, '3-5', n60p33sims1$ProbSelect[5], #n.ar = 60
      sum(n60p33sims1$ProbSelect[3:5]), n60p33sims1$TreatedAtDose[5],
      sum(n60p33sims1$TreatedAtDose[3:5]), n60p33sims1$TreatedAtDose[1]),
    
    # Scenario 2 
    # placebo.rand.prob = 0.2
    c(2, 0.2, 0, 'stop', 'stop/1', n0p20sims2$ProbStop, # n.ar = 0
      sum(n0p20sims2$ProbSelect[1] + n0p20sims2$ProbStop), '-',
      sum(n0p20sims2$TreatedAtDose[1]), sum(n0p20sims2$TreatedAtDose[1])),
    c(2, 0.2, 15, 'stop', 'stop/1', n15p20sims2$ProbStop, #n.ar = 15
      sum(n15p20sims2$ProbSelect[1] + n15p20sims2$ProbStop), '-',
      sum(n15p20sims2$TreatedAtDose[1]), sum(n15p20sims2$TreatedAtDose[1])),
    c(2, 0.2, 30, 'stop', 'stop/1', n30p20sims2$ProbStop, #n.ar = 30
      sum(n30p20sims2$ProbSelect[1] + n30p20sims2$ProbStop), '-',
      sum(n30p20sims2$TreatedAtDose[1]), sum(n30p20sims2$TreatedAtDose[1])),
    c(2, 0.2, 45, 'stop', 'stop/1', n45p20sims2$ProbStop, #n.ar = 45
      sum(n45p20sims2$ProbSelect[1] + n45p20sims2$ProbStop), '-',
      sum(n45p20sims2$TreatedAtDose[1]), sum(n45p20sims2$TreatedAtDose[1])),
    c(2, 0.2, 60, 'stop', 'stop/1', n60p20sims2$ProbStop, #n.ar = 60
      sum(n60p20sims2$ProbSelect[1] + n60p20sims2$ProbStop), '-',
      sum(n60p20sims2$TreatedAtDose[1]), sum(n60p20sims2$TreatedAtDose[1])),
    # placebo.rand.prob = 0.33
    c(2, 0.33, 0, 'stop', 'stop/1', n0p33sims2$ProbStop, # n.ar = 0
      sum(n0p33sims2$ProbSelect[1] + n0p33sims2$ProbStop), '-',
      sum(n0p33sims2$TreatedAtDose[1]), sum(n0p33sims2$TreatedAtDose[1])),
    c(2, 0.33, 15, 'stop', 'stop/1', n15p33sims2$ProbStop, #n.ar = 15
      sum(n15p33sims2$ProbSelect[1] + n15p33sims2$ProbStop), '-',
      sum(n15p33sims2$TreatedAtDose[1]), sum(n15p33sims2$TreatedAtDose[1])),
    c(2, 0.33, 30, 'stop', 'stop/1', n30p33sims2$ProbStop, #n.ar = 30
      sum(n30p33sims2$ProbSelect[1] + n30p33sims2$ProbStop), '-',
      sum(n30p33sims2$TreatedAtDose[1]), sum(n30p33sims2$TreatedAtDose[1])),
    c(2, 0.33, 45, 'stop', 'stop/1', n45p33sims2$ProbStop, #n.ar = 45
      sum(n45p33sims2$ProbSelect[1] + n45p33sims2$ProbStop), '-',
      sum(n45p33sims2$TreatedAtDose[1]), sum(n45p33sims2$TreatedAtDose[1])),
    c(2, 0.33, 60, 'stop', 'stop/1', n60p33sims2$ProbStop, #n.ar = 60
      sum(n60p33sims2$ProbSelect[1] + n60p33sims2$ProbStop), '-',
      sum(n60p33sims2$TreatedAtDose[1]), sum(n60p33sims2$TreatedAtDose[1])), 
    
    # Scenario 3
    # placebo.rand.prob = 0.2
    c(3, 0.2, 0, 3, '3', n0p20sims3$ProbSelect[3], # n.ar = 0
      sum(n0p20sims3$ProbSelect[3]), n0p20sims3$TreatedAtDose[3],
      sum(n0p20sims3$TreatedAtDose[3]), n0p20sims3$TreatedAtDose[1]),
    c(3, 0.2, 15, 3, '3', n15p20sims3$ProbSelect[3], #n.ar = 15
      sum(n15p20sims3$ProbSelect[3]), n15p20sims3$TreatedAtDose[3],
      sum(n15p20sims3$TreatedAtDose[3]), n15p20sims3$TreatedAtDose[1]),
    c(3, 0.2, 30, 3, '3', n30p20sims3$ProbSelect[3], #n.ar = 30
      sum(n30p20sims3$ProbSelect[3]), n30p20sims3$TreatedAtDose[3],
      sum(n30p20sims3$TreatedAtDose[3]), n30p20sims3$TreatedAtDose[1]),
    c(3, 0.2, 45, 3, '3', n45p20sims3$ProbSelect[3], #n.ar = 45
      sum(n45p20sims3$ProbSelect[3]), n45p20sims3$TreatedAtDose[3],
      sum(n45p20sims3$TreatedAtDose[3]), n45p20sims3$TreatedAtDose[1]),
    c(3, 0.2, 60, 3, '3', n60p20sims3$ProbSelect[3], #n.ar = 60
      sum(n60p20sims3$ProbSelect[3]), n60p20sims3$TreatedAtDose[3],
      sum(n60p20sims3$TreatedAtDose[3]), n60p20sims3$TreatedAtDose[1]),
    # placebo.rand.prob = 0.33
    c(3, 0.33, 0, 3, '3', n0p33sims3$ProbSelect[3], # n.ar = 0
      sum(n0p33sims3$ProbSelect[3]), n0p33sims3$TreatedAtDose[3],
      sum(n0p33sims3$TreatedAtDose[3]), n0p33sims3$TreatedAtDose[1]),
    c(3, 0.33, 15, 3, '3', n15p33sims3$ProbSelect[3], #n.ar = 15
      sum(n15p33sims3$ProbSelect[3]), n15p33sims3$TreatedAtDose[3],
      sum(n15p33sims3$TreatedAtDose[3]), n15p33sims3$TreatedAtDose[1]),
    c(3, 0.33, 30, 3, '3', n30p33sims3$ProbSelect[3], #n.ar = 30
      sum(n30p33sims3$ProbSelect[3]), n30p33sims3$TreatedAtDose[3],
      sum(n30p33sims3$TreatedAtDose[3]), n30p33sims3$TreatedAtDose[1]),
    c(3, 0.33, 45, 3, '3', n45p33sims3$ProbSelect[3], #n.ar = 45
      sum(n45p33sims3$ProbSelect[3]), n45p33sims3$TreatedAtDose[3],
      sum(n45p33sims3$TreatedAtDose[3]), n45p33sims3$TreatedAtDose[1]),
    c(3, 0.33, 60, 3, '3', n60p33sims3$ProbSelect[3], #n.ar = 60
      sum(n60p33sims3$ProbSelect[3]), n60p33sims3$TreatedAtDose[3],
      sum(n60p33sims3$TreatedAtDose[3]), n60p33sims3$TreatedAtDose[1]), 
    
    # Scenario 4
    # placebo.rand.prob = 0.2
    c(4, 0.2, 0, 3, '3-4', n0p20sims4$ProbSelect[3], # n.ar = 0
      sum(n0p20sims4$ProbSelect[3:4]), n0p20sims4$TreatedAtDose[3],
      sum(n0p20sims4$TreatedAtDose[3:4]), n0p20sims4$TreatedAtDose[1]),
    c(4, 0.2, 15, 3, '3-4', n15p20sims4$ProbSelect[3], #n.ar = 15
      sum(n15p20sims4$ProbSelect[3:4]), n15p20sims4$TreatedAtDose[3],
      sum(n15p20sims4$TreatedAtDose[3:4]), n15p20sims4$TreatedAtDose[1]),
    c(4, 0.2, 30, 3, '3-4', n30p20sims4$ProbSelect[3], #n.ar = 30
      sum(n30p20sims4$ProbSelect[3:4]), n30p20sims4$TreatedAtDose[3],
      sum(n30p20sims4$TreatedAtDose[3:4]), n30p20sims4$TreatedAtDose[1]),
    c(4, 0.2, 45, 3, '3-4', n45p20sims4$ProbSelect[3], #n.ar = 45
      sum(n45p20sims4$ProbSelect[3:4]), n45p20sims4$TreatedAtDose[3],
      sum(n45p20sims4$TreatedAtDose[3:4]), n45p20sims4$TreatedAtDose[1]),
    c(4, 0.2, 60, 3, '3-4', n60p20sims4$ProbSelect[3], #n.ar = 60
      sum(n60p20sims4$ProbSelect[3:4]), n60p20sims4$TreatedAtDose[3],
      sum(n60p20sims4$TreatedAtDose[3:4]), n60p20sims4$TreatedAtDose[1]),
    # placebo.rand.prob = 0.33
    c(4, 0.33, 0, 3, '3-4', n0p33sims4$ProbSelect[3], # n.ar = 0
      sum(n0p33sims4$ProbSelect[3:4]), n0p33sims4$TreatedAtDose[3],
      sum(n0p33sims4$TreatedAtDose[3:4]), n0p33sims4$TreatedAtDose[1]),
    c(4, 0.33, 15, 3, '3-4', n15p33sims4$ProbSelect[3], #n.ar = 15
      sum(n15p33sims4$ProbSelect[3:4]), n15p33sims4$TreatedAtDose[3],
      sum(n15p33sims4$TreatedAtDose[3:4]), n15p33sims4$TreatedAtDose[1]),
    c(4, 0.33, 30, 3, '3-4', n30p33sims4$ProbSelect[3], #n.ar = 30
      sum(n30p33sims4$ProbSelect[3:4]), n30p33sims4$TreatedAtDose[3],
      sum(n30p33sims4$TreatedAtDose[3:4]), n30p33sims4$TreatedAtDose[1]),
    c(4, 0.33, 45, 3, '3-4', n45p33sims4$ProbSelect[3], #n.ar = 45
      sum(n45p33sims4$ProbSelect[3:4]), n45p33sims4$TreatedAtDose[3],
      sum(n45p33sims4$TreatedAtDose[3:4]), n45p33sims4$TreatedAtDose[1]),
    c(4, 0.33, 60, 3, '3-4', n60p33sims4$ProbSelect[3], #n.ar = 60
      sum(n60p33sims4$ProbSelect[3:4]), n60p33sims4$TreatedAtDose[3],
      sum(n60p33sims4$TreatedAtDose[3:4]), n60p33sims4$TreatedAtDose[1]), 
    
    # Scenario 5
    # placebo.rand.prob = 0.2
    c(5, 0.2, 0, 'stop', 'stop/1', n0p20sims5$ProbStop, # n.ar = 0
      sum(n0p20sims5$ProbSelect[1] + n0p20sims5$ProbStop), '-',
      sum(n0p20sims5$TreatedAtDose[1]), sum(n0p20sims5$TreatedAtDose[1])),
    c(5, 0.2, 15, 'stop', 'stop/1', n15p20sims5$ProbStop, #n.ar = 15
      sum(n15p20sims5$ProbSelect[1] + n15p20sims5$ProbStop), '-',
      sum(n15p20sims5$TreatedAtDose[1]), sum(n15p20sims5$TreatedAtDose[1])),
    c(5, 0.2, 30, 'stop', 'stop/1', n30p20sims5$ProbStop, #n.ar = 30
      sum(n30p20sims5$ProbSelect[1] + n30p20sims5$ProbStop), '-',
      sum(n30p20sims5$TreatedAtDose[1]), sum(n30p20sims5$TreatedAtDose[1])),
    c(5, 0.2, 45, 'stop', 'stop/1', n45p20sims5$ProbStop, #n.ar = 45
      sum(n45p20sims5$ProbSelect[1] + n45p20sims5$ProbStop), '-',
      sum(n45p20sims5$TreatedAtDose[1]), sum(n45p20sims5$TreatedAtDose[1])),
    c(5, 0.2, 60, 'stop', 'stop/1', n60p20sims5$ProbStop, #n.ar = 60
      sum(n60p20sims5$ProbSelect[1] + n60p20sims5$ProbStop), '-',
      sum(n60p20sims5$TreatedAtDose[1]), sum(n60p20sims5$TreatedAtDose[1])),
    # placebo.rand.prob = 0.33
    c(5, 0.33, 0, 'stop', 'stop/1', n0p33sims5$ProbStop, # n.ar = 0
      sum(n0p33sims5$ProbSelect[1] + n0p33sims5$ProbStop), '-',
      sum(n0p33sims5$TreatedAtDose[1]), sum(n0p33sims5$TreatedAtDose[1])),
    c(5, 0.33, 15, 'stop', 'stop/1', n15p33sims5$ProbStop, #n.ar = 15
      sum(n15p33sims5$ProbSelect[1] + n15p33sims5$ProbStop), '-',
      sum(n15p33sims5$TreatedAtDose[1]), sum(n15p33sims5$TreatedAtDose[1])),
    c(5, 0.33, 30, 'stop', 'stop/1', n30p33sims5$ProbStop, #n.ar = 30
      sum(n30p33sims5$ProbSelect[1] + n30p33sims5$ProbStop), '-',
      sum(n30p33sims5$TreatedAtDose[1]), sum(n30p33sims5$TreatedAtDose[1])),
    c(5, 0.33, 45, 'stop', 'stop/1', n45p33sims5$ProbStop, #n.ar = 45
      sum(n45p33sims5$ProbSelect[1] + n45p33sims5$ProbStop), '-',
      sum(n45p33sims5$TreatedAtDose[1]), sum(n45p33sims5$TreatedAtDose[1])),
    c(5, 0.33, 60, 'stop', 'stop/1', n60p33sims5$ProbStop, #n.ar = 60
      sum(n60p33sims5$ProbSelect[1] + n60p33sims5$ProbStop), '-',
      sum(n60p33sims5$TreatedAtDose[1]), sum(n60p33sims5$TreatedAtDose[1])), 
    
    # Scenario 6
    # placebo.rand.prob = 0.2
    c(6, 0.2, 0, 3, '3', n0p20sims6$ProbSelect[3], # n.ar = 0
      sum(n0p20sims6$ProbSelect[3]), n0p20sims6$TreatedAtDose[3],
      sum(n0p20sims6$TreatedAtDose[3]), n0p20sims6$TreatedAtDose[1]),
    c(6, 0.2, 15, 3, '3', n15p20sims6$ProbSelect[3], #n.ar = 15
      sum(n15p20sims6$ProbSelect[3]), n15p20sims6$TreatedAtDose[3],
      sum(n15p20sims6$TreatedAtDose[3]), n15p20sims6$TreatedAtDose[1]),
    c(6, 0.2, 30, 3, '3', n30p20sims6$ProbSelect[3], #n.ar = 30
      sum(n30p20sims6$ProbSelect[3]), n30p20sims6$TreatedAtDose[3],
      sum(n30p20sims6$TreatedAtDose[3]), n30p20sims6$TreatedAtDose[1]),
    c(6, 0.2, 45, 3, '3', n45p20sims6$ProbSelect[3], #n.ar = 45
      sum(n45p20sims6$ProbSelect[3]), n45p20sims6$TreatedAtDose[3],
      sum(n45p20sims6$TreatedAtDose[3]), n45p20sims6$TreatedAtDose[1]),
    c(6, 0.2, 60, 3, '3', n60p20sims6$ProbSelect[3], #n.ar = 60
      sum(n60p20sims6$ProbSelect[3]), n60p20sims6$TreatedAtDose[3],
      sum(n60p20sims6$TreatedAtDose[3]), n60p20sims6$TreatedAtDose[1]),
    # placebo.rand.prob = 0.33
    c(6, 0.33, 0, 3, '3', n0p33sims6$ProbSelect[3], # n.ar = 0
      sum(n0p33sims6$ProbSelect[3]), n0p33sims6$TreatedAtDose[3],
      sum(n0p33sims6$TreatedAtDose[3]), n0p33sims6$TreatedAtDose[1]),
    c(6, 0.33, 15, 3, '3', n15p33sims6$ProbSelect[3], #n.ar = 15
      sum(n15p33sims6$ProbSelect[3]), n15p33sims6$TreatedAtDose[3],
      sum(n15p33sims6$TreatedAtDose[3]), n15p33sims6$TreatedAtDose[1]),
    c(6, 0.33, 30, 3, '3', n30p33sims6$ProbSelect[3], #n.ar = 30
      sum(n30p33sims6$ProbSelect[3]), n30p33sims6$TreatedAtDose[3],
      sum(n30p33sims6$TreatedAtDose[3]), n30p33sims6$TreatedAtDose[1]),
    c(6, 0.33, 45, 3, '3', n45p33sims6$ProbSelect[3], #n.ar = 45
      sum(n45p33sims6$ProbSelect[3]), n45p33sims6$TreatedAtDose[3],
      sum(n45p33sims6$TreatedAtDose[3]), n45p33sims6$TreatedAtDose[1]),
    c(6, 0.33, 60, 3, '3', n60p33sims6$ProbSelect[3], #n.ar = 60
      sum(n60p33sims6$ProbSelect[3]), n60p33sims6$TreatedAtDose[3],
      sum(n60p33sims6$TreatedAtDose[3]), n60p33sims6$TreatedAtDose[1]),
    
    # Scenario 7
    # placebo.rand.prob = 0.2
    c(7, 0.2, 0, 3, '3-5', n0p20sims7$ProbSelect[3], # n.ar = 0
      sum(n0p20sims7$ProbSelect[3:5]), n0p20sims7$TreatedAtDose[3],
      sum(n0p20sims7$TreatedAtDose[3:5]), n0p20sims7$TreatedAtDose[1]),
    c(7, 0.2, 15, 3, '3-5', n15p20sims7$ProbSelect[3], #n.ar = 15
      sum(n15p20sims7$ProbSelect[3:5]), n15p20sims7$TreatedAtDose[3],
      sum(n15p20sims7$TreatedAtDose[3:5]), n15p20sims7$TreatedAtDose[1]),
    c(7, 0.2, 30, 3, '3-5', n30p20sims7$ProbSelect[3], #n.ar = 30
      sum(n30p20sims7$ProbSelect[3:5]), n30p20sims7$TreatedAtDose[3],
      sum(n30p20sims7$TreatedAtDose[3:5]), n30p20sims7$TreatedAtDose[1]),
    c(7, 0.2, 45, 3, '3-5', n45p20sims7$ProbSelect[3], #n.ar = 45
      sum(n45p20sims7$ProbSelect[3:5]), n45p20sims7$TreatedAtDose[3],
      sum(n45p20sims7$TreatedAtDose[3:5]), n45p20sims7$TreatedAtDose[1]),
    c(7, 0.2, 60, 3, '3-5', n60p20sims7$ProbSelect[3], #n.ar = 60
      sum(n60p20sims7$ProbSelect[3:5]), n60p20sims7$TreatedAtDose[3],
      sum(n60p20sims7$TreatedAtDose[3:5]), n60p20sims7$TreatedAtDose[1]),
    # placebo.rand.prob = 0.33
    c(7, 0.33, 0, 3, '3-5', n0p33sims7$ProbSelect[3], # n.ar = 0
      sum(n0p33sims7$ProbSelect[3:5]), n0p33sims7$TreatedAtDose[3],
      sum(n0p33sims7$TreatedAtDose[3:5]), n0p33sims7$TreatedAtDose[1]),
    c(7, 0.33, 15, 3, '3-5', n15p33sims7$ProbSelect[3], #n.ar = 15
      sum(n15p33sims7$ProbSelect[3:5]), n15p33sims7$TreatedAtDose[3],
      sum(n15p33sims7$TreatedAtDose[3:5]), n15p33sims7$TreatedAtDose[1]),
    c(7, 0.33, 30, 3, '3-5', n30p33sims7$ProbSelect[3], #n.ar = 30
      sum(n30p33sims7$ProbSelect[3:5]), n30p33sims7$TreatedAtDose[3],
      sum(n30p33sims7$TreatedAtDose[3:5]), n30p33sims7$TreatedAtDose[1]),
    c(7, 0.33, 45, 3, '3-5', n45p33sims7$ProbSelect[3], #n.ar = 45
      sum(n45p33sims7$ProbSelect[3:5]), n45p33sims7$TreatedAtDose[3],
      sum(n45p33sims7$TreatedAtDose[3:5]), n45p33sims7$TreatedAtDose[1]),
    c(7, 0.33, 60, 3, '3-5', n60p33sims7$ProbSelect[3], #n.ar = 60
      sum(n60p33sims7$ProbSelect[3:5]), n60p33sims7$TreatedAtDose[3],
      sum(n60p33sims7$TreatedAtDose[3:5]), n60p33sims7$TreatedAtDose[1]), 
    
    # Scenario 8
    # placebo.rand.prob = 0.2
    c(8, 0.2, 0, 'stop', 'stop/1', n0p20sims8$ProbStop, # n.ar = 0
      sum(n0p20sims8$ProbSelect[1] + n0p20sims8$ProbStop), '-',
      sum(n0p20sims8$TreatedAtDose[1]), sum(n0p20sims8$TreatedAtDose[1])),
    c(8, 0.2, 15, 'stop', 'stop/1', n15p20sims8$ProbStop, #n.ar = 15
      sum(n15p20sims8$ProbSelect[1] + n15p20sims8$ProbStop), '-',
      sum(n15p20sims8$TreatedAtDose[1]), sum(n15p20sims8$TreatedAtDose[1])),
    c(8, 0.2, 30, 'stop', 'stop/1', n30p20sims8$ProbStop, #n.ar = 30
      sum(n30p20sims8$ProbSelect[1] + n30p20sims8$ProbStop), '-',
      sum(n30p20sims8$TreatedAtDose[1]), sum(n30p20sims8$TreatedAtDose[1])),
    c(8, 0.2, 45, 'stop', 'stop/1', n45p20sims8$ProbStop, #n.ar = 45
      sum(n45p20sims8$ProbSelect[1] + n45p20sims8$ProbStop), '-',
      sum(n45p20sims8$TreatedAtDose[1]), sum(n45p20sims8$TreatedAtDose[1])),
    c(8, 0.2, 60, 'stop', 'stop/1', n60p20sims8$ProbStop, #n.ar = 60
      sum(n60p20sims8$ProbSelect[1] + n60p20sims8$ProbStop), '-',
      sum(n60p20sims8$TreatedAtDose[1]), sum(n60p20sims8$TreatedAtDose[1])),
    # placebo.rand.prob = 0.33
    c(8, 0.33, 0, 'stop', 'stop/1', n0p33sims8$ProbStop, # n.ar = 0
      sum(n0p33sims8$ProbSelect[1] + n0p33sims8$ProbStop), '-',
      sum(n0p33sims8$TreatedAtDose[1]), sum(n0p33sims8$TreatedAtDose[1])),
    c(8, 0.33, 15, 'stop', 'stop/1', n15p33sims8$ProbStop, #n.ar = 15
      sum(n15p33sims8$ProbSelect[1] + n15p33sims8$ProbStop), '-',
      sum(n15p33sims8$TreatedAtDose[1]), sum(n15p33sims8$TreatedAtDose[1])),
    c(8, 0.33, 30, 'stop', 'stop/1', n30p33sims8$ProbStop, #n.ar = 30
      sum(n30p33sims8$ProbSelect[1] + n30p33sims8$ProbStop), '-',
      sum(n30p33sims8$TreatedAtDose[1]), sum(n30p33sims8$TreatedAtDose[1])),
    c(8, 0.33, 45, 'stop', 'stop/1', n45p33sims8$ProbStop, #n.ar = 45
      sum(n45p33sims8$ProbSelect[1] + n45p33sims8$ProbStop), '-',
      sum(n45p33sims8$TreatedAtDose[1]), sum(n45p33sims8$TreatedAtDose[1])),
    c(8, 0.33, 60, 'stop', 'stop/1', n60p33sims8$ProbStop, #n.ar = 60
      sum(n60p33sims8$ProbSelect[1] + n60p33sims8$ProbStop), '-',
      sum(n60p33sims8$TreatedAtDose[1]), sum(n60p33sims8$TreatedAtDose[1])), 
    
    # Scenario 9
    # placebo.rand.prob = 0.2
    c(9, 0.2, 0, 3, '3', n0p20sims9$ProbSelect[3], # n.ar = 0
      sum(n0p20sims9$ProbSelect[3]), n0p20sims9$TreatedAtDose[3],
      sum(n0p20sims9$TreatedAtDose[3]), n0p20sims9$TreatedAtDose[1]),
    c(9, 0.2, 15, 3, '3', n15p20sims9$ProbSelect[3], #n.ar = 15
      sum(n15p20sims9$ProbSelect[3]), n15p20sims9$TreatedAtDose[3],
      sum(n15p20sims9$TreatedAtDose[3]), n15p20sims9$TreatedAtDose[1]),
    c(9, 0.2, 30, 3, '3', n30p20sims9$ProbSelect[3], #n.ar = 30
      sum(n30p20sims9$ProbSelect[3]), n30p20sims9$TreatedAtDose[3],
      sum(n30p20sims9$TreatedAtDose[3]), n30p20sims9$TreatedAtDose[1]),
    c(9, 0.2, 45, 3, '3', n45p20sims9$ProbSelect[3], #n.ar = 45
      sum(n45p20sims9$ProbSelect[3]), n45p20sims9$TreatedAtDose[3],
      sum(n45p20sims9$TreatedAtDose[3]), n45p20sims9$TreatedAtDose[1]),
    c(9, 0.2, 60, 3, '3', n60p20sims9$ProbSelect[3], #n.ar = 60
      sum(n60p20sims9$ProbSelect[3]), n60p20sims9$TreatedAtDose[3],
      sum(n60p20sims9$TreatedAtDose[3]), n60p20sims9$TreatedAtDose[1]),
    # placebo.rand.prob = 0.33
    c(9, 0.33, 0, 3, '3', n0p33sims9$ProbSelect[3], # n.ar = 0
      sum(n0p33sims9$ProbSelect[3]), n0p33sims9$TreatedAtDose[3],
      sum(n0p33sims9$TreatedAtDose[3]), n0p33sims9$TreatedAtDose[1]),
    c(9, 0.33, 15, 3, '3', n15p33sims9$ProbSelect[3], #n.ar = 15
      sum(n15p33sims9$ProbSelect[3]), n15p33sims9$TreatedAtDose[3],
      sum(n15p33sims9$TreatedAtDose[3]), n15p33sims9$TreatedAtDose[1]),
    c(9, 0.33, 30, 3, '3', n30p33sims9$ProbSelect[3], #n.ar = 30
      sum(n30p33sims9$ProbSelect[3]), n30p33sims9$TreatedAtDose[3],
      sum(n30p33sims9$TreatedAtDose[3]), n30p33sims9$TreatedAtDose[1]),
    c(9, 0.33, 45, 3, '3', n45p33sims9$ProbSelect[3], #n.ar = 45
      sum(n45p33sims9$ProbSelect[3]), n45p33sims9$TreatedAtDose[3],
      sum(n45p33sims9$TreatedAtDose[3]), n45p33sims9$TreatedAtDose[1]),
    c(9, 0.33, 60, 3, '3', n60p33sims9$ProbSelect[3], #n.ar = 60
      sum(n60p33sims9$ProbSelect[3]), n60p33sims9$TreatedAtDose[3],
      sum(n60p33sims9$TreatedAtDose[3]), n60p33sims9$TreatedAtDose[1]), 
    
    # Scenario 10
    # placebo.rand.prob = 0.2
    c(10, 0.2, 0, 2, '2-4', n0p20sims10$ProbSelect[2], # n.ar = 0
      sum(n0p20sims10$ProbSelect[2:4]), n0p20sims10$TreatedAtDose[2],
      sum(n0p20sims10$TreatedAtDose[2:4]), n0p20sims10$TreatedAtDose[1]),
    c(10, 0.2, 15, 2, '2-4', n15p20sims10$ProbSelect[2], #n.ar = 15
      sum(n15p20sims10$ProbSelect[2:4]), n15p20sims10$TreatedAtDose[2],
      sum(n15p20sims10$TreatedAtDose[2:4]), n15p20sims10$TreatedAtDose[1]),
    c(10, 0.2, 30, 2, '2-4', n30p20sims10$ProbSelect[2], #n.ar = 30
      sum(n30p20sims10$ProbSelect[2:4]), n30p20sims10$TreatedAtDose[2],
      sum(n30p20sims10$TreatedAtDose[2:4]), n30p20sims10$TreatedAtDose[1]),
    c(10, 0.2, 45, 2, '2-4', n45p20sims10$ProbSelect[2], #n.ar = 45
      sum(n45p20sims10$ProbSelect[2:4]), n45p20sims10$TreatedAtDose[2],
      sum(n45p20sims10$TreatedAtDose[2:4]), n45p20sims10$TreatedAtDose[1]),
    c(10, 0.2, 60, 2, '2-4', n60p20sims10$ProbSelect[2], #n.ar = 60
      sum(n60p20sims10$ProbSelect[2:4]), n60p20sims10$TreatedAtDose[2],
      sum(n60p20sims10$TreatedAtDose[2:4]), n60p20sims10$TreatedAtDose[1]),
    # placebo.rand.prob = 0.33
    c(10, 0.33, 0, 2, '2-4', n0p33sims10$ProbSelect[2], # n.ar = 0
      sum(n0p33sims10$ProbSelect[2:4]), n0p33sims10$TreatedAtDose[2],
      sum(n0p33sims10$TreatedAtDose[2:4]), n0p33sims10$TreatedAtDose[1]),
    c(10, 0.33, 15, 2, '2-4', n15p33sims10$ProbSelect[2], #n.ar = 15
      sum(n15p33sims10$ProbSelect[2:4]), n15p33sims10$TreatedAtDose[2],
      sum(n15p33sims10$TreatedAtDose[2:4]), n15p33sims10$TreatedAtDose[1]),
    c(10, 0.33, 30, 2, '2-4', n30p33sims10$ProbSelect[2], #n.ar = 30
      sum(n30p33sims10$ProbSelect[2:4]), n30p33sims10$TreatedAtDose[2],
      sum(n30p33sims10$TreatedAtDose[2:4]), n30p33sims10$TreatedAtDose[1]),
    c(10, 0.33, 45, 2, '2-4', n45p33sims10$ProbSelect[2], #n.ar = 45
      sum(n45p33sims10$ProbSelect[2:4]), n45p33sims10$TreatedAtDose[2],
      sum(n45p33sims10$TreatedAtDose[2:4]), n45p33sims10$TreatedAtDose[1]),
    c(10, 0.33, 60, 2, '2-4', n60p33sims10$ProbSelect[2], #n.ar = 60
      sum(n60p33sims10$ProbSelect[2:4]), n60p33sims10$TreatedAtDose[2],
      sum(n60p33sims10$TreatedAtDose[2:4]), n60p33sims10$TreatedAtDose[1]), 
    
    # Scenario 11
    # placebo.rand.prob = 0.2
    c(11, 0.2, 0, 'stop', 'stop/1', n0p20sims11$ProbStop, # n.ar = 0
      sum(n0p20sims11$ProbSelect[1] + n0p20sims11$ProbStop), '-',
      sum(n0p20sims11$TreatedAtDose[1]), sum(n0p20sims11$TreatedAtDose[1])),
    c(11, 0.2, 15, 'stop', 'stop/1', n15p20sims11$ProbStop, #n.ar = 15
      sum(n15p20sims11$ProbSelect[1] + n15p20sims11$ProbStop), '-',
      sum(n15p20sims11$TreatedAtDose[1]), sum(n15p20sims11$TreatedAtDose[1])),
    c(11, 0.2, 30, 'stop', 'stop/1', n30p20sims11$ProbStop, #n.ar = 30
      sum(n30p20sims11$ProbSelect[1] + n30p20sims11$ProbStop), '-',
      sum(n30p20sims11$TreatedAtDose[1]), sum(n30p20sims11$TreatedAtDose[1])),
    c(11, 0.2, 45, 'stop', 'stop/1', n45p20sims11$ProbStop, #n.ar = 45
      sum(n45p20sims11$ProbSelect[1] + n45p20sims11$ProbStop), '-',
      sum(n45p20sims11$TreatedAtDose[1]), sum(n45p20sims11$TreatedAtDose[1])),
    c(11, 0.2, 60, 'stop', 'stop/1', n60p20sims11$ProbStop, #n.ar = 60
      sum(n60p20sims11$ProbSelect[1] + n60p20sims11$ProbStop), '-',
      sum(n60p20sims11$TreatedAtDose[1]), sum(n60p20sims11$TreatedAtDose[1])),
    # placebo.rand.prob = 0.33
    c(11, 0.33, 0, 'stop', 'stop/1', n0p33sims11$ProbStop, # n.ar = 0
      sum(n0p33sims11$ProbSelect[1] + n0p33sims11$ProbStop), '-',
      sum(n0p33sims11$TreatedAtDose[1]), sum(n0p33sims11$TreatedAtDose[1])),
    c(11, 0.33, 15, 'stop', 'stop/1', n15p33sims11$ProbStop, #n.ar = 15
      sum(n15p33sims11$ProbSelect[1] + n15p33sims11$ProbStop), '-',
      sum(n15p33sims11$TreatedAtDose[1]), sum(n15p33sims11$TreatedAtDose[1])),
    c(11, 0.33, 30, 'stop', 'stop/1', n30p33sims11$ProbStop, #n.ar = 30
      sum(n30p33sims11$ProbSelect[1] + n30p33sims11$ProbStop), '-',
      sum(n30p33sims11$TreatedAtDose[1]), sum(n30p33sims11$TreatedAtDose[1])),
    c(11, 0.33, 45, 'stop', 'stop/1', n45p33sims11$ProbStop, #n.ar = 45
      sum(n45p33sims11$ProbSelect[1] + n45p33sims11$ProbStop), '-',
      sum(n45p33sims11$TreatedAtDose[1]), sum(n45p33sims11$TreatedAtDose[1])),
    c(11, 0.33, 60, 'stop', 'stop/1', n60p33sims11$ProbStop, #n.ar = 60
      sum(n60p33sims11$ProbSelect[1] + n60p33sims11$ProbStop), '-',
      sum(n60p33sims11$TreatedAtDose[1]), sum(n60p33sims11$TreatedAtDose[1])), 
    
    
    # Scenario 12
    # placebo.rand.prob = 0.2
    c(12, 0.2, 0, 2, '2-3', n0p20sims12$ProbSelect[2], # n.ar = 0
      sum(n0p20sims12$ProbSelect[2:3]), n0p20sims12$TreatedAtDose[2],
      sum(n0p20sims12$TreatedAtDose[2:3]), n0p20sims12$TreatedAtDose[1]),
    c(12, 0.2, 15, 2, '2-3', n15p20sims12$ProbSelect[2], #n.ar = 15
      sum(n15p20sims12$ProbSelect[2:3]), n15p20sims12$TreatedAtDose[2],
      sum(n15p20sims12$TreatedAtDose[2:3]), n15p20sims12$TreatedAtDose[1]),
    c(12, 0.2, 30, 2, '2-3', n30p20sims12$ProbSelect[2], #n.ar = 30
      sum(n30p20sims12$ProbSelect[2:3]), n30p20sims12$TreatedAtDose[2],
      sum(n30p20sims12$TreatedAtDose[2:3]), n30p20sims12$TreatedAtDose[1]),
    c(12, 0.2, 45, 2, '2-3', n45p20sims12$ProbSelect[2], #n.ar = 45
      sum(n45p20sims12$ProbSelect[2:3]), n45p20sims12$TreatedAtDose[2],
      sum(n45p20sims12$TreatedAtDose[2:3]), n45p20sims12$TreatedAtDose[1]),
    c(12, 0.2, 60, 2, '2-3', n60p20sims12$ProbSelect[2], #n.ar = 60
      sum(n60p20sims12$ProbSelect[2:3]), n60p20sims12$TreatedAtDose[2],
      sum(n60p20sims12$TreatedAtDose[2:3]), n60p20sims12$TreatedAtDose[1]),
    # placebo.rand.prob = 0.33
    c(12, 0.33, 0, 2, '2-3', n0p33sims12$ProbSelect[2], # n.ar = 0
      sum(n0p33sims12$ProbSelect[2:3]), n0p33sims12$TreatedAtDose[2],
      sum(n0p33sims12$TreatedAtDose[2:3]), n0p33sims12$TreatedAtDose[1]),
    c(12, 0.33, 15, 2, '2-3', n15p33sims12$ProbSelect[2], #n.ar = 15
      sum(n15p33sims12$ProbSelect[2:3]), n15p33sims12$TreatedAtDose[2],
      sum(n15p33sims12$TreatedAtDose[2:3]), n15p33sims12$TreatedAtDose[1]),
    c(12, 0.33, 30, 2, '2-3', n30p33sims12$ProbSelect[2], #n.ar = 30
      sum(n30p33sims12$ProbSelect[2:3]), n30p33sims12$TreatedAtDose[2],
      sum(n30p33sims12$TreatedAtDose[2:3]), n30p33sims12$TreatedAtDose[1]),
    c(12, 0.33, 45, 2, '2-3', n45p33sims12$ProbSelect[2], #n.ar = 45
      sum(n45p33sims12$ProbSelect[2:3]), n45p33sims12$TreatedAtDose[2],
      sum(n45p33sims12$TreatedAtDose[2:3]), n45p33sims12$TreatedAtDose[1]),
    c(12, 0.33, 60, 2, '2-3', n60p33sims12$ProbSelect[2], #n.ar = 60
      sum(n60p33sims12$ProbSelect[2:3]), n60p33sims12$TreatedAtDose[2],
      sum(n60p33sims12$TreatedAtDose[2:3]), n60p33sims12$TreatedAtDose[1]), 
    
    # Scenario 13
    # placebo.rand.prob = 0.2
    c(13, 0.2, 0, 'stop', 'stop/1', n0p20sims13$ProbStop, # n.ar = 0
      sum(n0p20sims13$ProbSelect[1] + n0p20sims13$ProbStop), '-',
      sum(n0p20sims13$TreatedAtDose[1]), sum(n0p20sims13$TreatedAtDose[1])),
    c(13, 0.2, 15, 'stop', 'stop/1', n15p20sims13$ProbStop, #n.ar = 15
      sum(n15p20sims13$ProbSelect[1] + n15p20sims13$ProbStop), '-',
      sum(n15p20sims13$TreatedAtDose[1]), sum(n15p20sims13$TreatedAtDose[1])),
    c(13, 0.2, 30, 'stop', 'stop/1', n30p20sims13$ProbStop, #n.ar = 30
      sum(n30p20sims13$ProbSelect[1] + n30p20sims13$ProbStop), '-',
      sum(n30p20sims13$TreatedAtDose[1]), sum(n30p20sims13$TreatedAtDose[1])),
    c(13, 0.2, 45, 'stop', 'stop/1', n45p20sims13$ProbStop, #n.ar = 45
      sum(n45p20sims13$ProbSelect[1] + n45p20sims13$ProbStop), '-',
      sum(n45p20sims13$TreatedAtDose[1]), sum(n45p20sims13$TreatedAtDose[1])),
    c(13, 0.2, 60, 'stop', 'stop/1', n60p20sims13$ProbStop, #n.ar = 60
      sum(n60p20sims13$ProbSelect[1] + n60p20sims13$ProbStop), '-',
      sum(n60p20sims13$TreatedAtDose[1]), sum(n60p20sims13$TreatedAtDose[1])),
    # placebo.rand.prob = 0.33
    c(13, 0.33, 0, 'stop', 'stop/1', n0p33sims13$ProbStop, # n.ar = 0
      sum(n0p33sims13$ProbSelect[1] + n0p33sims13$ProbStop), '-',
      sum(n0p33sims13$TreatedAtDose[1]), sum(n0p33sims13$TreatedAtDose[1])),
    c(13, 0.33, 15, 'stop', 'stop/1', n15p33sims13$ProbStop, #n.ar = 15
      sum(n15p33sims13$ProbSelect[1] + n15p33sims13$ProbStop), '-',
      sum(n15p33sims13$TreatedAtDose[1]), sum(n15p33sims13$TreatedAtDose[1])),
    c(13, 0.33, 30, 'stop', 'stop/1', n30p33sims13$ProbStop, #n.ar = 30
      sum(n30p33sims13$ProbSelect[1] + n30p33sims13$ProbStop), '-',
      sum(n30p33sims13$TreatedAtDose[1]), sum(n30p33sims13$TreatedAtDose[1])),
    c(13, 0.33, 45, 'stop', 'stop/1', n45p33sims13$ProbStop, #n.ar = 45
      sum(n45p33sims13$ProbSelect[1] + n45p33sims13$ProbStop), '-',
      sum(n45p33sims13$TreatedAtDose[1]), sum(n45p33sims13$TreatedAtDose[1])),
    c(13, 0.33, 60, 'stop', 'stop/1', n60p33sims13$ProbStop, #n.ar = 60
      sum(n60p33sims13$ProbSelect[1] + n60p33sims13$ProbStop), '-',
      sum(n60p33sims13$TreatedAtDose[1]), sum(n60p33sims13$TreatedAtDose[1])), 
    
    # Scenario 14
    # placebo.rand.prob = 0.2
    c(14, 0.2, 0, 'stop', 'stop/1', n0p20sims14$ProbStop, # n.ar = 0
      sum(n0p20sims14$ProbSelect[1] + n0p20sims14$ProbStop), '-',
      sum(n0p20sims14$TreatedAtDose[1]), sum(n0p20sims14$TreatedAtDose[1])),
    c(14, 0.2, 15, 'stop', 'stop/1', n15p20sims14$ProbStop, #n.ar = 15
      sum(n15p20sims14$ProbSelect[1] + n15p20sims14$ProbStop), '-',
      sum(n15p20sims14$TreatedAtDose[1]), sum(n15p20sims14$TreatedAtDose[1])),
    c(14, 0.2, 30, 'stop', 'stop/1', n30p20sims14$ProbStop, #n.ar = 30
      sum(n30p20sims14$ProbSelect[1] + n30p20sims14$ProbStop), '-',
      sum(n30p20sims14$TreatedAtDose[1]), sum(n30p20sims14$TreatedAtDose[1])),
    c(14, 0.2, 45, 'stop', 'stop/1', n45p20sims14$ProbStop, #n.ar = 45
      sum(n45p20sims14$ProbSelect[1] + n45p20sims14$ProbStop), '-',
      sum(n45p20sims14$TreatedAtDose[1]), sum(n45p20sims14$TreatedAtDose[1])),
    c(14, 0.2, 60, 'stop', 'stop/1', n60p20sims14$ProbStop, #n.ar = 60
      sum(n60p20sims14$ProbSelect[1] + n60p20sims14$ProbStop), '-',
      sum(n60p20sims14$TreatedAtDose[1]), sum(n60p20sims14$TreatedAtDose[1])),
    # placebo.rand.prob = 0.33
    c(14, 0.33, 0, 'stop', 'stop/1', n0p33sims14$ProbStop, # n.ar = 0
      sum(n0p33sims14$ProbSelect[1] + n0p33sims14$ProbStop), '-',
      sum(n0p33sims14$TreatedAtDose[1]), sum(n0p33sims14$TreatedAtDose[1])),
    c(14, 0.33, 15, 'stop', 'stop/1', n15p33sims14$ProbStop, #n.ar = 15
      sum(n15p33sims14$ProbSelect[1] + n15p33sims14$ProbStop), '-',
      sum(n15p33sims14$TreatedAtDose[1]), sum(n15p33sims14$TreatedAtDose[1])),
    c(14, 0.33, 30, 'stop', 'stop/1', n30p33sims14$ProbStop, #n.ar = 30
      sum(n30p33sims14$ProbSelect[1] + n30p33sims14$ProbStop), '-',
      sum(n30p33sims14$TreatedAtDose[1]), sum(n30p33sims14$TreatedAtDose[1])),
    c(14, 0.33, 45, 'stop', 'stop/1', n45p33sims14$ProbStop, #n.ar = 45
      sum(n45p33sims14$ProbSelect[1] + n45p33sims14$ProbStop), '-',
      sum(n45p33sims14$TreatedAtDose[1]), sum(n45p33sims14$TreatedAtDose[1])),
    c(14, 0.33, 60, 'stop', 'stop/1', n60p33sims14$ProbStop, #n.ar = 60
      sum(n60p33sims14$ProbSelect[1] + n60p33sims14$ProbStop), '-',
      sum(n60p33sims14$TreatedAtDose[1]), sum(n60p33sims14$TreatedAtDose[1])), 
    
    # Scenario 15
    # placebo.rand.prob = 0.2
    c(15, 0.2, 0, 'stop', 'stop/1', n0p20sims15$ProbStop, # n.ar = 0
      sum(n0p20sims15$ProbSelect[1] + n0p20sims15$ProbStop), '-',
      sum(n0p20sims15$TreatedAtDose[1]), sum(n0p20sims15$TreatedAtDose[1])),
    c(15, 0.2, 15, 'stop', 'stop/1', n15p20sims15$ProbStop, #n.ar = 15
      sum(n15p20sims15$ProbSelect[1] + n15p20sims15$ProbStop), '-',
      sum(n15p20sims15$TreatedAtDose[1]), sum(n15p20sims15$TreatedAtDose[1])),
    c(15, 0.2, 30, 'stop', 'stop/1', n30p20sims15$ProbStop, #n.ar = 30
      sum(n30p20sims15$ProbSelect[1] + n30p20sims15$ProbStop), '-',
      sum(n30p20sims15$TreatedAtDose[1]), sum(n30p20sims15$TreatedAtDose[1])),
    c(15, 0.2, 45, 'stop', 'stop/1', n45p20sims15$ProbStop, #n.ar = 45
      sum(n45p20sims15$ProbSelect[1] + n45p20sims15$ProbStop), '-',
      sum(n45p20sims15$TreatedAtDose[1]), sum(n45p20sims15$TreatedAtDose[1])),
    c(15, 0.2, 60, 'stop', 'stop/1', n60p20sims15$ProbStop, #n.ar = 60
      sum(n60p20sims15$ProbSelect[1] + n60p20sims15$ProbStop), '-',
      sum(n60p20sims15$TreatedAtDose[1]), sum(n60p20sims15$TreatedAtDose[1])),
    # placebo.rand.prob = 0.33
    c(15, 0.33, 0, 'stop', 'stop/1', n0p33sims15$ProbStop, # n.ar = 0
      sum(n0p33sims15$ProbSelect[1] + n0p33sims15$ProbStop), '-',
      sum(n0p33sims15$TreatedAtDose[1]), sum(n0p33sims15$TreatedAtDose[1])),
    c(15, 0.33, 15, 'stop', 'stop/1', n15p33sims15$ProbStop, #n.ar = 15
      sum(n15p33sims15$ProbSelect[1] + n15p33sims15$ProbStop), '-',
      sum(n15p33sims15$TreatedAtDose[1]), sum(n15p33sims15$TreatedAtDose[1])),
    c(15, 0.33, 30, 'stop', 'stop/1', n30p33sims15$ProbStop, #n.ar = 30
      sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop), '-',
      sum(n30p33sims15$TreatedAtDose[1]), sum(n30p33sims15$TreatedAtDose[1])),
    c(15, 0.33, 45, 'stop', 'stop/1', n45p33sims15$ProbStop, #n.ar = 45
      sum(n45p33sims15$ProbSelect[1] + n45p33sims15$ProbStop), '-',
      sum(n45p33sims15$TreatedAtDose[1]), sum(n45p33sims15$TreatedAtDose[1])),
    c(15, 0.33, 60, 'stop', 'stop/1', n60p33sims15$ProbStop, #n.ar = 60
      sum(n60p33sims15$ProbSelect[1] + n60p33sims15$ProbStop), '-',
      sum(n60p33sims15$TreatedAtDose[1]), sum(n60p33sims15$TreatedAtDose[1]))
  ) %>% 
  as.data.frame()

data$V6 <- as.numeric(data$V6)
data$V7 <- as.numeric(data$V7)

data %>% 
  mutate(V6 = round(V6, 2),
         V7 = round(V7, 2)) -> data


colnames(data) <- c('Scenario', '$phi_R$', '$j_R$', 'Best Dose Level',
                    'Good Dose Levels', 'Prob of Picking Best Dose', 
                    'Prob of Picking Good Doses', 'Mean N at Best Dose', 
                    'Mean N at Good Doses', 'Mean N at 1 (Control Dose)')

colnames(data) <- c('Scenario', '$phi_R$', '$j_R$', 'Best Dose',
                    'Good Doses', 'P(Best)', 'P(Good)', 'N(Best)', 
                    'N(Good)', 'N(Control)')

data %>% 
  kable(format = 'latex', linesep = '', longtable = T, booktabs = T, 
        align = c(rep('c', times = 10)), 
        caption = '\\label{tab_wt:OCsCombos}Operating characteristics for multiple combinations of AR phase size and probabilities for randomisation to control. Probability of selecting the best or good dose levels as the OBD, mean number of patients treated at those dose levels and at the control dose after 10000 simulations.'
        ) %>% 
  kable_styling(position = 'center', repeat_header_method = 'replace',
                latex_options = c('repeat_header', 'HOLD_position'), font_size = 9,
                repeat_header_text = "Operating characteristics (continued)") %>%
  # column_spec(2:3, width = '0.5cm') %>% 
  # column_spec(c(4,5,6,7,8,9,10), width = '1.5cm') %>%
  #olumn_spec(7 , width = '2.3cm') %>% 
  collapse_rows(columns = 1:2, latex_hline = "major",  valign = "middle") %>% 
  #landscape() %>% 
  cat()

rm(data)

# Table for operating characteristics for different combos of AR size and 
# randomisation to control probabilities looking at summary statistics. 

data <- rbind(
  # Probability Best Dose 
  # n.ar = 0, placebo.rand.prob = 0.2
  c(0.2, 0, n0p20sims1$ProbSelect[5], n0p20sims2$ProbStop,
    n0p20sims3$ProbSelect[3], n0p20sims4$ProbSelect[3], n0p20sims5$ProbStop,
    n0p20sims6$ProbSelect[3], n0p20sims7$ProbSelect[3], n0p20sims8$ProbStop, 
    n0p20sims9$ProbSelect[3], n0p20sims10$ProbSelect[2], n0p20sims11$ProbStop,
    n0p20sims12$ProbSelect[2], n0p20sims13$ProbStop, n0p20sims14$ProbStop, 
    n0p20sims15$ProbStop, 
    mean(c(n0p20sims1$ProbSelect[5], n0p20sims2$ProbStop,
           n0p20sims3$ProbSelect[3], n0p20sims4$ProbSelect[3], n0p20sims5$ProbStop,
           n0p20sims6$ProbSelect[3], n0p20sims7$ProbSelect[3], n0p20sims8$ProbStop, 
           n0p20sims9$ProbSelect[3], n0p20sims10$ProbSelect[2], n0p20sims11$ProbStop,
           n0p20sims12$ProbSelect[2], n0p20sims13$ProbStop, n0p20sims14$ProbStop, 
           n0p20sims15$ProbStop)),
    sd(c(n0p20sims1$ProbSelect[5], n0p20sims2$ProbStop,
         n0p20sims3$ProbSelect[3], n0p20sims4$ProbSelect[3], n0p20sims5$ProbStop,
         n0p20sims6$ProbSelect[3], n0p20sims7$ProbSelect[3], n0p20sims8$ProbStop, 
         n0p20sims9$ProbSelect[3], n0p20sims10$ProbSelect[2], n0p20sims11$ProbStop,
         n0p20sims12$ProbSelect[2], n0p20sims13$ProbStop, n0p20sims14$ProbStop, 
         n0p20sims15$ProbStop)), 
    mean(c(n0p20sims1$ProbSelect[5], n0p20sims3$ProbSelect[3], n0p20sims4$ProbSelect[3], 
           n0p20sims6$ProbSelect[3], n0p20sims7$ProbSelect[3], 
           n0p20sims9$ProbSelect[3], n0p20sims10$ProbSelect[2], 
           n0p20sims12$ProbSelect[2])),
    sd(c(n0p20sims1$ProbSelect[5], 
         n0p20sims3$ProbSelect[3], n0p20sims4$ProbSelect[3], 
         n0p20sims6$ProbSelect[3], n0p20sims7$ProbSelect[3],  
         n0p20sims9$ProbSelect[3], n0p20sims10$ProbSelect[2], 
         n0p20sims12$ProbSelect[2]))),
  
  # n.ar = 15, placebo.rand.prob = 0.2 
  c(0.2, 15, n15p20sims1$ProbSelect[5], n15p20sims2$ProbStop,
    n15p20sims3$ProbSelect[3], n15p20sims4$ProbSelect[3], n15p20sims5$ProbStop,
    n15p20sims6$ProbSelect[3], n15p20sims7$ProbSelect[3], n15p20sims8$ProbStop, 
    n15p20sims9$ProbSelect[3], n15p20sims10$ProbSelect[2], n15p20sims11$ProbStop,
    n15p20sims12$ProbSelect[2], n15p20sims13$ProbStop, n15p20sims14$ProbStop, 
    n15p20sims15$ProbStop, 
    mean(c(n15p20sims1$ProbSelect[5], n15p20sims2$ProbStop,
           n15p20sims3$ProbSelect[3], n15p20sims4$ProbSelect[3], n15p20sims5$ProbStop,
           n15p20sims6$ProbSelect[3], n15p20sims7$ProbSelect[3], n15p20sims8$ProbStop, 
           n15p20sims9$ProbSelect[3], n15p20sims10$ProbSelect[2], n15p20sims11$ProbStop,
           n15p20sims12$ProbSelect[2], n15p20sims13$ProbStop, n15p20sims14$ProbStop, 
           n15p20sims15$ProbStop)),
    sd(c(n15p20sims1$ProbSelect[5], n15p20sims2$ProbStop,
         n15p20sims3$ProbSelect[3], n15p20sims4$ProbSelect[3], n15p20sims5$ProbStop,
         n15p20sims6$ProbSelect[3], n15p20sims7$ProbSelect[3], n15p20sims8$ProbStop, 
         n15p20sims9$ProbSelect[3], n15p20sims10$ProbSelect[2], n15p20sims11$ProbStop,
         n15p20sims12$ProbSelect[2], n15p20sims13$ProbStop, n15p20sims14$ProbStop, 
         n15p20sims15$ProbStop)), 
    mean(c(n15p20sims1$ProbSelect[5], 
           n15p20sims3$ProbSelect[3], n15p20sims4$ProbSelect[3], 
           n15p20sims6$ProbSelect[3], n15p20sims7$ProbSelect[3], 
           n15p20sims9$ProbSelect[3], n15p20sims10$ProbSelect[2], 
           n15p20sims12$ProbSelect[2])),
    sd(c(n15p20sims1$ProbSelect[5], 
         n15p20sims3$ProbSelect[3], n15p20sims4$ProbSelect[3], 
         n15p20sims6$ProbSelect[3], n15p20sims7$ProbSelect[3], 
         n15p20sims9$ProbSelect[3], n15p20sims10$ProbSelect[2], 
         n15p20sims12$ProbSelect[2]))), 
  
  # n.ar = 30, placebo.rand.prob = 0.2 
  c(0.2, 30, n30p20sims1$ProbSelect[5], n30p20sims2$ProbStop,
    n30p20sims3$ProbSelect[3], n30p20sims4$ProbSelect[3], n30p20sims5$ProbStop,
    n30p20sims6$ProbSelect[3], n30p20sims7$ProbSelect[3], n30p20sims8$ProbStop, 
    n30p20sims9$ProbSelect[3], n30p20sims10$ProbSelect[2], n30p20sims11$ProbStop,
    n30p20sims12$ProbSelect[2], n30p20sims13$ProbStop, n30p20sims14$ProbStop, 
    n30p20sims15$ProbStop, 
    mean(c(n30p20sims1$ProbSelect[5], n30p20sims2$ProbStop,
           n30p20sims3$ProbSelect[3], n30p20sims4$ProbSelect[3], n30p20sims5$ProbStop,
           n30p20sims6$ProbSelect[3], n30p20sims7$ProbSelect[3], n30p20sims8$ProbStop, 
           n30p20sims9$ProbSelect[3], n30p20sims10$ProbSelect[2], n30p20sims11$ProbStop,
           n30p20sims12$ProbSelect[2], n30p20sims13$ProbStop, n30p20sims14$ProbStop, 
           n30p20sims15$ProbStop)),
    sd(c(n30p20sims1$ProbSelect[5], n30p20sims2$ProbStop,
         n30p20sims3$ProbSelect[3], n30p20sims4$ProbSelect[3], n30p20sims5$ProbStop,
         n30p20sims6$ProbSelect[3], n30p20sims7$ProbSelect[3], n30p20sims8$ProbStop, 
         n30p20sims9$ProbSelect[3], n30p20sims10$ProbSelect[2], n30p20sims11$ProbStop,
         n30p20sims12$ProbSelect[2], n30p20sims13$ProbStop, n30p20sims14$ProbStop, 
         n30p20sims15$ProbStop)), 
    mean(c(n30p20sims1$ProbSelect[5], 
           n30p20sims3$ProbSelect[3], n30p20sims4$ProbSelect[3], 
           n30p20sims6$ProbSelect[3], n30p20sims7$ProbSelect[3], 
           n30p20sims9$ProbSelect[3], n30p20sims10$ProbSelect[2], 
           n30p20sims12$ProbSelect[2])),
    sd(c(n30p20sims1$ProbSelect[5], 
         n30p20sims3$ProbSelect[3], n30p20sims4$ProbSelect[3], 
         n30p20sims6$ProbSelect[3], n30p20sims7$ProbSelect[3], 
         n30p20sims9$ProbSelect[3], n30p20sims10$ProbSelect[2], 
         n30p20sims12$ProbSelect[2]))), 
  
  # n.ar = 45, placebo.rand.prob = 0.2 
  c(0.2, 45, n45p20sims1$ProbSelect[5], n45p20sims2$ProbStop,
    n45p20sims3$ProbSelect[3], n45p20sims4$ProbSelect[3], n45p20sims5$ProbStop,
    n45p20sims6$ProbSelect[3], n45p20sims7$ProbSelect[3], n45p20sims8$ProbStop, 
    n45p20sims9$ProbSelect[3], n45p20sims10$ProbSelect[2], n45p20sims11$ProbStop,
    n45p20sims12$ProbSelect[2], n45p20sims13$ProbStop, n45p20sims14$ProbStop, 
    n45p20sims15$ProbStop, 
    mean(c(n45p20sims1$ProbSelect[5], n45p20sims2$ProbStop,
           n45p20sims3$ProbSelect[3], n45p20sims4$ProbSelect[3], n45p20sims5$ProbStop,
           n45p20sims6$ProbSelect[3], n45p20sims7$ProbSelect[3], n45p20sims8$ProbStop, 
           n45p20sims9$ProbSelect[3], n45p20sims10$ProbSelect[2], n45p20sims11$ProbStop,
           n45p20sims12$ProbSelect[2], n45p20sims13$ProbStop, n45p20sims14$ProbStop, 
           n45p20sims15$ProbStop)),
    sd(c(n45p20sims1$ProbSelect[5], n45p20sims2$ProbStop,
         n45p20sims3$ProbSelect[3], n45p20sims4$ProbSelect[3], n45p20sims5$ProbStop,
         n45p20sims6$ProbSelect[3], n45p20sims7$ProbSelect[3], n45p20sims8$ProbStop, 
         n45p20sims9$ProbSelect[3], n45p20sims10$ProbSelect[2], n45p20sims11$ProbStop,
         n45p20sims12$ProbSelect[2], n45p20sims13$ProbStop, n45p20sims14$ProbStop, 
         n45p20sims15$ProbStop)), 
    mean(c(n45p20sims1$ProbSelect[5], 
           n45p20sims3$ProbSelect[3], n45p20sims4$ProbSelect[3], 
           n45p20sims6$ProbSelect[3], n45p20sims7$ProbSelect[3],  
           n45p20sims9$ProbSelect[3], n45p20sims10$ProbSelect[2], 
           n45p20sims12$ProbSelect[2])),
    sd(c(n45p20sims1$ProbSelect[5], 
         n45p20sims3$ProbSelect[3], n45p20sims4$ProbSelect[3], 
         n45p20sims6$ProbSelect[3], n45p20sims7$ProbSelect[3],  
         n45p20sims9$ProbSelect[3], n45p20sims10$ProbSelect[2], 
         n45p20sims12$ProbSelect[2]))),
  
  # n.ar = 60, placebo.rand.prob = 0.2 
  c(0.2, 60, n60p20sims1$ProbSelect[5], n60p20sims2$ProbStop,
    n60p20sims3$ProbSelect[3], n60p20sims4$ProbSelect[3], n60p20sims5$ProbStop,
    n60p20sims6$ProbSelect[3], n60p20sims7$ProbSelect[3], n60p20sims8$ProbStop, 
    n60p20sims9$ProbSelect[3], n60p20sims10$ProbSelect[2], n60p20sims11$ProbStop,
    n60p20sims12$ProbSelect[2], n60p20sims13$ProbStop, n60p20sims14$ProbStop, 
    n60p20sims15$ProbStop, 
    mean(c(n60p20sims1$ProbSelect[5], n60p20sims2$ProbStop,
           n60p20sims3$ProbSelect[3], n60p20sims4$ProbSelect[3], n60p20sims5$ProbStop,
           n60p20sims6$ProbSelect[3], n60p20sims7$ProbSelect[3], n60p20sims8$ProbStop, 
           n60p20sims9$ProbSelect[3], n60p20sims10$ProbSelect[2], n60p20sims11$ProbStop,
           n60p20sims12$ProbSelect[2], n60p20sims13$ProbStop, n60p20sims14$ProbStop, 
           n60p20sims15$ProbStop)),
    sd(c(n60p20sims1$ProbSelect[5], n60p20sims2$ProbStop,
         n60p20sims3$ProbSelect[3], n60p20sims4$ProbSelect[3], n60p20sims5$ProbStop,
         n60p20sims6$ProbSelect[3], n60p20sims7$ProbSelect[3], n60p20sims8$ProbStop, 
         n60p20sims9$ProbSelect[3], n60p20sims10$ProbSelect[2], n60p20sims11$ProbStop,
         n60p20sims12$ProbSelect[2], n60p20sims13$ProbStop, n60p20sims14$ProbStop, 
         n60p20sims15$ProbStop)), 
    mean(c(n60p20sims1$ProbSelect[5], 
           n60p20sims3$ProbSelect[3], n60p20sims4$ProbSelect[3], 
           n60p20sims6$ProbSelect[3], n60p20sims7$ProbSelect[3], 
           n60p20sims9$ProbSelect[3], n60p20sims10$ProbSelect[2], 
           n60p20sims12$ProbSelect[2])),
    sd(c(n60p20sims1$ProbSelect[5], 
         n60p20sims3$ProbSelect[3], n60p20sims4$ProbSelect[3], 
         n60p20sims6$ProbSelect[3], n60p20sims7$ProbSelect[3], 
         n60p20sims9$ProbSelect[3], n60p20sims10$ProbSelect[2], 
         n60p20sims12$ProbSelect[2]))),
  
  # n.ar = 0, placebo.rand.prob = 0.33
  c(0.33, 0, n0p33sims1$ProbSelect[5], n0p33sims2$ProbStop,
    n0p33sims3$ProbSelect[3], n0p33sims4$ProbSelect[3], n0p33sims5$ProbStop,
    n0p33sims6$ProbSelect[3], n0p33sims7$ProbSelect[3], n0p33sims8$ProbStop, 
    n0p33sims9$ProbSelect[3], n0p33sims10$ProbSelect[2], n0p33sims11$ProbStop,
    n0p33sims12$ProbSelect[2], n0p33sims13$ProbStop, n0p33sims14$ProbStop, 
    n0p33sims15$ProbStop, 
    mean(c(n0p33sims1$ProbSelect[5], n0p33sims2$ProbStop,
           n0p33sims3$ProbSelect[3], n0p33sims4$ProbSelect[3], n0p33sims5$ProbStop,
           n0p33sims6$ProbSelect[3], n0p33sims7$ProbSelect[3], n0p33sims8$ProbStop, 
           n0p33sims9$ProbSelect[3], n0p33sims10$ProbSelect[2], n0p33sims11$ProbStop,
           n0p33sims12$ProbSelect[2], n0p33sims13$ProbStop, n0p33sims14$ProbStop, 
           n0p33sims15$ProbStop)),
    sd(c(n0p33sims1$ProbSelect[5], n0p33sims2$ProbStop,
         n0p33sims3$ProbSelect[3], n0p33sims4$ProbSelect[3], n0p33sims5$ProbStop,
         n0p33sims6$ProbSelect[3], n0p33sims7$ProbSelect[3], n0p33sims8$ProbStop, 
         n0p33sims9$ProbSelect[3], n0p33sims10$ProbSelect[2], n0p33sims11$ProbStop,
         n0p33sims12$ProbSelect[2], n0p33sims13$ProbStop, n0p33sims14$ProbStop, 
         n0p33sims15$ProbStop)), 
    mean(c(n0p33sims1$ProbSelect[5], 
           n0p33sims3$ProbSelect[3], n0p33sims4$ProbSelect[3], 
           n0p33sims6$ProbSelect[3], n0p33sims7$ProbSelect[3], 
           n0p33sims9$ProbSelect[3], n0p33sims10$ProbSelect[2], 
           n0p33sims12$ProbSelect[2])),
    sd(c(n0p33sims1$ProbSelect[5], 
         n0p33sims3$ProbSelect[3], n0p33sims4$ProbSelect[3], 
         n0p33sims6$ProbSelect[3], n0p33sims7$ProbSelect[3],  
         n0p33sims9$ProbSelect[3], n0p33sims10$ProbSelect[2], 
         n0p33sims12$ProbSelect[2]))),
  
  # n.ar = 15, placebo.rand.prob = 0.33 
  c(0.33, 15, n15p33sims1$ProbSelect[5], n15p33sims2$ProbStop,
    n15p33sims3$ProbSelect[3], n15p33sims4$ProbSelect[3], n15p33sims5$ProbStop,
    n15p33sims6$ProbSelect[3], n15p33sims7$ProbSelect[3], n15p33sims8$ProbStop, 
    n15p33sims9$ProbSelect[3], n15p33sims10$ProbSelect[2], n15p33sims11$ProbStop,
    n15p33sims12$ProbSelect[2], n15p33sims13$ProbStop, n15p33sims14$ProbStop, 
    n15p33sims15$ProbStop, 
    mean(c(n15p33sims1$ProbSelect[5], n15p33sims2$ProbStop,
           n15p33sims3$ProbSelect[3], n15p33sims4$ProbSelect[3], n15p33sims5$ProbStop,
           n15p33sims6$ProbSelect[3], n15p33sims7$ProbSelect[3], n15p33sims8$ProbStop, 
           n15p33sims9$ProbSelect[3], n15p33sims10$ProbSelect[2], n15p33sims11$ProbStop,
           n15p33sims12$ProbSelect[2], n15p33sims13$ProbStop, n15p33sims14$ProbStop, 
           n15p33sims15$ProbStop)),
    sd(c(n15p33sims1$ProbSelect[5], n15p33sims2$ProbStop,
         n15p33sims3$ProbSelect[3], n15p33sims4$ProbSelect[3], n15p33sims5$ProbStop,
         n15p33sims6$ProbSelect[3], n15p33sims7$ProbSelect[3], n15p33sims8$ProbStop, 
         n15p33sims9$ProbSelect[3], n15p33sims10$ProbSelect[2], n15p33sims11$ProbStop,
         n15p33sims12$ProbSelect[2], n15p33sims13$ProbStop, n15p33sims14$ProbStop, 
         n15p33sims15$ProbStop)), 
    mean(c(n15p33sims1$ProbSelect[5], 
           n15p33sims3$ProbSelect[3], n15p33sims4$ProbSelect[3], 
           n15p33sims6$ProbSelect[3], n15p33sims7$ProbSelect[3],  
           n15p33sims9$ProbSelect[3], n15p33sims10$ProbSelect[2], 
           n15p33sims12$ProbSelect[2])),
    sd(c(n15p33sims1$ProbSelect[5], 
         n15p33sims3$ProbSelect[3], n15p33sims4$ProbSelect[3], 
         n15p33sims6$ProbSelect[3], n15p33sims7$ProbSelect[3], 
         n15p33sims9$ProbSelect[3], n15p33sims10$ProbSelect[2], 
         n15p33sims12$ProbSelect[2]))), 
  
  # n.ar = 30, placebo.rand.prob = 0.33 
  c(0.33, 30, n30p33sims1$ProbSelect[5], n30p33sims2$ProbStop,
    n30p33sims3$ProbSelect[3], n30p33sims4$ProbSelect[3], n30p33sims5$ProbStop,
    n30p33sims6$ProbSelect[3], n30p33sims7$ProbSelect[3], n30p33sims8$ProbStop, 
    n30p33sims9$ProbSelect[3], n30p33sims10$ProbSelect[2], n30p33sims11$ProbStop,
    n30p33sims12$ProbSelect[2], n30p33sims13$ProbStop, n30p33sims14$ProbStop, 
    n30p33sims15$ProbStop, 
    mean(c(n30p33sims1$ProbSelect[5], n30p33sims2$ProbStop,
           n30p33sims3$ProbSelect[3], n30p33sims4$ProbSelect[3], n30p33sims5$ProbStop,
           n30p33sims6$ProbSelect[3], n30p33sims7$ProbSelect[3], n30p33sims8$ProbStop, 
           n30p33sims9$ProbSelect[3], n30p33sims10$ProbSelect[2], n30p33sims11$ProbStop,
           n30p33sims12$ProbSelect[2], n30p33sims13$ProbStop, n30p33sims14$ProbStop, 
           n30p33sims15$ProbStop)),
    sd(c(n30p33sims1$ProbSelect[5], n30p33sims2$ProbStop,
         n30p33sims3$ProbSelect[3], n30p33sims4$ProbSelect[3], n30p33sims5$ProbStop,
         n30p33sims6$ProbSelect[3], n30p33sims7$ProbSelect[3], n30p33sims8$ProbStop, 
         n30p33sims9$ProbSelect[3], n30p33sims10$ProbSelect[2], n30p33sims11$ProbStop,
         n30p33sims12$ProbSelect[2], n30p33sims13$ProbStop, n30p33sims14$ProbStop, 
         n30p33sims15$ProbStop)), 
    mean(c(n30p33sims1$ProbSelect[5], 
           n30p33sims3$ProbSelect[3], n30p33sims4$ProbSelect[3], 
           n30p33sims6$ProbSelect[3], n30p33sims7$ProbSelect[3], 
           n30p33sims9$ProbSelect[3], n30p33sims10$ProbSelect[2], 
           n30p33sims12$ProbSelect[2])),
    sd(c(n30p33sims1$ProbSelect[5], 
         n30p33sims3$ProbSelect[3], n30p33sims4$ProbSelect[3], 
         n30p33sims6$ProbSelect[3], n30p33sims7$ProbSelect[3],  
         n30p33sims9$ProbSelect[3], n30p33sims10$ProbSelect[2], 
         n30p33sims12$ProbSelect[2]))), 
  
  # n.ar = 45, placebo.rand.prob = 0.33 
  c(0.33, 45, n45p33sims1$ProbSelect[5], n45p33sims2$ProbStop,
    n45p33sims3$ProbSelect[3], n45p33sims4$ProbSelect[3], n45p33sims5$ProbStop,
    n45p33sims6$ProbSelect[3], n45p33sims7$ProbSelect[3], n45p33sims8$ProbStop, 
    n45p33sims9$ProbSelect[3], n45p33sims10$ProbSelect[2], n45p33sims11$ProbStop,
    n45p33sims12$ProbSelect[2], n45p33sims13$ProbStop, n45p33sims14$ProbStop, 
    n45p33sims15$ProbStop, 
    mean(c(n45p33sims1$ProbSelect[5], n45p33sims2$ProbStop,
           n45p33sims3$ProbSelect[3], n45p33sims4$ProbSelect[3], n45p33sims5$ProbStop,
           n45p33sims6$ProbSelect[3], n45p33sims7$ProbSelect[3], n45p33sims8$ProbStop, 
           n45p33sims9$ProbSelect[3], n45p33sims10$ProbSelect[2], n45p33sims11$ProbStop,
           n45p33sims12$ProbSelect[2], n45p33sims13$ProbStop, n45p33sims14$ProbStop, 
           n45p33sims15$ProbStop)),
    sd(c(n45p33sims1$ProbSelect[5], n45p33sims2$ProbStop,
         n45p33sims3$ProbSelect[3], n45p33sims4$ProbSelect[3], n45p33sims5$ProbStop,
         n45p33sims6$ProbSelect[3], n45p33sims7$ProbSelect[3], n45p33sims8$ProbStop, 
         n45p33sims9$ProbSelect[3], n45p33sims10$ProbSelect[2], n45p33sims11$ProbStop,
         n45p33sims12$ProbSelect[2], n45p33sims13$ProbStop, n45p33sims14$ProbStop, 
         n45p33sims15$ProbStop)), 
    mean(c(n45p33sims1$ProbSelect[5], 
           n45p33sims3$ProbSelect[3], n45p33sims4$ProbSelect[3], 
           n45p33sims6$ProbSelect[3], n45p33sims7$ProbSelect[3],  
           n45p33sims9$ProbSelect[3], n45p33sims10$ProbSelect[2], 
           n45p33sims12$ProbSelect[2])),
    sd(c(n45p33sims1$ProbSelect[5], 
         n45p33sims3$ProbSelect[3], n45p33sims4$ProbSelect[3], 
         n45p33sims6$ProbSelect[3], n45p33sims7$ProbSelect[3],  
         n45p33sims9$ProbSelect[3], n45p33sims10$ProbSelect[2], 
         n45p33sims12$ProbSelect[2]))),
  
  # n.ar = 60, placebo.rand.prob = 0.33 
  c(0.33, 60, n60p33sims1$ProbSelect[5], n60p33sims2$ProbStop,
    n60p33sims3$ProbSelect[3], n60p33sims4$ProbSelect[3], n60p33sims5$ProbStop,
    n60p33sims6$ProbSelect[3], n60p33sims7$ProbSelect[3], n60p33sims8$ProbStop, 
    n60p33sims9$ProbSelect[3], n60p33sims10$ProbSelect[2], n60p33sims11$ProbStop,
    n60p33sims12$ProbSelect[2], n60p33sims13$ProbStop, n60p33sims14$ProbStop, 
    n60p33sims15$ProbStop, 
    mean(c(n60p33sims1$ProbSelect[5], n60p33sims2$ProbStop,
           n60p33sims3$ProbSelect[3], n60p33sims4$ProbSelect[3], n60p33sims5$ProbStop,
           n60p33sims6$ProbSelect[3], n60p33sims7$ProbSelect[3], n60p33sims8$ProbStop, 
           n60p33sims9$ProbSelect[3], n60p33sims10$ProbSelect[2], n60p33sims11$ProbStop,
           n60p33sims12$ProbSelect[2], n60p33sims13$ProbStop, n60p33sims14$ProbStop, 
           n60p33sims15$ProbStop)),
    sd(c(n60p33sims1$ProbSelect[5], n60p33sims2$ProbStop,
         n60p33sims3$ProbSelect[3], n60p33sims4$ProbSelect[3], n60p33sims5$ProbStop,
         n60p33sims6$ProbSelect[3], n60p33sims7$ProbSelect[3], n60p33sims8$ProbStop, 
         n60p33sims9$ProbSelect[3], n60p33sims10$ProbSelect[2], n60p33sims11$ProbStop,
         n60p33sims12$ProbSelect[2], n60p33sims13$ProbStop, n60p33sims14$ProbStop, 
         n60p33sims15$ProbStop)), 
    mean(c(n60p33sims1$ProbSelect[5], 
           n60p33sims3$ProbSelect[3], n60p33sims4$ProbSelect[3], 
           n60p33sims6$ProbSelect[3], n60p33sims7$ProbSelect[3],  
           n60p33sims9$ProbSelect[3], n60p33sims10$ProbSelect[2], 
           n60p33sims12$ProbSelect[2])),
    sd(c(n60p33sims1$ProbSelect[5], 
         n60p33sims3$ProbSelect[3], n60p33sims4$ProbSelect[3],
         n60p33sims6$ProbSelect[3], n60p33sims7$ProbSelect[3],  
         n60p33sims9$ProbSelect[3], n60p33sims10$ProbSelect[2], 
         n60p33sims12$ProbSelect[2]))),
  
  
  #Probability good doses 
  # n.ar = 0, placebo.rand.prob = 0.2
  c(0.2, 0, sum(n0p20sims1$ProbSelect[3:5]),
    sum(n0p20sims2$ProbSelect[1] + n0p20sims2$ProbStop),
    sum(n0p20sims3$ProbSelect[3]),
    sum(n0p20sims4$ProbSelect[3:4]),
    sum(n0p20sims5$ProbSelect[1] + n0p20sims5$ProbStop),
    sum(n0p20sims6$ProbSelect[3]),
    sum(n0p20sims7$ProbSelect[3:5]),
    sum(n0p20sims8$ProbSelect[1] + n0p20sims8$ProbStop),
    sum(n0p20sims9$ProbSelect[3]), 
    sum(n0p20sims10$ProbSelect[2:4]),
    sum(n0p20sims11$ProbSelect[1] + n0p20sims11$ProbStop),
    sum(n0p20sims12$ProbSelect[2:3]),
    sum(n0p20sims13$ProbSelect[1] + n0p20sims13$ProbStop),
    sum(n0p20sims14$ProbSelect[1] + n0p20sims14$ProbStop),
    sum(n0p20sims15$ProbSelect[1] + n0p20sims15$ProbStop),
    mean(c(sum(n0p20sims1$ProbSelect[3:5]), 
           sum(n0p20sims2$ProbSelect[1] + n0p20sims2$ProbStop),
           sum(n0p20sims3$ProbSelect[3]),
           sum(n0p20sims4$ProbSelect[3:4]),
           sum(n0p20sims5$ProbSelect[1] + n0p20sims5$ProbStop),
           sum(n0p20sims6$ProbSelect[3]),
           sum(n0p20sims7$ProbSelect[3:5]),
           sum(n0p20sims8$ProbSelect[1] + n0p20sims8$ProbStop),
           sum(n0p20sims9$ProbSelect[3]), 
           sum(n0p20sims10$ProbSelect[2:4]),
           sum(n0p20sims11$ProbSelect[1] + n0p20sims11$ProbStop),
           sum(n0p20sims12$ProbSelect[2:3]),
           sum(n0p20sims13$ProbSelect[1] + n0p20sims13$ProbStop),
           sum(n0p20sims14$ProbSelect[1] + n0p20sims14$ProbStop),
           sum(n0p20sims15$ProbSelect[1] + n0p20sims15$ProbStop))),
    sd(c(sum(n0p20sims1$ProbSelect[3:5]), 
         sum(n0p20sims2$ProbSelect[1] + n0p20sims2$ProbStop),
         sum(n0p20sims3$ProbSelect[3]),
         sum(n0p20sims4$ProbSelect[3:4]),
         sum(n0p20sims5$ProbSelect[1] + n0p20sims5$ProbStop),
         sum(n0p20sims6$ProbSelect[3]),
         sum(n0p20sims7$ProbSelect[3:5]),
         sum(n0p20sims8$ProbSelect[1] + n0p20sims8$ProbStop),
         sum(n0p20sims9$ProbSelect[3]), 
         sum(n0p20sims10$ProbSelect[2:4]),
         sum(n0p20sims11$ProbSelect[1] + n0p20sims11$ProbStop),
         sum(n0p20sims12$ProbSelect[2:3]),
         sum(n0p20sims13$ProbSelect[1] + n0p20sims13$ProbStop),
         sum(n0p20sims14$ProbSelect[1] + n0p20sims14$ProbStop),
         sum(n0p20sims15$ProbSelect[1] + n0p20sims15$ProbStop))),
    mean(c(sum(n0p20sims1$ProbSelect[3:5]), 
           sum(n0p20sims3$ProbSelect[3]),
           sum(n0p20sims4$ProbSelect[3:4]),
           sum(n0p20sims6$ProbSelect[3]),
           sum(n0p20sims7$ProbSelect[3:5]),
           sum(n0p20sims9$ProbSelect[3]), 
           sum(n0p20sims10$ProbSelect[2:4]),
           sum(n0p20sims12$ProbSelect[2:3]))),
    sd(c(sum(n0p20sims1$ProbSelect[3:5]), 
         sum(n0p20sims3$ProbSelect[3]),
         sum(n0p20sims4$ProbSelect[3:4]),
         sum(n0p20sims6$ProbSelect[3]),
         sum(n0p20sims7$ProbSelect[3:5]),
         sum(n0p20sims9$ProbSelect[3]), 
         sum(n0p20sims10$ProbSelect[2:4]),
         sum(n0p20sims12$ProbSelect[2:3])))),
  
  # n.ar = 15, placebo.rand.prob = 0.2
  c(0.2, 15, sum(n15p20sims1$ProbSelect[3:5]),
    sum(n15p20sims2$ProbSelect[1] + n15p20sims2$ProbStop),
    sum(n15p20sims3$ProbSelect[3]),
    sum(n15p20sims4$ProbSelect[3:4]),
    sum(n15p20sims5$ProbSelect[1] + n15p20sims5$ProbStop),
    sum(n15p20sims6$ProbSelect[3]),
    sum(n15p20sims7$ProbSelect[3:5]),
    sum(n15p20sims8$ProbSelect[1] + n15p20sims8$ProbStop),
    sum(n15p20sims9$ProbSelect[3]), 
    sum(n15p20sims10$ProbSelect[2:4]),
    sum(n15p20sims11$ProbSelect[1] + n15p20sims11$ProbStop),
    sum(n15p20sims12$ProbSelect[2:3]),
    sum(n15p20sims13$ProbSelect[1] + n15p20sims13$ProbStop),
    sum(n15p20sims14$ProbSelect[1] + n15p20sims14$ProbStop),
    sum(n15p20sims15$ProbSelect[1] + n15p20sims15$ProbStop),
    mean(c(sum(n15p20sims1$ProbSelect[3:5]), 
           sum(n15p20sims2$ProbSelect[1] + n15p20sims2$ProbStop),
           sum(n15p20sims3$ProbSelect[3]),
           sum(n15p20sims4$ProbSelect[3:4]),
           sum(n15p20sims5$ProbSelect[1] + n15p20sims5$ProbStop),
           sum(n15p20sims6$ProbSelect[3]),
           sum(n15p20sims7$ProbSelect[3:5]),
           sum(n15p20sims8$ProbSelect[1] + n15p20sims8$ProbStop),
           sum(n15p20sims9$ProbSelect[3]), 
           sum(n15p20sims10$ProbSelect[2:4]),
           sum(n15p20sims11$ProbSelect[1] + n15p20sims11$ProbStop),
           sum(n15p20sims12$ProbSelect[2:3]),
           sum(n15p20sims13$ProbSelect[1] + n15p20sims13$ProbStop),
           sum(n15p20sims14$ProbSelect[1] + n15p20sims14$ProbStop),
           sum(n15p20sims15$ProbSelect[1] + n15p20sims15$ProbStop))),
    sd(c(sum(n15p20sims1$ProbSelect[3:5]), 
         sum(n15p20sims2$ProbSelect[1] + n15p20sims2$ProbStop),
         sum(n15p20sims3$ProbSelect[3]),
         sum(n15p20sims4$ProbSelect[3:4]),
         sum(n15p20sims5$ProbSelect[1] + n15p20sims5$ProbStop),
         sum(n15p20sims6$ProbSelect[3]),
         sum(n15p20sims7$ProbSelect[3:5]),
         sum(n15p20sims8$ProbSelect[1] + n15p20sims8$ProbStop),
         sum(n15p20sims9$ProbSelect[3]), 
         sum(n15p20sims10$ProbSelect[2:4]),
         sum(n15p20sims11$ProbSelect[1] + n15p20sims11$ProbStop),
         sum(n15p20sims12$ProbSelect[2:3]),
         sum(n15p20sims13$ProbSelect[1] + n15p20sims13$ProbStop),
         sum(n15p20sims14$ProbSelect[1] + n15p20sims14$ProbStop),
         sum(n15p20sims15$ProbSelect[1] + n15p20sims15$ProbStop))),
    mean(c(sum(n15p20sims1$ProbSelect[3:5]), 
           sum(n15p20sims3$ProbSelect[3]),
           sum(n15p20sims4$ProbSelect[3:4]),
           sum(n15p20sims6$ProbSelect[3]),
           sum(n15p20sims7$ProbSelect[3:5]),
           sum(n15p20sims9$ProbSelect[3]), 
           sum(n15p20sims10$ProbSelect[2:4]),
           sum(n15p20sims12$ProbSelect[2:3]))),
    sd(c(sum(n15p20sims1$ProbSelect[3:5]), 
         sum(n15p20sims3$ProbSelect[3]),
         sum(n15p20sims4$ProbSelect[3:4]),
         sum(n15p20sims6$ProbSelect[3]),
         sum(n15p20sims7$ProbSelect[3:5]),
         sum(n15p20sims9$ProbSelect[3]), 
         sum(n15p20sims10$ProbSelect[2:4]),
         sum(n15p20sims12$ProbSelect[2:3])))),
  
  # n.ar = 30, placebo.rand.prob = 0.2
  c(0.2, 30, sum(n30p20sims1$ProbSelect[3:5]),
    sum(n30p20sims2$ProbSelect[1] + n30p20sims2$ProbStop),
    sum(n30p20sims3$ProbSelect[3]),
    sum(n30p20sims4$ProbSelect[3:4]),
    sum(n30p20sims5$ProbSelect[1] + n30p20sims5$ProbStop),
    sum(n30p20sims6$ProbSelect[3]),
    sum(n30p20sims7$ProbSelect[3:5]),
    sum(n30p20sims8$ProbSelect[1] + n30p20sims8$ProbStop),
    sum(n30p20sims9$ProbSelect[3]), 
    sum(n30p20sims10$ProbSelect[2:4]),
    sum(n30p20sims11$ProbSelect[1] + n30p20sims11$ProbStop),
    sum(n30p20sims12$ProbSelect[2:3]),
    sum(n30p20sims13$ProbSelect[1] + n30p20sims13$ProbStop),
    sum(n30p20sims14$ProbSelect[1] + n30p20sims14$ProbStop),
    sum(n30p20sims15$ProbSelect[1] + n30p20sims15$ProbStop),
    mean(c(sum(n30p20sims1$ProbSelect[3:5]), 
           sum(n30p20sims2$ProbSelect[1] + n30p20sims2$ProbStop),
           sum(n30p20sims3$ProbSelect[3]),
           sum(n30p20sims4$ProbSelect[3:4]),
           sum(n30p20sims5$ProbSelect[1] + n30p20sims5$ProbStop),
           sum(n30p20sims6$ProbSelect[3]),
           sum(n30p20sims7$ProbSelect[3:5]),
           sum(n30p20sims8$ProbSelect[1] + n30p20sims8$ProbStop),
           sum(n30p20sims9$ProbSelect[3]), 
           sum(n30p20sims10$ProbSelect[2:4]),
           sum(n30p20sims11$ProbSelect[1] + n30p20sims11$ProbStop),
           sum(n30p20sims12$ProbSelect[2:3]),
           sum(n30p20sims13$ProbSelect[1] + n30p20sims13$ProbStop),
           sum(n30p20sims14$ProbSelect[1] + n30p20sims14$ProbStop),
           sum(n30p20sims15$ProbSelect[1] + n30p20sims15$ProbStop))),
    sd(c(sum(n30p20sims1$ProbSelect[3:5]), 
         sum(n30p20sims2$ProbSelect[1] + n30p20sims2$ProbStop),
         sum(n30p20sims3$ProbSelect[3]),
         sum(n30p20sims4$ProbSelect[3:4]),
         sum(n30p20sims5$ProbSelect[1] + n30p20sims5$ProbStop),
         sum(n30p20sims6$ProbSelect[3]),
         sum(n30p20sims7$ProbSelect[3:5]),
         sum(n30p20sims8$ProbSelect[1] + n30p20sims8$ProbStop),
         sum(n30p20sims9$ProbSelect[3]), 
         sum(n30p20sims10$ProbSelect[2:4]),
         sum(n30p20sims11$ProbSelect[1] + n30p20sims11$ProbStop),
         sum(n30p20sims12$ProbSelect[2:3]),
         sum(n30p20sims13$ProbSelect[1] + n30p20sims13$ProbStop),
         sum(n30p20sims14$ProbSelect[1] + n30p20sims14$ProbStop),
         sum(n30p20sims15$ProbSelect[1] + n30p20sims15$ProbStop))),
    mean(c(sum(n30p20sims1$ProbSelect[3:5]), 
           sum(n30p20sims3$ProbSelect[3]),
           sum(n30p20sims4$ProbSelect[3:4]),
           sum(n30p20sims6$ProbSelect[3]),
           sum(n30p20sims7$ProbSelect[3:5]),
           sum(n30p20sims9$ProbSelect[3]), 
           sum(n30p20sims10$ProbSelect[2:4]),
           sum(n30p20sims12$ProbSelect[2:3]))),
    sd(c(sum(n30p20sims1$ProbSelect[3:5]), 
         sum(n30p20sims3$ProbSelect[3]),
         sum(n30p20sims4$ProbSelect[3:4]),
         sum(n30p20sims6$ProbSelect[3]),
         sum(n30p20sims7$ProbSelect[3:5]),
         sum(n30p20sims9$ProbSelect[3]), 
         sum(n30p20sims10$ProbSelect[2:4]),
         sum(n30p20sims12$ProbSelect[2:3])))),
  
  # n.ar = 45, placebo.rand.prob = 0.2
  c(0.2, 45, sum(n45p20sims1$ProbSelect[3:5]),
    sum(n45p20sims2$ProbSelect[1] + n45p20sims2$ProbStop),
    sum(n45p20sims3$ProbSelect[3]),
    sum(n45p20sims4$ProbSelect[3:4]),
    sum(n45p20sims5$ProbSelect[1] + n45p20sims5$ProbStop),
    sum(n45p20sims6$ProbSelect[3]),
    sum(n45p20sims7$ProbSelect[3:5]),
    sum(n45p20sims8$ProbSelect[1] + n45p20sims8$ProbStop),
    sum(n45p20sims9$ProbSelect[3]), 
    sum(n45p20sims10$ProbSelect[2:4]),
    sum(n45p20sims11$ProbSelect[1] + n45p20sims11$ProbStop),
    sum(n45p20sims12$ProbSelect[2:3]),
    sum(n45p20sims13$ProbSelect[1] + n45p20sims13$ProbStop),
    sum(n45p20sims14$ProbSelect[1] + n45p20sims14$ProbStop),
    sum(n45p20sims15$ProbSelect[1] + n45p20sims15$ProbStop),
    mean(c(sum(n45p20sims1$ProbSelect[3:5]), 
           sum(n45p20sims2$ProbSelect[1] + n45p20sims2$ProbStop),
           sum(n45p20sims3$ProbSelect[3]),
           sum(n45p20sims4$ProbSelect[3:4]),
           sum(n45p20sims5$ProbSelect[1] + n45p20sims5$ProbStop),
           sum(n45p20sims6$ProbSelect[3]),
           sum(n45p20sims7$ProbSelect[3:5]),
           sum(n45p20sims8$ProbSelect[1] + n45p20sims8$ProbStop),
           sum(n45p20sims9$ProbSelect[3]), 
           sum(n45p20sims10$ProbSelect[2:4]),
           sum(n45p20sims11$ProbSelect[1] + n45p20sims11$ProbStop),
           sum(n45p20sims12$ProbSelect[2:3]),
           sum(n45p20sims13$ProbSelect[1] + n45p20sims13$ProbStop),
           sum(n45p20sims14$ProbSelect[1] + n45p20sims14$ProbStop),
           sum(n45p20sims15$ProbSelect[1] + n45p20sims15$ProbStop))),
    sd(c(sum(n45p20sims1$ProbSelect[3:5]), 
         sum(n45p20sims2$ProbSelect[1] + n45p20sims2$ProbStop),
         sum(n45p20sims3$ProbSelect[3]),
         sum(n45p20sims4$ProbSelect[3:4]),
         sum(n45p20sims5$ProbSelect[1] + n45p20sims5$ProbStop),
         sum(n45p20sims6$ProbSelect[3]),
         sum(n45p20sims7$ProbSelect[3:5]),
         sum(n45p20sims8$ProbSelect[1] + n45p20sims8$ProbStop),
         sum(n45p20sims9$ProbSelect[3]), 
         sum(n45p20sims10$ProbSelect[2:4]),
         sum(n45p20sims11$ProbSelect[1] + n45p20sims11$ProbStop),
         sum(n45p20sims12$ProbSelect[2:3]),
         sum(n45p20sims13$ProbSelect[1] + n45p20sims13$ProbStop),
         sum(n45p20sims14$ProbSelect[1] + n45p20sims14$ProbStop),
         sum(n45p20sims15$ProbSelect[1] + n45p20sims15$ProbStop))),
    mean(c(sum(n45p20sims1$ProbSelect[3:5]), 
           sum(n45p20sims3$ProbSelect[3]),
           sum(n45p20sims4$ProbSelect[3:4]),
           sum(n45p20sims6$ProbSelect[3]),
           sum(n45p20sims7$ProbSelect[3:5]),
           sum(n45p20sims9$ProbSelect[3]), 
           sum(n45p20sims10$ProbSelect[2:4]),
           sum(n45p20sims12$ProbSelect[2:3]))),
    sd(c(sum(n45p20sims1$ProbSelect[3:5]), 
         sum(n45p20sims3$ProbSelect[3]),
         sum(n45p20sims4$ProbSelect[3:4]),
         sum(n45p20sims6$ProbSelect[3]),
         sum(n45p20sims7$ProbSelect[3:5]),
         sum(n45p20sims9$ProbSelect[3]), 
         sum(n45p20sims10$ProbSelect[2:4]),
         sum(n45p20sims12$ProbSelect[2:3])))),
  
  # n.ar = 60, placebo.rand.prob = 0.2
  c(0.2, 60, sum(n60p20sims1$ProbSelect[3:5]),
    sum(n60p20sims2$ProbSelect[1] + n60p20sims2$ProbStop),
    sum(n60p20sims3$ProbSelect[3]),
    sum(n60p20sims4$ProbSelect[3:4]),
    sum(n60p20sims5$ProbSelect[1] + n60p20sims5$ProbStop),
    sum(n60p20sims6$ProbSelect[3]),
    sum(n60p20sims7$ProbSelect[3:5]),
    sum(n60p20sims8$ProbSelect[1] + n60p20sims8$ProbStop),
    sum(n60p20sims9$ProbSelect[3]), 
    sum(n60p20sims10$ProbSelect[2:4]),
    sum(n60p20sims11$ProbSelect[1] + n60p20sims11$ProbStop),
    sum(n60p20sims12$ProbSelect[2:3]),
    sum(n60p20sims13$ProbSelect[1] + n60p20sims13$ProbStop),
    sum(n60p20sims14$ProbSelect[1] + n60p20sims14$ProbStop),
    sum(n60p20sims15$ProbSelect[1] + n60p20sims15$ProbStop),
    mean(c(sum(n60p20sims1$ProbSelect[3:5]), 
           sum(n60p20sims2$ProbSelect[1] + n60p20sims2$ProbStop),
           sum(n60p20sims3$ProbSelect[3]),
           sum(n60p20sims4$ProbSelect[3:4]),
           sum(n60p20sims5$ProbSelect[1] + n60p20sims5$ProbStop),
           sum(n60p20sims6$ProbSelect[3]),
           sum(n60p20sims7$ProbSelect[3:5]),
           sum(n60p20sims8$ProbSelect[1] + n60p20sims8$ProbStop),
           sum(n60p20sims9$ProbSelect[3]), 
           sum(n60p20sims10$ProbSelect[2:4]),
           sum(n60p20sims11$ProbSelect[1] + n60p20sims11$ProbStop),
           sum(n60p20sims12$ProbSelect[2:3]),
           sum(n60p20sims13$ProbSelect[1] + n60p20sims13$ProbStop),
           sum(n60p20sims14$ProbSelect[1] + n60p20sims14$ProbStop),
           sum(n60p20sims15$ProbSelect[1] + n60p20sims15$ProbStop))),
    sd(c(sum(n60p20sims1$ProbSelect[3:5]), 
         sum(n60p20sims2$ProbSelect[1] + n60p20sims2$ProbStop),
         sum(n60p20sims3$ProbSelect[3]),
         sum(n60p20sims4$ProbSelect[3:4]),
         sum(n60p20sims5$ProbSelect[1] + n60p20sims5$ProbStop),
         sum(n60p20sims6$ProbSelect[3]),
         sum(n60p20sims7$ProbSelect[3:5]),
         sum(n60p20sims8$ProbSelect[1] + n60p20sims8$ProbStop),
         sum(n60p20sims9$ProbSelect[3]), 
         sum(n60p20sims10$ProbSelect[2:4]),
         sum(n60p20sims11$ProbSelect[1] + n60p20sims11$ProbStop),
         sum(n60p20sims12$ProbSelect[2:3]),
         sum(n60p20sims13$ProbSelect[1] + n60p20sims13$ProbStop),
         sum(n60p20sims14$ProbSelect[1] + n60p20sims14$ProbStop),
         sum(n60p20sims15$ProbSelect[1] + n60p20sims15$ProbStop))),
    mean(c(sum(n60p20sims1$ProbSelect[3:5]), 
           sum(n60p20sims3$ProbSelect[3]),
           sum(n60p20sims4$ProbSelect[3:4]),
           sum(n60p20sims6$ProbSelect[3]),
           sum(n60p20sims7$ProbSelect[3:5]),
           sum(n60p20sims9$ProbSelect[3]), 
           sum(n60p20sims10$ProbSelect[2:4]),
           sum(n60p20sims12$ProbSelect[2:3]))),
    sd(c(sum(n60p20sims1$ProbSelect[3:5]), 
         sum(n60p20sims3$ProbSelect[3]),
         sum(n60p20sims4$ProbSelect[3:4]),
         sum(n60p20sims6$ProbSelect[3]),
         sum(n60p20sims7$ProbSelect[3:5]),
         sum(n60p20sims9$ProbSelect[3]), 
         sum(n60p20sims10$ProbSelect[2:4]),
         sum(n60p20sims12$ProbSelect[2:3])))),
  
  # n.ar = 0, placebo.rand.prob = 0.33
  c(0.33, 0, sum(n0p33sims1$ProbSelect[3:5]),
    sum(n0p33sims2$ProbSelect[1] + n0p33sims2$ProbStop),
    sum(n0p33sims3$ProbSelect[3]),
    sum(n0p33sims4$ProbSelect[3:4]),
    sum(n0p33sims5$ProbSelect[1] + n0p33sims5$ProbStop),
    sum(n0p33sims6$ProbSelect[3]),
    sum(n0p33sims7$ProbSelect[3:5]),
    sum(n0p33sims8$ProbSelect[1] + n0p33sims8$ProbStop),
    sum(n0p33sims9$ProbSelect[3]), 
    sum(n0p33sims10$ProbSelect[2:4]),
    sum(n0p33sims11$ProbSelect[1] + n0p33sims11$ProbStop),
    sum(n0p33sims12$ProbSelect[2:3]),
    sum(n0p33sims13$ProbSelect[1] + n0p33sims13$ProbStop),
    sum(n0p33sims14$ProbSelect[1] + n0p33sims14$ProbStop),
    sum(n0p33sims15$ProbSelect[1] + n0p33sims15$ProbStop),
    mean(c(sum(n0p33sims1$ProbSelect[3:5]), 
           sum(n0p33sims2$ProbSelect[1] + n0p33sims2$ProbStop),
           sum(n0p33sims3$ProbSelect[3]),
           sum(n0p33sims4$ProbSelect[3:4]),
           sum(n0p33sims5$ProbSelect[1] + n0p33sims5$ProbStop),
           sum(n0p33sims6$ProbSelect[3]),
           sum(n0p33sims7$ProbSelect[3:5]),
           sum(n0p33sims8$ProbSelect[1] + n0p33sims8$ProbStop),
           sum(n0p33sims9$ProbSelect[3]), 
           sum(n0p33sims10$ProbSelect[2:4]),
           sum(n0p33sims11$ProbSelect[1] + n0p33sims11$ProbStop),
           sum(n0p33sims12$ProbSelect[2:3]),
           sum(n0p33sims13$ProbSelect[1] + n0p33sims13$ProbStop),
           sum(n0p33sims14$ProbSelect[1] + n0p33sims14$ProbStop),
           sum(n0p33sims15$ProbSelect[1] + n0p33sims15$ProbStop))),
    sd(c(sum(n0p33sims1$ProbSelect[3:5]), 
         sum(n0p33sims2$ProbSelect[1] + n0p33sims2$ProbStop),
         sum(n0p33sims3$ProbSelect[3]),
         sum(n0p33sims4$ProbSelect[3:4]),
         sum(n0p33sims5$ProbSelect[1] + n0p33sims5$ProbStop),
         sum(n0p33sims6$ProbSelect[3]),
         sum(n0p33sims7$ProbSelect[3:5]),
         sum(n0p33sims8$ProbSelect[1] + n0p33sims8$ProbStop),
         sum(n0p33sims9$ProbSelect[3]), 
         sum(n0p33sims10$ProbSelect[2:4]),
         sum(n0p33sims11$ProbSelect[1] + n0p33sims11$ProbStop),
         sum(n0p33sims12$ProbSelect[2:3]),
         sum(n0p33sims13$ProbSelect[1] + n0p33sims13$ProbStop),
         sum(n0p33sims14$ProbSelect[1] + n0p33sims14$ProbStop),
         sum(n0p33sims15$ProbSelect[1] + n0p33sims15$ProbStop))),
    mean(c(sum(n0p33sims1$ProbSelect[3:5]), 
           sum(n0p33sims3$ProbSelect[3]),
           sum(n0p33sims4$ProbSelect[3:4]),
           sum(n0p33sims6$ProbSelect[3]),
           sum(n0p33sims7$ProbSelect[3:5]),
           sum(n0p33sims9$ProbSelect[3]), 
           sum(n0p33sims10$ProbSelect[2:4]),
           sum(n0p33sims12$ProbSelect[2:3]))),
    sd(c(sum(n0p33sims1$ProbSelect[3:5]), 
         sum(n0p33sims3$ProbSelect[3]),
         sum(n0p33sims4$ProbSelect[3:4]),
         sum(n0p33sims6$ProbSelect[3]),
         sum(n0p33sims7$ProbSelect[3:5]),
         sum(n0p33sims9$ProbSelect[3]), 
         sum(n0p33sims10$ProbSelect[2:4]),
         sum(n0p33sims12$ProbSelect[2:3])))),
  
  # n.ar = 15, placebo.rand.prob = 0.33
  c(0.33, 15, sum(n15p33sims1$ProbSelect[3:5]),
    sum(n15p33sims2$ProbSelect[1] + n15p33sims2$ProbStop),
    sum(n15p33sims3$ProbSelect[3]),
    sum(n15p33sims4$ProbSelect[3:4]),
    sum(n15p33sims5$ProbSelect[1] + n15p33sims5$ProbStop),
    sum(n15p33sims6$ProbSelect[3]),
    sum(n15p33sims7$ProbSelect[3:5]),
    sum(n15p33sims8$ProbSelect[1] + n15p33sims8$ProbStop),
    sum(n15p33sims9$ProbSelect[3]), 
    sum(n15p33sims10$ProbSelect[2:4]),
    sum(n15p33sims11$ProbSelect[1] + n15p33sims11$ProbStop),
    sum(n15p33sims12$ProbSelect[2:3]),
    sum(n15p33sims13$ProbSelect[1] + n15p33sims13$ProbStop),
    sum(n15p33sims14$ProbSelect[1] + n15p33sims14$ProbStop),
    sum(n15p33sims15$ProbSelect[1] + n15p33sims15$ProbStop),
    mean(c(sum(n15p33sims1$ProbSelect[3:5]), 
           sum(n15p33sims2$ProbSelect[1] + n15p33sims2$ProbStop),
           sum(n15p33sims3$ProbSelect[3]),
           sum(n15p33sims4$ProbSelect[3:4]),
           sum(n15p33sims5$ProbSelect[1] + n15p33sims5$ProbStop),
           sum(n15p33sims6$ProbSelect[3]),
           sum(n15p33sims7$ProbSelect[3:5]),
           sum(n15p33sims8$ProbSelect[1] + n15p33sims8$ProbStop),
           sum(n15p33sims9$ProbSelect[3]), 
           sum(n15p33sims10$ProbSelect[2:4]),
           sum(n15p33sims11$ProbSelect[1] + n15p33sims11$ProbStop),
           sum(n15p33sims12$ProbSelect[2:3]),
           sum(n15p33sims13$ProbSelect[1] + n15p33sims13$ProbStop),
           sum(n15p33sims14$ProbSelect[1] + n15p33sims14$ProbStop),
           sum(n15p33sims15$ProbSelect[1] + n15p33sims15$ProbStop))),
    sd(c(sum(n15p33sims1$ProbSelect[3:5]), 
         sum(n15p33sims2$ProbSelect[1] + n15p33sims2$ProbStop),
         sum(n15p33sims3$ProbSelect[3]),
         sum(n15p33sims4$ProbSelect[3:4]),
         sum(n15p33sims5$ProbSelect[1] + n15p33sims5$ProbStop),
         sum(n15p33sims6$ProbSelect[3]),
         sum(n15p33sims7$ProbSelect[3:5]),
         sum(n15p33sims8$ProbSelect[1] + n15p33sims8$ProbStop),
         sum(n15p33sims9$ProbSelect[3]), 
         sum(n15p33sims10$ProbSelect[2:4]),
         sum(n15p33sims11$ProbSelect[1] + n15p33sims11$ProbStop),
         sum(n15p33sims12$ProbSelect[2:3]),
         sum(n15p33sims13$ProbSelect[1] + n15p33sims13$ProbStop),
         sum(n15p33sims14$ProbSelect[1] + n15p33sims14$ProbStop),
         sum(n15p33sims15$ProbSelect[1] + n15p33sims15$ProbStop))),
    mean(c(sum(n15p33sims1$ProbSelect[3:5]), 
           sum(n15p33sims3$ProbSelect[3]),
           sum(n15p33sims4$ProbSelect[3:4]),
           sum(n15p33sims6$ProbSelect[3]),
           sum(n15p33sims7$ProbSelect[3:5]),
           sum(n15p33sims9$ProbSelect[3]), 
           sum(n15p33sims10$ProbSelect[2:4]),
           sum(n15p33sims12$ProbSelect[2:3]))),
    sd(c(sum(n15p33sims1$ProbSelect[3:5]), 
         sum(n15p33sims3$ProbSelect[3]),
         sum(n15p33sims4$ProbSelect[3:4]),
         sum(n15p33sims6$ProbSelect[3]),
         sum(n15p33sims7$ProbSelect[3:5]),
         sum(n15p33sims9$ProbSelect[3]), 
         sum(n15p33sims10$ProbSelect[2:4]),
         sum(n15p33sims12$ProbSelect[2:3])))),
  
  # n.ar = 30, placebo.rand.prob = 0.33
  c(0.33, 30, sum(n30p33sims1$ProbSelect[3:5]),
    sum(n30p33sims2$ProbSelect[1] + n30p33sims2$ProbStop),
    sum(n30p33sims3$ProbSelect[3]),
    sum(n30p33sims4$ProbSelect[3:4]),
    sum(n30p33sims5$ProbSelect[1] + n30p33sims5$ProbStop),
    sum(n30p33sims6$ProbSelect[3]),
    sum(n30p33sims7$ProbSelect[3:5]),
    sum(n30p33sims8$ProbSelect[1] + n30p33sims8$ProbStop),
    sum(n30p33sims9$ProbSelect[3]), 
    sum(n30p33sims10$ProbSelect[2:4]),
    sum(n30p33sims11$ProbSelect[1] + n30p33sims11$ProbStop),
    sum(n30p33sims12$ProbSelect[2:3]),
    sum(n30p33sims13$ProbSelect[1] + n30p33sims13$ProbStop),
    sum(n30p33sims14$ProbSelect[1] + n30p33sims14$ProbStop),
    sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop),
    mean(c(sum(n30p33sims1$ProbSelect[3:5]), 
           sum(n30p33sims2$ProbSelect[1] + n30p33sims2$ProbStop),
           sum(n30p33sims3$ProbSelect[3]),
           sum(n30p33sims4$ProbSelect[3:4]),
           sum(n30p33sims5$ProbSelect[1] + n30p33sims5$ProbStop),
           sum(n30p33sims6$ProbSelect[3]),
           sum(n30p33sims7$ProbSelect[3:5]),
           sum(n30p33sims8$ProbSelect[1] + n30p33sims8$ProbStop),
           sum(n30p33sims9$ProbSelect[3]), 
           sum(n30p33sims10$ProbSelect[2:4]),
           sum(n30p33sims11$ProbSelect[1] + n30p33sims11$ProbStop),
           sum(n30p33sims12$ProbSelect[2:3]),
           sum(n30p33sims13$ProbSelect[1] + n30p33sims13$ProbStop),
           sum(n30p33sims14$ProbSelect[1] + n30p33sims14$ProbStop),
           sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop))),
    sd(c(sum(n30p33sims1$ProbSelect[3:5]), 
         sum(n30p33sims2$ProbSelect[1] + n30p33sims2$ProbStop),
         sum(n30p33sims3$ProbSelect[3]),
         sum(n30p33sims4$ProbSelect[3:4]),
         sum(n30p33sims5$ProbSelect[1] + n30p33sims5$ProbStop),
         sum(n30p33sims6$ProbSelect[3]),
         sum(n30p33sims7$ProbSelect[3:5]),
         sum(n30p33sims8$ProbSelect[1] + n30p33sims8$ProbStop),
         sum(n30p33sims9$ProbSelect[3]), 
         sum(n30p33sims10$ProbSelect[2:4]),
         sum(n30p33sims11$ProbSelect[1] + n30p33sims11$ProbStop),
         sum(n30p33sims12$ProbSelect[2:3]),
         sum(n30p33sims13$ProbSelect[1] + n30p33sims13$ProbStop),
         sum(n30p33sims14$ProbSelect[1] + n30p33sims14$ProbStop),
         sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop))),
    mean(c(sum(n30p33sims1$ProbSelect[3:5]), 
           sum(n30p33sims3$ProbSelect[3]),
           sum(n30p33sims4$ProbSelect[3:4]),
           sum(n30p33sims6$ProbSelect[3]),
           sum(n30p33sims7$ProbSelect[3:5]),
           sum(n30p33sims9$ProbSelect[3]), 
           sum(n30p33sims10$ProbSelect[2:4]),
           sum(n30p33sims12$ProbSelect[2:3]))),
    sd(c(sum(n30p33sims1$ProbSelect[3:5]), 
         sum(n30p33sims3$ProbSelect[3]),
         sum(n30p33sims4$ProbSelect[3:4]),
         sum(n30p33sims6$ProbSelect[3]),
         sum(n30p33sims7$ProbSelect[3:5]),
         sum(n30p33sims9$ProbSelect[3]), 
         sum(n30p33sims10$ProbSelect[2:4]),
         sum(n30p33sims12$ProbSelect[2:3])))),
  
  # n.ar = 45, placebo.rand.prob = 0.33
  c(0.33, 45, sum(n45p33sims1$ProbSelect[3:5]),
    sum(n45p33sims2$ProbSelect[1] + n45p33sims2$ProbStop),
    sum(n45p33sims3$ProbSelect[3]),
    sum(n45p33sims4$ProbSelect[3:4]),
    sum(n45p33sims5$ProbSelect[1] + n45p33sims5$ProbStop),
    sum(n45p33sims6$ProbSelect[3]),
    sum(n45p33sims7$ProbSelect[3:5]),
    sum(n45p33sims8$ProbSelect[1] + n45p33sims8$ProbStop),
    sum(n45p33sims9$ProbSelect[3]), 
    sum(n45p33sims10$ProbSelect[2:4]),
    sum(n45p33sims11$ProbSelect[1] + n45p33sims11$ProbStop),
    sum(n45p33sims12$ProbSelect[2:3]),
    sum(n45p33sims13$ProbSelect[1] + n45p33sims13$ProbStop),
    sum(n45p33sims14$ProbSelect[1] + n45p33sims14$ProbStop),
    sum(n45p33sims15$ProbSelect[1] + n45p33sims15$ProbStop),
    mean(c(sum(n45p33sims1$ProbSelect[3:5]), 
           sum(n45p33sims2$ProbSelect[1] + n45p33sims2$ProbStop),
           sum(n45p33sims3$ProbSelect[3]),
           sum(n45p33sims4$ProbSelect[3:4]),
           sum(n45p33sims5$ProbSelect[1] + n45p33sims5$ProbStop),
           sum(n45p33sims6$ProbSelect[3]),
           sum(n45p33sims7$ProbSelect[3:5]),
           sum(n45p33sims8$ProbSelect[1] + n45p33sims8$ProbStop),
           sum(n45p33sims9$ProbSelect[3]), 
           sum(n45p33sims10$ProbSelect[2:4]),
           sum(n45p33sims11$ProbSelect[1] + n45p33sims11$ProbStop),
           sum(n45p33sims12$ProbSelect[2:3]),
           sum(n45p33sims13$ProbSelect[1] + n45p33sims13$ProbStop),
           sum(n45p33sims14$ProbSelect[1] + n45p33sims14$ProbStop),
           sum(n45p33sims15$ProbSelect[1] + n45p33sims15$ProbStop))),
    sd(c(sum(n45p33sims1$ProbSelect[3:5]), 
         sum(n45p33sims2$ProbSelect[1] + n45p33sims2$ProbStop),
         sum(n45p33sims3$ProbSelect[3]),
         sum(n45p33sims4$ProbSelect[3:4]),
         sum(n45p33sims5$ProbSelect[1] + n45p33sims5$ProbStop),
         sum(n45p33sims6$ProbSelect[3]),
         sum(n45p33sims7$ProbSelect[3:5]),
         sum(n45p33sims8$ProbSelect[1] + n45p33sims8$ProbStop),
         sum(n45p33sims9$ProbSelect[3]), 
         sum(n45p33sims10$ProbSelect[2:4]),
         sum(n45p33sims11$ProbSelect[1] + n45p33sims11$ProbStop),
         sum(n45p33sims12$ProbSelect[2:3]),
         sum(n45p33sims13$ProbSelect[1] + n45p33sims13$ProbStop),
         sum(n45p33sims14$ProbSelect[1] + n45p33sims14$ProbStop),
         sum(n45p33sims15$ProbSelect[1] + n45p33sims15$ProbStop))),
    mean(c(sum(n45p33sims1$ProbSelect[3:5]), 
           sum(n45p33sims3$ProbSelect[3]),
           sum(n45p33sims4$ProbSelect[3:4]),
           sum(n45p33sims6$ProbSelect[3]),
           sum(n45p33sims7$ProbSelect[3:5]),
           sum(n45p33sims9$ProbSelect[3]), 
           sum(n45p33sims10$ProbSelect[2:4]),
           sum(n45p33sims12$ProbSelect[2:3]))),
    sd(c(sum(n45p33sims1$ProbSelect[3:5]), 
         sum(n45p33sims3$ProbSelect[3]),
         sum(n45p33sims4$ProbSelect[3:4]),
         sum(n45p33sims6$ProbSelect[3]),
         sum(n45p33sims7$ProbSelect[3:5]),
         sum(n45p33sims9$ProbSelect[3]), 
         sum(n45p33sims10$ProbSelect[2:4]),
         sum(n45p33sims12$ProbSelect[2:3])))),
  
  # n.ar = 60, placebo.rand.prob = 0.33
  c(0.33, 60, sum(n60p33sims1$ProbSelect[3:5]),
    sum(n60p33sims2$ProbSelect[1] + n60p33sims2$ProbStop),
    sum(n60p33sims3$ProbSelect[3]),
    sum(n60p33sims4$ProbSelect[3:4]),
    sum(n60p33sims5$ProbSelect[1] + n60p33sims5$ProbStop),
    sum(n60p33sims6$ProbSelect[3]),
    sum(n60p33sims7$ProbSelect[3:5]),
    sum(n60p33sims8$ProbSelect[1] + n60p33sims8$ProbStop),
    sum(n60p33sims9$ProbSelect[3]), 
    sum(n60p33sims10$ProbSelect[2:4]),
    sum(n60p33sims11$ProbSelect[1] + n60p33sims11$ProbStop),
    sum(n60p33sims12$ProbSelect[2:3]),
    sum(n60p33sims13$ProbSelect[1] + n60p33sims13$ProbStop),
    sum(n60p33sims14$ProbSelect[1] + n60p33sims14$ProbStop),
    sum(n60p33sims15$ProbSelect[1] + n60p33sims15$ProbStop),
    mean(c(sum(n60p33sims1$ProbSelect[3:5]), 
           sum(n60p33sims2$ProbSelect[1] + n60p33sims2$ProbStop),
           sum(n60p33sims3$ProbSelect[3]),
           sum(n60p33sims4$ProbSelect[3:4]),
           sum(n60p33sims5$ProbSelect[1] + n60p33sims5$ProbStop),
           sum(n60p33sims6$ProbSelect[3]),
           sum(n60p33sims7$ProbSelect[3:5]),
           sum(n60p33sims8$ProbSelect[1] + n60p33sims8$ProbStop),
           sum(n60p33sims9$ProbSelect[3]), 
           sum(n60p33sims10$ProbSelect[2:4]),
           sum(n60p33sims11$ProbSelect[1] + n60p33sims11$ProbStop),
           sum(n60p33sims12$ProbSelect[2:3]),
           sum(n60p33sims13$ProbSelect[1] + n60p33sims13$ProbStop),
           sum(n60p33sims14$ProbSelect[1] + n60p33sims14$ProbStop),
           sum(n60p33sims15$ProbSelect[1] + n60p33sims15$ProbStop))),
    sd(c(sum(n60p33sims1$ProbSelect[3:5]), 
         sum(n60p33sims2$ProbSelect[1] + n60p33sims2$ProbStop),
         sum(n60p33sims3$ProbSelect[3]),
         sum(n60p33sims4$ProbSelect[3:4]),
         sum(n60p33sims5$ProbSelect[1] + n60p33sims5$ProbStop),
         sum(n60p33sims6$ProbSelect[3]),
         sum(n60p33sims7$ProbSelect[3:5]),
         sum(n60p33sims8$ProbSelect[1] + n60p33sims8$ProbStop),
         sum(n60p33sims9$ProbSelect[3]), 
         sum(n60p33sims10$ProbSelect[2:4]),
         sum(n60p33sims11$ProbSelect[1] + n60p33sims11$ProbStop),
         sum(n60p33sims12$ProbSelect[2:3]),
         sum(n60p33sims13$ProbSelect[1] + n60p33sims13$ProbStop),
         sum(n60p33sims14$ProbSelect[1] + n60p33sims14$ProbStop),
         sum(n60p33sims15$ProbSelect[1] + n60p33sims15$ProbStop))),
    mean(c(sum(n60p33sims1$ProbSelect[3:5]), 
           sum(n60p33sims3$ProbSelect[3]),
           sum(n60p33sims4$ProbSelect[3:4]),
           sum(n60p33sims6$ProbSelect[3]),
           sum(n60p33sims7$ProbSelect[3:5]),
           sum(n60p33sims9$ProbSelect[3]), 
           sum(n60p33sims10$ProbSelect[2:4]),
           sum(n60p33sims12$ProbSelect[2:3]))),
    sd(c(sum(n60p33sims1$ProbSelect[3:5]), 
         sum(n60p33sims3$ProbSelect[3]),
         sum(n60p33sims4$ProbSelect[3:4]),
         sum(n60p33sims6$ProbSelect[3]),
         sum(n60p33sims7$ProbSelect[3:5]),
         sum(n60p33sims9$ProbSelect[3]), 
         sum(n60p33sims10$ProbSelect[2:4]),
         sum(n60p33sims12$ProbSelect[2:3]))))
) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, round, digits = 2)

colnames(data) <- c('$phi_R$', '$j_R$', '1', '2', '3', '4', '5', '6', '7', '8',
                    '9', '10', '11', '12', '13' , '14', '15', 'Mean', 'StDev', 
                    'Mean', 'StDev')

data %>% 
  kable(format = 'latex', booktabs = T, linesep = '', 
        align = c(rep('c', times = 21)), 
        caption = '\\label{tab_wt:OCsCombosSummary}Probabilities of selecting the OBD and good dose levels for multiple combinations of AR phase size and probabilities for randomisation to control, plus summary statistics.'
  ) %>% 
  kable_styling(position = 'center', latex_options = 'scale_down') %>%
  collapse_rows(columns = 1, latex_hline = "major",  valign = "middle") %>% 
  add_header_above(c(' ' = 2, 
                     'Selection probabilities: Scenarios 1-15' = 15,
                     'All scenarios' = 2, 'Non Stopping' = 2)) %>% 
  pack_rows('Selection probabilities for the OBD', 1, 10) %>% 
  pack_rows('Selection probabilities for good dose-levels', 11, 20) %>% 
  cat()
rm(data)

# Comparison of designs 


# Table for operating characteristics for different combos of AR size and 
# randomisation to control probabilities looking at best/good doses 
# number of patients at those doses and number of controls
load('RProbComboSimsData.RData')
load('AltDesignsSimsData.RData')
data <-
  rbind(
      # scenario 1 
      c(1, 'RtC-WT', 5, '3-5', n30p33sims1$ProbSelect[5],
        sum(n30p33sims1$ProbSelect[3:5]), n30p33sims1$TreatedAtDose[5],
        sum(n30p33sims1$TreatedAtDose[3:5]), n30p33sims1$TreatedAtDose[1]),
      c(1, 'WT', 5, '3-5', WTsims1$ProbSelect[5],
        sum(WTsims1$ProbSelect[3:5]), WTsims1$TreatedAtDose[5],
        sum(WTsims1$TreatedAtDose[3:5]), WTsims1$TreatedAtDose[1]),
      c(1, 'Two-Arm', 5, '3-5', TwoArmsims1$ProbSelect[4],
        sum(TwoArmsims1$ProbSelect[2:4]), TwoArmsims1$TreatedAtDose[4],
        sum(TwoArmsims1$TreatedAtDose[2:4]), 9),
      
      # scenario 2 
      c(2, 'RtC-WT', 'stop', 'stop/1', n30p33sims2$ProbStop, 
        sum(n30p33sims2$ProbSelect[1] + n30p33sims2$ProbStop), '-',
        sum(n30p33sims2$TreatedAtDose[1]), sum(n30p33sims2$TreatedAtDose[1])),
      c(2, 'WT', 'stop', 'stop/1', WTsims2$ProbStop, 
        sum(WTsims2$ProbSelect[1] + WTsims2$ProbStop), '-',
        sum(WTsims2$TreatedAtDose[1]), sum(WTsims2$TreatedAtDose[1])),
      c(2, 'Two-Arm', 'stop', 'stop', TwoArmsims2$ProbStop, 
        TwoArmsims2$ProbStop, '-', '-', 9), 
      
      # scenario 3 
      c(3, 'RtC-WT', 3, '3', n30p33sims3$ProbSelect[3],
        sum(n30p33sims3$ProbSelect[3]), n30p33sims3$TreatedAtDose[3],
        sum(n30p33sims3$TreatedAtDose[3]), n30p33sims3$TreatedAtDose[1]),
      c(3, 'WT', 3, '3', WTsims3$ProbSelect[3],
        sum(WTsims3$ProbSelect[3]), WTsims3$TreatedAtDose[3],
        sum(WTsims3$TreatedAtDose[3]), WTsims3$TreatedAtDose[1]),
      c(3, 'Two-Arm', 3, '3', TwoArmsims3$ProbSelect[2],
        sum(TwoArmsims3$ProbSelect[2]), TwoArmsims3$TreatedAtDose[2],
        sum(TwoArmsims3$TreatedAtDose[2]), 9), 
      
      # scenario 4 
      c(4, 'RtC-WT', 3, '3-4', n30p33sims4$ProbSelect[3],
        sum(n30p33sims4$ProbSelect[3:4]), n30p33sims4$TreatedAtDose[3],
        sum(n30p33sims4$TreatedAtDose[3:4]), n30p33sims4$TreatedAtDose[1]),
      c(4, 'WT', 3, '3-4', WTsims4$ProbSelect[3],
        sum(WTsims4$ProbSelect[3:4]), WTsims4$TreatedAtDose[3],
        sum(WTsims4$TreatedAtDose[3:4]), WTsims4$TreatedAtDose[1]),
      c(4, 'Two-Arm', 3, '3-4', TwoArmsims4$ProbSelect[2],
        sum(TwoArmsims4$ProbSelect[2:3]), TwoArmsims4$TreatedAtDose[2],
        sum(TwoArmsims4$TreatedAtDose[2:3]), 9), 
      
      # scenario 5 
      c(5, 'RtC-WT', 'stop', 'stop/1', n30p33sims5$ProbStop, 
        sum(n30p33sims5$ProbSelect[1] + n30p33sims5$ProbStop), '-',
        sum(n30p33sims5$TreatedAtDose[1]), sum(n30p33sims5$TreatedAtDose[1])),
      c(5, 'WT', 'stop', 'stop/1', WTsims5$ProbStop, 
        sum(WTsims5$ProbSelect[1] + WTsims5$ProbStop), '-',
        sum(WTsims5$TreatedAtDose[1]), sum(WTsims5$TreatedAtDose[1])),
      c(5, 'Two-Arm', 'stop', 'stop', TwoArmsims5$ProbStop, 
        TwoArmsims5$ProbStop, '-', '-', 9), 
      
      # scenario 6 
      c(6, 'RtC-WT', 3, '3', n30p33sims6$ProbSelect[3],
        sum(n30p33sims6$ProbSelect[3]), n30p33sims6$TreatedAtDose[3],
        sum(n30p33sims6$TreatedAtDose[3]), n30p33sims6$TreatedAtDose[1]),
      c(6, 'WT', 3, '3', WTsims6$ProbSelect[3],
        sum(WTsims6$ProbSelect[3]), WTsims6$TreatedAtDose[3],
        sum(WTsims6$TreatedAtDose[3]), WTsims6$TreatedAtDose[1]),
      c(6, 'Two-Arm', 3, '3', TwoArmsims6$ProbSelect[2],
        sum(TwoArmsims6$ProbSelect[2]), TwoArmsims6$TreatedAtDose[2],
        sum(TwoArmsims6$TreatedAtDose[2]), 9), 
      
      # scenario 7 
      c(7, 'RtC-WT', 3, '3-5', n30p33sims7$ProbSelect[3],
        sum(n30p33sims7$ProbSelect[3:5]), n30p33sims7$TreatedAtDose[3],
        sum(n30p33sims7$TreatedAtDose[3:5]), n30p33sims7$TreatedAtDose[1]),
      c(7, 'WT', 3, '3-5', WTsims7$ProbSelect[3],
        sum(WTsims7$ProbSelect[3:5]), WTsims7$TreatedAtDose[3],
        sum(WTsims7$TreatedAtDose[3:5]), WTsims7$TreatedAtDose[1]),
      c(7, 'Two-Arm', 3, '3-5', TwoArmsims7$ProbSelect[2],
        sum(TwoArmsims7$ProbSelect[2:4]), TwoArmsims7$TreatedAtDose[2],
        sum(TwoArmsims7$TreatedAtDose[2:4]), 9), 
      
      # scenario 8 
      c(8, 'RtC-WT', 'stop', 'stop/1', n30p33sims8$ProbStop, 
        sum(n30p33sims8$ProbSelect[1] + n30p33sims8$ProbStop), '-',
        sum(n30p33sims8$TreatedAtDose[1]), sum(n30p33sims8$TreatedAtDose[1])),
      c(8, 'WT', 'stop', 'stop/1', WTsims8$ProbStop, 
        sum(WTsims8$ProbSelect[1] + WTsims8$ProbStop), '-',
        sum(WTsims8$TreatedAtDose[1]), sum(WTsims8$TreatedAtDose[1])),
      c(8, 'Two-Arm', 'stop', 'stop', TwoArmsims8$ProbStop, 
        TwoArmsims8$ProbStop, '-', '-', 9), 
      
      # scenario 9 
      c(9, 'RtC-WT', 3, '3', n30p33sims9$ProbSelect[3],
        sum(n30p33sims9$ProbSelect[3]), n30p33sims9$TreatedAtDose[3],
        sum(n30p33sims9$TreatedAtDose[3]), n30p33sims9$TreatedAtDose[1]),
      c(9, 'WT', 3, '3', WTsims9$ProbSelect[3],
        sum(WTsims9$ProbSelect[3]), WTsims9$TreatedAtDose[3],
        sum(WTsims9$TreatedAtDose[3]), WTsims9$TreatedAtDose[1]),
      c(9, 'Two-Arm', 3, '3', TwoArmsims9$ProbSelect[2],
        sum(TwoArmsims9$ProbSelect[2]), TwoArmsims9$TreatedAtDose[2],
        sum(TwoArmsims9$TreatedAtDose[2]), 9), 
      
      # scenario 10
      c(10, 'RtC-WT', 2, '2-4', n30p33sims10$ProbSelect[2],
        sum(n30p33sims10$ProbSelect[2:4]), n30p33sims10$TreatedAtDose[2],
        sum(n30p33sims10$TreatedAtDose[2:4]), n30p33sims10$TreatedAtDose[1]),
      c(10, 'WT', 2, '2-4', WTsims10$ProbSelect[2],
        sum(WTsims10$ProbSelect[2:4]), WTsims10$TreatedAtDose[2],
        sum(WTsims10$TreatedAtDose[2:4]), WTsims10$TreatedAtDose[1]),
      c(10, 'Two-Arm', 2, '2-4', TwoArmsims10$ProbSelect[1],
        sum(TwoArmsims10$ProbSelect[1:3]), TwoArmsims10$TreatedAtDose[1],
        sum(TwoArmsims10$TreatedAtDose[1:3]), 9), 
      
      # scenario 11 
      c(11, 'RtC-WT', 'stop', 'stop/1', n30p33sims11$ProbStop, 
        sum(n30p33sims11$ProbSelect[1] + n30p33sims11$ProbStop), '-',
        sum(n30p33sims11$TreatedAtDose[1]), sum(n30p33sims11$TreatedAtDose[1])),
      c(11, 'WT', 'stop', 'stop/1', WTsims11$ProbStop, 
        sum(WTsims11$ProbSelect[1] + WTsims11$ProbStop), '-',
        sum(WTsims11$TreatedAtDose[1]), sum(WTsims11$TreatedAtDose[1])),
      c(11, 'Two-Arm', 'stop', 'stop', TwoArmsims11$ProbStop, 
        TwoArmsims11$ProbStop, '-', '-', 9),
      
      # scenario 12
      c(12, 'RtC-WT', 2, '2-3', n30p33sims12$ProbSelect[2],
        sum(n30p33sims12$ProbSelect[2:3]), n30p33sims12$TreatedAtDose[2],
        sum(n30p33sims12$TreatedAtDose[2:3]), n30p33sims12$TreatedAtDose[1]),
      c(12, 'WT', 2, '2-3', WTsims12$ProbSelect[2],
        sum(WTsims12$ProbSelect[2:3]), WTsims12$TreatedAtDose[2],
        sum(WTsims12$TreatedAtDose[2:3]), WTsims12$TreatedAtDose[1]),
      c(12, 'Two-Arm', 2, '2-3', TwoArmsims12$ProbSelect[1],
        sum(TwoArmsims12$ProbSelect[1:2]), TwoArmsims12$TreatedAtDose[1],
        sum(TwoArmsims12$TreatedAtDose[1:2]), 9),
      
      # scenario 13
      c(13, 'RtC-WT', 'stop', 'stop/1', n30p33sims13$ProbStop, 
        sum(n30p33sims13$ProbSelect[1] + n30p33sims13$ProbStop), '-',
        sum(n30p33sims13$TreatedAtDose[1]), sum(n30p33sims13$TreatedAtDose[1])),
      c(13, 'WT', 'stop', 'stop/1', WTsims13$ProbStop, 
        sum(WTsims13$ProbSelect[1] + WTsims13$ProbStop), '-',
        sum(WTsims13$TreatedAtDose[1]), sum(WTsims13$TreatedAtDose[1])),
      c(13, 'Two-Arm', 'stop', 'stop', TwoArmsims13$ProbStop, 
        TwoArmsims13$ProbStop, '-', '-', 9), 
      
      # scenario 14
      c(14, 'RtC-WT', 'stop', 'stop/1', n30p33sims14$ProbStop, 
        sum(n30p33sims14$ProbSelect[1] + n30p33sims14$ProbStop), '-',
        sum(n30p33sims14$TreatedAtDose[1]), sum(n30p33sims14$TreatedAtDose[1])),
      c(14, 'WT', 'stop', 'stop/1', WTsims14$ProbStop, 
        sum(WTsims14$ProbSelect[1] + WTsims14$ProbStop), '-',
        sum(WTsims14$TreatedAtDose[1]), sum(WTsims14$TreatedAtDose[1])),
      c(14, 'Two-Arm', 'stop', 'stop', TwoArmsims14$ProbStop, 
        TwoArmsims14$ProbStop, '-', '-', 9),
      
      # scenario 15
      c(15, 'RtC-WT', 'stop', 'stop/1', n30p33sims15$ProbStop, 
        sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop), '-',
        sum(n30p33sims15$TreatedAtDose[1]), sum(n30p33sims15$TreatedAtDose[1])),
      c(15, 'WT', 'stop', 'stop/1', WTsims15$ProbStop, 
        sum(WTsims15$ProbSelect[1] + WTsims15$ProbStop), '-',
        sum(WTsims15$TreatedAtDose[1]), sum(WTsims15$TreatedAtDose[1])),
      c(15, 'Two-Arm', 'stop', 'stop', TwoArmsims15$ProbStop, 
        TwoArmsims15$ProbStop, '-', '-', 9)
    ) %>% 
  as.data.frame()

data$V5 <- as.numeric(data$V5)
data$V6 <- as.numeric(data$V6)

data %>% 
  mutate(V5 = round(V5, 2),
         V6 = round(V6, 2)) -> data


colnames(data) <- c('Scenario', 'Design', 'Best Dose Level',
                    'Good Dose Levels', 'Prob of Picking Best Dose', 
                    'Prob of Picking Good Doses', 'Mean N at Best Dose', 
                    'Mean N at Good Doses', 'Mean N at 1 (Control Dose)')

colnames(data) <- c('Scenario', 'Design', 'Best Dose',
                    'Good Doses', 'P(Best)', 'P(Good)', 'N(Best)', 
                    'N(Good)', 'N(Control)')

data %>% 
  kable(format = 'latex', linesep = '', longtable = T, booktabs = T, 
        align = c(rep('c', times = 9)), 
        caption = '\\label{tab_wt:OCsDesigns-to-compare}Operating characteristics for alternative designs. Probability of selecting the best or good dose levels as the OBD, mean number of patients treated at those dose levels and at the control dose after 10000 simulations.'
  ) %>% 
  kable_styling(position = 'center', repeat_header_method = 'replace',
                latex_options = c('repeat_header', 'HOLD_position'), font_size = 9,
                repeat_header_text = "Operating characteristics (continued)") %>%
  # column_spec(2:3, width = '0.5cm') %>% 
  # column_spec(c(4,5,6,7,8,9,10), width = '1.5cm') %>%
  #olumn_spec(7 , width = '2.3cm') %>% 
  collapse_rows(columns = 1:2, latex_hline = "major",  valign = "middle") %>% 
  #landscape() %>% 
  cat() 

# Table for operating characteristics for different designs looking at summary statistics. 
data <- rbind(
  c(n30p33sims1$ProbSelect[5], n30p33sims2$ProbStop,
    n30p33sims3$ProbSelect[3], n30p33sims4$ProbSelect[3], n30p33sims5$ProbStop,
    n30p33sims6$ProbSelect[3], n30p33sims7$ProbSelect[3], n30p33sims8$ProbStop, 
    n30p33sims9$ProbSelect[3], n30p33sims10$ProbSelect[2], n30p33sims11$ProbStop,
    n30p33sims12$ProbSelect[2], n30p33sims13$ProbStop, n30p33sims14$ProbStop, 
    n30p33sims15$ProbStop, 
    mean(c(n30p33sims1$ProbSelect[5], n30p33sims2$ProbStop,
           n30p33sims3$ProbSelect[3], n30p33sims4$ProbSelect[3], n30p33sims5$ProbStop,
           n30p33sims6$ProbSelect[3], n30p33sims7$ProbSelect[3], n30p33sims8$ProbStop, 
           n30p33sims9$ProbSelect[3], n30p33sims10$ProbSelect[2], n30p33sims11$ProbStop,
           n30p33sims12$ProbSelect[2], n30p33sims13$ProbStop, n30p33sims14$ProbStop, 
           n30p33sims15$ProbStop)),
    sd(c(n30p33sims1$ProbSelect[5], n30p33sims2$ProbStop,
         n30p33sims3$ProbSelect[3], n30p33sims4$ProbSelect[3], n30p33sims5$ProbStop,
         n30p33sims6$ProbSelect[3], n30p33sims7$ProbSelect[3], n30p33sims8$ProbStop, 
         n30p33sims9$ProbSelect[3], n30p33sims10$ProbSelect[2], n30p33sims11$ProbStop,
         n30p33sims12$ProbSelect[2], n30p33sims13$ProbStop, n30p33sims14$ProbStop, 
         n30p33sims15$ProbStop)),
    mean(c(n30p33sims1$ProbSelect[5], n30p33sims3$ProbSelect[3], 
           n30p33sims4$ProbSelect[3], n30p33sims6$ProbSelect[3], 
           n30p33sims7$ProbSelect[3], n30p33sims9$ProbSelect[3], 
           n30p33sims10$ProbSelect[2], n30p33sims12$ProbSelect[2])),
    sd(c(n30p33sims1$ProbSelect[5], n30p33sims3$ProbSelect[3], 
         n30p33sims4$ProbSelect[3], n30p33sims6$ProbSelect[3], 
         n30p33sims7$ProbSelect[3], n30p33sims9$ProbSelect[3], 
         n30p33sims10$ProbSelect[2], n30p33sims12$ProbSelect[2]))),
  
  c(WTsims1$ProbSelect[5], WTsims2$ProbStop,
    WTsims3$ProbSelect[3], WTsims4$ProbSelect[3], WTsims5$ProbStop,
    WTsims6$ProbSelect[3], WTsims7$ProbSelect[3], WTsims8$ProbStop, 
    WTsims9$ProbSelect[3], WTsims10$ProbSelect[2], WTsims11$ProbStop,
    WTsims12$ProbSelect[2], WTsims13$ProbStop, WTsims14$ProbStop, 
    WTsims15$ProbStop, 
    mean(c(WTsims1$ProbSelect[5], WTsims2$ProbStop,
           WTsims3$ProbSelect[3], WTsims4$ProbSelect[3], WTsims5$ProbStop,
           WTsims6$ProbSelect[3], WTsims7$ProbSelect[3], WTsims8$ProbStop, 
           WTsims9$ProbSelect[3], WTsims10$ProbSelect[2], WTsims11$ProbStop,
           WTsims12$ProbSelect[2], WTsims13$ProbStop, WTsims14$ProbStop, 
           WTsims15$ProbStop)),
    sd(c(WTsims1$ProbSelect[5], WTsims2$ProbStop,
         WTsims3$ProbSelect[3], WTsims4$ProbSelect[3], WTsims5$ProbStop,
         WTsims6$ProbSelect[3], WTsims7$ProbSelect[3], WTsims8$ProbStop, 
         WTsims9$ProbSelect[3], WTsims10$ProbSelect[2], WTsims11$ProbStop,
         WTsims12$ProbSelect[2], WTsims13$ProbStop, WTsims14$ProbStop, 
         WTsims15$ProbStop)),
    mean(c(WTsims1$ProbSelect[5], WTsims3$ProbSelect[3], 
           WTsims4$ProbSelect[3], WTsims6$ProbSelect[3], 
           WTsims7$ProbSelect[3], WTsims9$ProbSelect[3], 
           WTsims10$ProbSelect[2], WTsims12$ProbSelect[2])),
    sd(c(WTsims1$ProbSelect[5], WTsims3$ProbSelect[3], 
         WTsims4$ProbSelect[3], WTsims6$ProbSelect[3], 
         WTsims7$ProbSelect[3], WTsims9$ProbSelect[3], 
         WTsims10$ProbSelect[2], WTsims12$ProbSelect[2]))), 
  
  c(TwoArmsims1$ProbSelect[4], TwoArmsims2$ProbStop,
    TwoArmsims3$ProbSelect[2], TwoArmsims4$ProbSelect[2], TwoArmsims5$ProbStop,
    TwoArmsims6$ProbSelect[2], TwoArmsims7$ProbSelect[2], TwoArmsims8$ProbStop, 
    TwoArmsims9$ProbSelect[2], TwoArmsims10$ProbSelect[1], TwoArmsims11$ProbStop,
    TwoArmsims12$ProbSelect[1], TwoArmsims13$ProbStop, TwoArmsims14$ProbStop, 
    TwoArmsims15$ProbStop, 
    mean(c(TwoArmsims1$ProbSelect[4], TwoArmsims2$ProbStop,
           TwoArmsims3$ProbSelect[2], TwoArmsims4$ProbSelect[2], TwoArmsims5$ProbStop,
           TwoArmsims6$ProbSelect[2], TwoArmsims7$ProbSelect[2], TwoArmsims8$ProbStop, 
           TwoArmsims9$ProbSelect[2], TwoArmsims10$ProbSelect[1], TwoArmsims11$ProbStop,
           TwoArmsims12$ProbSelect[1], TwoArmsims13$ProbStop, TwoArmsims14$ProbStop, 
           TwoArmsims15$ProbStop)),
    sd(c(TwoArmsims1$ProbSelect[4], TwoArmsims2$ProbStop,
         TwoArmsims3$ProbSelect[2], TwoArmsims4$ProbSelect[2], TwoArmsims5$ProbStop,
         TwoArmsims6$ProbSelect[2], TwoArmsims7$ProbSelect[2], TwoArmsims8$ProbStop, 
         TwoArmsims9$ProbSelect[2], TwoArmsims10$ProbSelect[1], TwoArmsims11$ProbStop,
         TwoArmsims12$ProbSelect[1], TwoArmsims13$ProbStop, TwoArmsims14$ProbStop, 
         TwoArmsims15$ProbStop)),
    mean(c(TwoArmsims1$ProbSelect[4], TwoArmsims3$ProbSelect[2], 
           TwoArmsims4$ProbSelect[2], TwoArmsims6$ProbSelect[2], 
           TwoArmsims7$ProbSelect[2], TwoArmsims9$ProbSelect[2], 
           TwoArmsims10$ProbSelect[1], TwoArmsims12$ProbSelect[1])),
    sd(c(TwoArmsims1$ProbSelect[4], TwoArmsims3$ProbSelect[2], 
         TwoArmsims4$ProbSelect[2], TwoArmsims6$ProbSelect[2], 
         TwoArmsims7$ProbSelect[2], TwoArmsims9$ProbSelect[2], 
         TwoArmsims10$ProbSelect[1], TwoArmsims12$ProbSelect[1]))), 
  
  c(sum(n30p33sims1$ProbSelect[3:5]),
    sum(n30p33sims2$ProbSelect[1] + n30p33sims2$ProbStop),
    sum(n30p33sims3$ProbSelect[3]),
    sum(n30p33sims4$ProbSelect[3:4]),
    sum(n30p33sims5$ProbSelect[1] + n30p33sims5$ProbStop),
    sum(n30p33sims6$ProbSelect[3]),
    sum(n30p33sims7$ProbSelect[3:5]),
    sum(n30p33sims8$ProbSelect[1] + n30p33sims8$ProbStop),
    sum(n30p33sims9$ProbSelect[3]), 
    sum(n30p33sims10$ProbSelect[2:4]),
    sum(n30p33sims11$ProbSelect[1] + n30p33sims11$ProbStop),
    sum(n30p33sims12$ProbSelect[2:3]),
    sum(n30p33sims13$ProbSelect[1] + n30p33sims13$ProbStop),
    sum(n30p33sims14$ProbSelect[1] + n30p33sims14$ProbStop),
    sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop),
    mean(c(sum(n30p33sims1$ProbSelect[3:5]), 
           sum(n30p33sims2$ProbSelect[1] + n30p33sims2$ProbStop),
           sum(n30p33sims3$ProbSelect[3]),
           sum(n30p33sims4$ProbSelect[3:4]),
           sum(n30p33sims5$ProbSelect[1] + n30p33sims5$ProbStop),
           sum(n30p33sims6$ProbSelect[3]),
           sum(n30p33sims7$ProbSelect[3:5]),
           sum(n30p33sims8$ProbSelect[1] + n30p33sims8$ProbStop),
           sum(n30p33sims9$ProbSelect[3]), 
           sum(n30p33sims10$ProbSelect[2:4]),
           sum(n30p33sims11$ProbSelect[1] + n30p33sims11$ProbStop),
           sum(n30p33sims12$ProbSelect[2:3]),
           sum(n30p33sims13$ProbSelect[1] + n30p33sims13$ProbStop),
           sum(n30p33sims14$ProbSelect[1] + n30p33sims14$ProbStop),
           sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop))),
    sd(c(sum(n30p33sims1$ProbSelect[3:5]), 
         sum(n30p33sims2$ProbSelect[1] + n30p33sims2$ProbStop),
         sum(n30p33sims3$ProbSelect[3]),
         sum(n30p33sims4$ProbSelect[3:4]),
         sum(n30p33sims5$ProbSelect[1] + n30p33sims5$ProbStop),
         sum(n30p33sims6$ProbSelect[3]),
         sum(n30p33sims7$ProbSelect[3:5]),
         sum(n30p33sims8$ProbSelect[1] + n30p33sims8$ProbStop),
         sum(n30p33sims9$ProbSelect[3]), 
         sum(n30p33sims10$ProbSelect[2:4]),
         sum(n30p33sims11$ProbSelect[1] + n30p33sims11$ProbStop),
         sum(n30p33sims12$ProbSelect[2:3]),
         sum(n30p33sims13$ProbSelect[1] + n30p33sims13$ProbStop),
         sum(n30p33sims14$ProbSelect[1] + n30p33sims14$ProbStop),
         sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop))),
    mean(c(sum(n30p33sims1$ProbSelect[3:5]), 
           sum(n30p33sims3$ProbSelect[3]),
           sum(n30p33sims4$ProbSelect[3:4]),
           sum(n30p33sims6$ProbSelect[3]),
           sum(n30p33sims7$ProbSelect[3:5]),
           sum(n30p33sims9$ProbSelect[3]), 
           sum(n30p33sims10$ProbSelect[2:4]),
           sum(n30p33sims12$ProbSelect[2:3]))),
    sd(c(sum(n30p33sims1$ProbSelect[3:5]), 
         sum(n30p33sims3$ProbSelect[3]),
         sum(n30p33sims4$ProbSelect[3:4]),
         sum(n30p33sims6$ProbSelect[3]),
         sum(n30p33sims7$ProbSelect[3:5]),
         sum(n30p33sims9$ProbSelect[3]), 
         sum(n30p33sims10$ProbSelect[2:4]),
         sum(n30p33sims12$ProbSelect[2:3])))),
  
  c(sum(WTsims1$ProbSelect[3:5]),
    sum(WTsims2$ProbSelect[1] + WTsims2$ProbStop),
    sum(WTsims3$ProbSelect[3]),
    sum(WTsims4$ProbSelect[3:4]),
    sum(WTsims5$ProbSelect[1] + WTsims5$ProbStop),
    sum(WTsims6$ProbSelect[3]),
    sum(WTsims7$ProbSelect[3:5]),
    sum(WTsims8$ProbSelect[1] + WTsims8$ProbStop),
    sum(WTsims9$ProbSelect[3]), 
    sum(WTsims10$ProbSelect[2:4]),
    sum(WTsims11$ProbSelect[1] + WTsims11$ProbStop),
    sum(WTsims12$ProbSelect[2:3]),
    sum(WTsims13$ProbSelect[1] + WTsims13$ProbStop),
    sum(WTsims14$ProbSelect[1] + WTsims14$ProbStop),
    sum(WTsims15$ProbSelect[1] + WTsims15$ProbStop),
    mean(c(sum(WTsims1$ProbSelect[3:5]), 
           sum(WTsims2$ProbSelect[1] + WTsims2$ProbStop),
           sum(WTsims3$ProbSelect[3]),
           sum(WTsims4$ProbSelect[3:4]),
           sum(WTsims5$ProbSelect[1] + WTsims5$ProbStop),
           sum(WTsims6$ProbSelect[3]),
           sum(WTsims7$ProbSelect[3:5]),
           sum(WTsims8$ProbSelect[1] + WTsims8$ProbStop),
           sum(WTsims9$ProbSelect[3]), 
           sum(WTsims10$ProbSelect[2:4]),
           sum(WTsims11$ProbSelect[1] + WTsims11$ProbStop),
           sum(WTsims12$ProbSelect[2:3]),
           sum(WTsims13$ProbSelect[1] + WTsims13$ProbStop),
           sum(WTsims14$ProbSelect[1] + WTsims14$ProbStop),
           sum(WTsims15$ProbSelect[1] + WTsims15$ProbStop))),
    sd(c(sum(WTsims1$ProbSelect[3:5]), 
         sum(WTsims2$ProbSelect[1] + WTsims2$ProbStop),
         sum(WTsims3$ProbSelect[3]),
         sum(WTsims4$ProbSelect[3:4]),
         sum(WTsims5$ProbSelect[1] + WTsims5$ProbStop),
         sum(WTsims6$ProbSelect[3]),
         sum(WTsims7$ProbSelect[3:5]),
         sum(WTsims8$ProbSelect[1] + WTsims8$ProbStop),
         sum(WTsims9$ProbSelect[3]), 
         sum(WTsims10$ProbSelect[2:4]),
         sum(WTsims11$ProbSelect[1] + WTsims11$ProbStop),
         sum(WTsims12$ProbSelect[2:3]),
         sum(WTsims13$ProbSelect[1] + WTsims13$ProbStop),
         sum(WTsims14$ProbSelect[1] + WTsims14$ProbStop),
         sum(WTsims15$ProbSelect[1] + WTsims15$ProbStop))),
    mean(c(sum(WTsims1$ProbSelect[3:5]), 
           sum(WTsims3$ProbSelect[3]),
           sum(WTsims4$ProbSelect[3:4]),
           sum(WTsims6$ProbSelect[3]),
           sum(WTsims7$ProbSelect[3:5]),
           sum(WTsims9$ProbSelect[3]), 
           sum(WTsims10$ProbSelect[2:4]),
           sum(WTsims12$ProbSelect[2:3]))),
    sd(c(sum(WTsims1$ProbSelect[3:5]), 
         sum(WTsims3$ProbSelect[3]),
         sum(WTsims4$ProbSelect[3:4]),
         sum(WTsims6$ProbSelect[3]),
         sum(WTsims7$ProbSelect[3:5]),
         sum(WTsims9$ProbSelect[3]), 
         sum(WTsims10$ProbSelect[2:4]),
         sum(WTsims12$ProbSelect[2:3])))), 
  
  c(sum(TwoArmsims1$ProbSelect[2:4]),
    sum(TwoArmsims2$ProbStop),
    sum(TwoArmsims3$ProbSelect[2]),
    sum(TwoArmsims4$ProbSelect[2:3]),
    sum(TwoArmsims5$ProbStop),
    sum(TwoArmsims6$ProbSelect[2]),
    sum(TwoArmsims7$ProbSelect[2:4]),
    sum(TwoArmsims8$ProbStop),
    sum(TwoArmsims9$ProbSelect[2]), 
    sum(TwoArmsims10$ProbSelect[1:3]),
    sum(TwoArmsims11$ProbStop),
    sum(TwoArmsims12$ProbSelect[1:2]),
    sum(TwoArmsims13$ProbStop),
    sum(TwoArmsims14$ProbStop),
    sum(TwoArmsims15$ProbStop),
    mean(c(sum(TwoArmsims1$ProbSelect[2:4]), 
           sum(TwoArmsims2$ProbStop),
           sum(TwoArmsims3$ProbSelect[2]),
           sum(TwoArmsims4$ProbSelect[2:3]),
           sum(TwoArmsims5$ProbStop),
           sum(TwoArmsims6$ProbSelect[2]),
           sum(TwoArmsims7$ProbSelect[2:4]),
           sum(TwoArmsims8$ProbStop),
           sum(TwoArmsims9$ProbSelect[2]), 
           sum(TwoArmsims10$ProbSelect[1:3]),
           sum(TwoArmsims11$ProbStop),
           sum(TwoArmsims12$ProbSelect[1:2]),
           sum(TwoArmsims13$ProbStop),
           sum(TwoArmsims14$ProbStop),
           sum(TwoArmsims15$ProbStop))),
    sd(c(sum(TwoArmsims1$ProbSelect[2:4]), 
         sum(TwoArmsims2$ProbStop),
         sum(TwoArmsims3$ProbSelect[2]),
         sum(TwoArmsims4$ProbSelect[2:3]),
         sum(TwoArmsims5$ProbStop),
         sum(TwoArmsims6$ProbSelect[2]),
         sum(TwoArmsims7$ProbSelect[2:4]),
         sum(TwoArmsims8$ProbStop),
         sum(TwoArmsims9$ProbSelect[2]), 
         sum(TwoArmsims10$ProbSelect[1:3]),
         sum(TwoArmsims11$ProbStop),
         sum(TwoArmsims12$ProbSelect[1:2]),
         sum(TwoArmsims13$ProbStop),
         sum(TwoArmsims14$ProbStop),
         sum(TwoArmsims15$ProbStop))),
    mean(c(sum(TwoArmsims1$ProbSelect[2:4]), 
           sum(TwoArmsims3$ProbSelect[2]),
           sum(TwoArmsims4$ProbSelect[2:3]),
           sum(TwoArmsims6$ProbSelect[2]),
           sum(TwoArmsims7$ProbSelect[2:4]),
           sum(TwoArmsims9$ProbSelect[2]), 
           sum(TwoArmsims10$ProbSelect[1:3]),
           sum(TwoArmsims12$ProbSelect[1:2]))),
    sd(c(sum(TwoArmsims1$ProbSelect[2:4]), 
         sum(TwoArmsims3$ProbSelect[2]),
         sum(TwoArmsims4$ProbSelect[2:3]),
         sum(TwoArmsims6$ProbSelect[2]),
         sum(TwoArmsims7$ProbSelect[2:4]),
         sum(TwoArmsims9$ProbSelect[2]), 
         sum(TwoArmsims10$ProbSelect[1:3]),
         sum(TwoArmsims12$ProbSelect[1:2]))))
) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(Design = c('RtC-WT', 'WT', 'Two-Arm',' RtC-WT', 'WT', 'Two-Arm')) %>% 
  select(Design, everything())

colnames(data) <- c('Design', '1', '2', '3', '4', '5', '6', '7', '8',
                    '9', '10', '11', '12', '13' , '14', '15', 'Mean', 'StDev',
                    'Mean', 'StDev')

data %>% 
  kable(format = 'latex', booktabs = T, linesep = '', 
        align = c(rep('c', times = 20)), 
        caption = '\\label{tab_wt:OCsDesigns-to-compare-Summary}Probabilities of selecting the OBD and good dose levels for multiple designs, plus summary statistics.'
  ) %>% 
  kable_styling(position = 'center', latex_options = 'scale_down') %>%
  add_header_above(c(' ' = 1, 
                     'Selection probabilities: Scenarios 1-15' = 15,
                     'All scenarios' = 2, 'Non Stopping' = 2)) %>% 
  pack_rows('Selection probabilities for the OBD', 1, 3) %>% 
  pack_rows('Selection probabilities for good dose-levels', 4, 6) %>% 
  cat()


# Power Calculations 

# Table for power calculation summary
load("PowerSimsData.RData")
data <- rbind(
  c(pwrsims1$PowerMean[1], pwrsims1$PowerMean[2], pwrsims1$PowerMean[3], 
    pwrsims1$PowerMean[4], pwrsims1$NPower, pwrsims1$PowerSD[1],
    pwrsims1$PowerSD[2], pwrsims1$PowerSD[3], pwrsims1$PowerSD[4]), 
  
  c(pwrsims3$PowerMean[1], pwrsims3$PowerMean[2], pwrsims3$PowerMean[3], 
    pwrsims3$PowerMean[4], pwrsims3$NPower, pwrsims3$PowerSD[1], 
    pwrsims3$PowerSD[2], pwrsims3$PowerSD[3], pwrsims3$PowerSD[4]),

  c(pwrsims4$PowerMean[1], pwrsims4$PowerMean[2], pwrsims4$PowerMean[3], 
    pwrsims4$PowerMean[4],pwrsims4$NPower, pwrsims4$PowerSD[1], 
    pwrsims4$PowerSD[2], pwrsims4$PowerSD[3], pwrsims4$PowerSD[4]),

  c(pwrsims6$PowerMean[1], pwrsims6$PowerMean[2], pwrsims6$PowerMean[3], 
    pwrsims6$PowerMean[4], pwrsims6$NPower, pwrsims6$PowerSD[1], 
    pwrsims6$PowerSD[2], pwrsims6$PowerSD[3], pwrsims6$PowerSD[4]),

  c(pwrsims7$PowerMean[1], pwrsims7$PowerMean[2], pwrsims7$PowerMean[3], 
    pwrsims7$PowerMean[4], pwrsims7$NPower, pwrsims7$PowerSD[1], 
    pwrsims7$PowerSD[2], pwrsims7$PowerSD[3], pwrsims7$PowerSD[4]),

  c(pwrsims9$PowerMean[1], pwrsims9$PowerMean[2], pwrsims9$PowerMean[3], 
    pwrsims9$PowerMean[4], pwrsims9$NPower, pwrsims9$PowerSD[1], 
    pwrsims9$PowerSD[2], pwrsims9$PowerSD[3], pwrsims9$PowerSD[4]),

  c(pwrsims10$PowerMean[1], pwrsims10$PowerMean[2], pwrsims10$PowerMean[3], 
    pwrsims10$PowerMean[4], pwrsims10$NPower, pwrsims10$PowerSD[1],
    pwrsims10$PowerSD[2], pwrsims10$PowerSD[3], pwrsims10$PowerSD[4]),

  c(pwrsims12$PowerMean[1], pwrsims12$PowerMean[2], pwrsims12$PowerMean[3],
    pwrsims12$PowerMean[4], pwrsims12$NPower, pwrsims12$PowerSD[1], 
    pwrsims12$PowerSD[2], pwrsims12$PowerSD[3], pwrsims12$PowerSD[4])
) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(Scenario = c('1', '3', '4',' 6', '7', '9', '10', '12'),
         '0.2' = paste0(X1, ' (', X6, ')', sep = ''),
         '0.41' = paste0(X2, ' (', X7, ')', sep = ''),
         '0.5' = paste0(X3, ' (', X8, ')', sep = ''),
         '0.8' = paste0(X4, ' (', X9, ')', sep = ''),
         N = X5 ) %>% 
  select(Scenario, '0.2', '0.41', '0.5', '0.8', N)

data %>% 
  kable(format = 'latex', booktabs = T, linesep = '', 
        align = c('l', rep('c', times = 5)), 
        caption = '\\label{tab_wt:Power-Calcs}Power calculations for various effect sizes based on simulation results.'
  ) %>% 
  kable_styling(position = 'center') %>%
  add_header_above(c(' ' = 1, 
                     'Effect Sizes' = 4,
                     ' ' = 1)) %>% 
  cat()
