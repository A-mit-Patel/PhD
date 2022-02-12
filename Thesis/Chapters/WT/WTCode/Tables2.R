library(dplyr)
library(tibble)
library(kableExtra)

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
      sum(n15p33sims3$ProbSelect[3]), n15p33sims3$TreatedAtDose[5],
      sum(n15p33sims3$TreatedAtDose[3]), n15p33sims3$TreatedAtDose[1]),
    c(3, 0.33, 30, 3, '3', n30p33sims3$ProbSelect[3], #n.ar = 30
      sum(n30p33sims3$ProbSelect[3]), n30p33sims3$TreatedAtDose[5],
      sum(n30p33sims3$TreatedAtDose[3]), n30p33sims3$TreatedAtDose[1]),
    c(3, 0.33, 45, 3, '3', n45p33sims3$ProbSelect[3], #n.ar = 45
      sum(n45p33sims3$ProbSelect[3]), n45p33sims3$TreatedAtDose[5],
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
      sum(n15p33sims6$ProbSelect[3]), n15p33sims6$TreatedAtDose[5],
      sum(n15p33sims6$TreatedAtDose[3]), n15p33sims6$TreatedAtDose[1]),
    c(6, 0.33, 30, 3, '3', n30p33sims6$ProbSelect[3], #n.ar = 30
      sum(n30p33sims6$ProbSelect[3]), n30p33sims6$TreatedAtDose[5],
      sum(n30p33sims6$TreatedAtDose[3]), n30p33sims6$TreatedAtDose[1]),
    c(6, 0.33, 45, 3, '3', n45p33sims6$ProbSelect[3], #n.ar = 45
      sum(n45p33sims6$ProbSelect[3]), n45p33sims6$TreatedAtDose[5],
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
      sum(n15p33sims9$ProbSelect[3]), n15p33sims9$TreatedAtDose[5],
      sum(n15p33sims9$TreatedAtDose[3]), n15p33sims9$TreatedAtDose[1]),
    c(9, 0.33, 30, 3, '3', n30p33sims9$ProbSelect[3], #n.ar = 30
      sum(n30p33sims9$ProbSelect[3]), n30p33sims9$TreatedAtDose[5],
      sum(n30p33sims9$TreatedAtDose[3]), n30p33sims9$TreatedAtDose[1]),
    c(9, 0.33, 45, 3, '3', n45p33sims9$ProbSelect[3], #n.ar = 45
      sum(n45p33sims9$ProbSelect[3]), n45p33sims9$TreatedAtDose[5],
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
         n0p20sims15$ProbStop))),
  
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
         n15p20sims15$ProbStop))), 
  
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
         n30p20sims15$ProbStop))), 
  
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
         n45p20sims15$ProbStop))),
  
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
         n60p20sims15$ProbStop))),
  
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
         n0p33sims15$ProbStop))),
  
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
         n15p33sims15$ProbStop))), 
  
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
         n30p33sims15$ProbStop))), 
  
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
         n45p33sims15$ProbStop))),
  
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
         n60p33sims15$ProbStop)))
) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, round, digits = 2)


colnames(data) <- c('$phi_R$', '$j_R$', '1', '2', '3', '4', '5', '6', '7', '8',
                    '9', '10', '11', '12', '13' , '14', '15', 'Mean', 'StDev')

data %>% 
  kable(format = 'latex', booktabs = T, linesep = '', 
        align = c(rep('c', times = 19)), 
        caption = '\\label{tab_wt:OCsCombosSummaryBest}Probabilities of selecting the best dose level for multiple combinations of AR phase size and probabilities for randomisation to control, plus summary statistics.'
  ) %>% 
  kable_styling(position = 'center', latex_options = 'scale_down') %>%
  collapse_rows(columns = 1, latex_hline = "major",  valign = "middle") %>% 
  add_header_above(c(' ' = 2, 
                     'Probability of selecting the best dose level: Scenarios 1-15' = 15,
                     'All scenarios' = 2)) %>% 
  cat()

rm(data)

data <- rbind(
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
         sum(n0p20sims15$ProbSelect[1] + n0p20sims15$ProbStop)))),
  
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
         sum(n15p20sims15$ProbSelect[1] + n15p20sims15$ProbStop)))),
  
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
         sum(n30p20sims15$ProbSelect[1] + n30p20sims15$ProbStop)))),
  
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
         sum(n45p20sims15$ProbSelect[1] + n45p20sims15$ProbStop)))),
  
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
         sum(n60p20sims15$ProbSelect[1] + n60p20sims15$ProbStop)))),
  
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
         sum(n0p33sims15$ProbSelect[1] + n0p33sims15$ProbStop)))),
  
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
         sum(n15p33sims15$ProbSelect[1] + n15p33sims15$ProbStop)))),
  
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
         sum(n30p33sims15$ProbSelect[1] + n30p33sims15$ProbStop)))),
  
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
         sum(n45p33sims15$ProbSelect[1] + n45p33sims15$ProbStop)))),
  
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
         sum(n60p33sims15$ProbSelect[1] + n60p33sims15$ProbStop))))
) %>% 
  data.frame() %>% 
  mutate_if(is.numeric, round, digits = 2)


colnames(data) <- c('$phi_R$', '$j_R$', '1', '2', '3', '4', '5', '6', '7', '8',
                    '9', '10', '11', '12', '13' , '14', '15', 'Mean', 'StDev')

data %>% 
  kable(format = 'latex', booktabs = T, linesep = '', 
        align = c(rep('c', times = 19)), 
        caption = '\\label{tab_wt:OCsCombosSummaryGood}Probabilities of selecting good dose levels for multiple combinations of AR phase size and probabilities for randomisation to control, plus summary statistics.'
  ) %>% 
  kable_styling(position = 'center', latex_options = 'scale_down') %>%
  collapse_rows(columns = 1, latex_hline = "major",  valign = "middle") %>% 
  add_header_above(c(' ' = 2, 
                     'Probability of selecting good dose levels: Scenarios 1-15' = 15,
                     'All scenarios' = 2)) %>% 
  cat()

rm(data)
  