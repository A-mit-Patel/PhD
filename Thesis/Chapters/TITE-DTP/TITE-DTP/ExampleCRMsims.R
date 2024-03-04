library(dplyr)
library(escalation)
library(dfcrm)
library(kableExtra)


# Set-up 
skeleton <- getprior(target = 0.25, nu =4, nlevel =5, halfwidth = 0.05)
target <- 0.25 
num_sims <- 10000

sc1 <- c(0.25, 0.35, 0.45, 0.55, 0.65)
sc2 <- c(0.15, 0.25, 0.35, 0.45, 0.55)
sc3 <- c(0.10, 0.15, 0.25, 0.35, 0.45)
sc4 <- c(0.05, 0.10, 0.15, 0.25, 0.35)
sc5 <- c(0.01, 0.05, 0.10, 0.15, 0.25)
sc6 <- c(0.50, 0.55, 0.65, 0.75, 0.85)

## First example CRM
model1 <- get_dfcrm(skeleton = skeleton, target = target)
## Second example CRM with stopping rules
model2 <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  dont_skip_doses(when_escalating = TRUE) %>% 
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.9)

TimeStart <- Sys.time()

# Scenario 1 - Dose 1 TD25 

sc1_mod1 <- model1 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc1,
                  next_dose = 2)

#sc1_mod1

sc1_mod2 <- model2 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc1,
                  next_dose = 2)
#sc1_mod2

# Scenario 2 - Dose 2 TD25 

sc2_mod1 <- model1 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc2,
                  next_dose = 2)

#sc2_mod1

sc2_mod2 <- model2 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc2,
                  next_dose = 2)
#sc2_mod2

# Scenario 3 - Dose 3 TD25 

sc3_mod1 <- model1 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc3,
                  next_dose = 2)

#sc3_mod1

sc3_mod2 <- model2 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc3,
                  next_dose = 2)
#sc3_mod2

# Scenario 4 - Dose 4 TD25 

sc4_mod1 <- model1 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc4,
                  next_dose = 2)

#sc4_mod1

sc4_mod2 <- model2 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc4,
                  next_dose = 2)
#sc4_mod2

# Scenario 5 - Dose 5 TD25 

sc5_mod1 <- model1 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc5,
                  next_dose = 2)

#sc5_mod1

sc5_mod2 <- model2 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc5,
                  next_dose = 2)
#sc5_mod2

# Scenario 6 - All toxic 

sc6_mod1 <- model1 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc6,
                  next_dose = 2)

#sc6_mod1

sc6_mod2 <- model2 %>% 
  stop_at_n(n = 30) %>% 
  simulate_trials(num_sims = num_sims, true_prob_tox = sc6,
                  next_dose = 2)
#sc6_mod2

TimeEnd <- Sys.time()
Duration <- TimeEnd - TimeStart

save.image(file = 'ExampleCRMsims.RData')

################################################################################

# Tables for thesis 

# First example CRM 

OCtab_1 <- rbind(
  c(round(skeleton, 2), " "),
  
  c(sc1, " "), 
  c(prob_recommend(sc1_mod1)[2:6], prob_recommend(sc1_mod1)[1]) %>% round(digits = 2),
  
  c(sc2, " "), 
  c(prob_recommend(sc2_mod1)[2:6], prob_recommend(sc2_mod1)[1]) %>% round(digits = 2),
  
  c(sc3, " "), 
  c(prob_recommend(sc3_mod1)[2:6], prob_recommend(sc3_mod1)[1]) %>% round(digits = 2),
  
  c(sc4, " "), 
  c(prob_recommend(sc4_mod1)[2:6], prob_recommend(sc4_mod1)[1]) %>% round(digits = 2),
  
  c(sc5, " "), 
  c(prob_recommend(sc5_mod1)[2:6], prob_recommend(sc5_mod1)[1]) %>% round(digits = 2),
  
  c(sc6, " "), 
  c(prob_recommend(sc6_mod1)[2:6], prob_recommend(sc6_mod1)[1]) %>% round(digits = 2)
)

OCtab_1 <- cbind(
  c("Scenario" , 
    "1:TD25 @1", "1:TD25 @1", "2:TD25 @2", "2:TD25 @2",
    "3:TD25 @3", "3:TD25 @3", "4:TD25 @4", "4:TD25 @4",
    "5:TD25 @5", "5:TD25 @5", "6:All toxic", "6:All toxic"),
  c("Prior DLT", rep(c("True DLT rate", "P(Select)"),6)),
  OCtab_1
)

codetab_1 <- kable(OCtab_1, "latex", booktabs = T, linesep = "", align = "c", 
                   col.names = c(" ", " ", "1", "2", "3", "4", "5", "Stop"),
                   caption = "Selection probabilities from 10000 simulated trials under various scenarios for the example CRM."
                   ) %>% 
  kable_styling(latex_options = c("HOLD_position"),
                position = "center") %>% 
  add_header_above(c(" " = 2, "Dose Levels" = 5)) %>% 
  collapse_rows(columns = 1, latex_hline = "major",  valign = "middle") 

cat(codetab_1)  

# Updated example CRM 

OCtab_2 <- rbind(
  c(round(skeleton, 2), " "),
  
  c(sc1, " "), 
  c(prob_recommend(sc1_mod2)[2:6], prob_recommend(sc1_mod2)[1]) %>% round(digits = 2),
  
  c(sc2, " "), 
  c(prob_recommend(sc2_mod2)[2:6], prob_recommend(sc2_mod2)[1]) %>% round(digits = 2),
  
  c(sc3, " "), 
  c(prob_recommend(sc3_mod2)[2:6], prob_recommend(sc3_mod2)[1]) %>% round(digits = 2),
  
  c(sc4, " "), 
  c(prob_recommend(sc4_mod2)[2:6], prob_recommend(sc4_mod2)[1]) %>% round(digits = 2),
  
  c(sc5, " "), 
  c(prob_recommend(sc5_mod2)[2:6], prob_recommend(sc5_mod2)[1]) %>% round(digits = 2),
  
  c(sc6, " "), 
  c(prob_recommend(sc6_mod2)[2:6], prob_recommend(sc6_mod2)[1]) %>% round(digits = 2)
)

OCtab_2 <- cbind(
  c("Scenario" , 
    "1:TD25 @1", "1:TD25 @1", "2:TD25 @2", "2:TD25 @2",
    "3:TD25 @3", "3:TD25 @3", "4:TD25 @4", "4:TD25 @4",
    "5:TD25 @5", "5:TD25 @5", "6:All toxic", "6:All toxic"),
  c("Prior DLT", rep(c("True DLT rate", "P(Select)"),6)),
  OCtab_2
)

codetab_2 <- kable(OCtab_2, "latex", booktabs = T, linesep = "", align = "c", 
                   col.names = c(" ", " ", "1", "2", "3", "4", "5", "Stop"),
                   caption = "Updated selection probabilities from 10000 simulated trials under various scenarios for the example CRM with additional rules."
) %>% 
  kable_styling(latex_options = c("HOLD_position"),
                position = "center") %>% 
  add_header_above(c(" " = 2, "Dose Levels" = 5)) %>% 
  collapse_rows(columns = 1, latex_hline = "major",  valign = "middle") 

cat(codetab_2)  
