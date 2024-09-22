load('ADePT-DDR_OC.RData')
library(kableExtra)

################################################################################
# ADePT-DDR Full simulations 
################################################################################

# Table for scenarios 1-8
temp <- kable(OC_tab, "latex", booktabs = T, linesep = "", align = "c", 
      caption = " Operating Characteristics of the two-stage PO-TITE-CRM (with true DLT rates that imply 2b is more toxic than 2a) based on 10000 simulated trials. Definitions: DLT: Dose-limiting toxicity. P(select):
Probability of selecting a dose as the TD25.") %>%
  kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"),
                position = "center",
                stripe_index = c(2:5, 10:13, 18:21, 26:29)) %>% 
  add_header_above(c(" " = 2,"Dose Levels" = 6)) %>% 
  collapse_rows(columns = 1:2, latex_hline = "major",  valign = "middle")

cat(temp)

# Table for scenarios 9-16 
temp2 <- kable(OC_tab2, "latex", booktabs = T, linesep = "", align = "c", 
               caption = " Operating Characteristics of the two-stage PO-TITE-CRM (with true DLT rates that imply 2a is more toxic than 2b) based on 10000 simulated trials. Definitions: DLT: Dose-limiting toxicity. P(select):
Probability of selecting a dose as the TD25.") %>%
  kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"),
                position = "center",
                stripe_index = c(2:5, 10:13, 18:21, 26:29)) %>% 
  add_header_above(c(" " = 2,"Dose Levels" = 6)) %>% 
  collapse_rows(columns = 1:2, latex_hline = "major",  valign = "middle")

cat(temp2)

# Table for N in simulations 
Ndata <- rbind(
  c(fit_s1$max.n.count, fit_s1$summary.trialsize[6], fit_s1$summary.trialsize[4]),
  c(fit_s2$max.n.count, fit_s2$summary.trialsize[6], fit_s2$summary.trialsize[4]),
  c(fit_s3$max.n.count, fit_s3$summary.trialsize[6], fit_s3$summary.trialsize[4]),
  c(fit_s4$max.n.count, fit_s4$summary.trialsize[6], fit_s4$summary.trialsize[4]),
  c(fit_s5$max.n.count, fit_s5$summary.trialsize[6], fit_s5$summary.trialsize[4]),
  c(fit_s6$max.n.count, fit_s6$summary.trialsize[6], fit_s6$summary.trialsize[4]),
  c(fit_s7$max.n.count, fit_s7$summary.trialsize[6], fit_s7$summary.trialsize[4]),
  c(fit_s8$max.n.count, fit_s8$summary.trialsize[6], fit_s8$summary.trialsize[4]), 
  c(fit2_s1$max.n.count, fit2_s1$summary.trialsize[6], fit2_s1$summary.trialsize[4]),
  c(fit2_s2$max.n.count, fit2_s2$summary.trialsize[6], fit2_s2$summary.trialsize[4]),
  c(fit2_s3$max.n.count, fit2_s3$summary.trialsize[6], fit2_s3$summary.trialsize[4]),
  c(fit2_s4$max.n.count, fit2_s4$summary.trialsize[6], fit2_s4$summary.trialsize[4]),
  c(fit2_s5$max.n.count, fit2_s5$summary.trialsize[6], fit2_s5$summary.trialsize[4]),
  c(fit2_s6$max.n.count, fit2_s6$summary.trialsize[6], fit2_s6$summary.trialsize[4]),
  c(fit2_s7$max.n.count, fit2_s7$summary.trialsize[6], fit2_s7$summary.trialsize[4]),
  c(fit2_s8$max.n.count, fit2_s8$summary.trialsize[6], fit2_s8$summary.trialsize[4])
)

# Kristian recommended using percentages instead of raw numbers so will do that 

Ndata <- rbind(
  c(fit_s1$max.n.count/100, fit_s1$summary.trialsize[6], fit_s1$summary.trialsize[4]),
  c(fit_s2$max.n.count/100, fit_s2$summary.trialsize[6], fit_s2$summary.trialsize[4]),
  c(fit_s3$max.n.count/100, fit_s3$summary.trialsize[6], fit_s3$summary.trialsize[4]),
  c(fit_s4$max.n.count/100, fit_s4$summary.trialsize[6], fit_s4$summary.trialsize[4]),
  c(fit_s5$max.n.count/100, fit_s5$summary.trialsize[6], fit_s5$summary.trialsize[4]),
  c(fit_s6$max.n.count/100, fit_s6$summary.trialsize[6], fit_s6$summary.trialsize[4]),
  c(fit_s7$max.n.count/100, fit_s7$summary.trialsize[6], fit_s7$summary.trialsize[4]),
  c(fit_s8$max.n.count/100, fit_s8$summary.trialsize[6], fit_s8$summary.trialsize[4]), 
  c(fit2_s1$max.n.count/100, fit2_s1$summary.trialsize[6], fit2_s1$summary.trialsize[4]),
  c(fit2_s2$max.n.count/100, fit2_s2$summary.trialsize[6], fit2_s2$summary.trialsize[4]),
  c(fit2_s3$max.n.count/100, fit2_s3$summary.trialsize[6], fit2_s3$summary.trialsize[4]),
  c(fit2_s4$max.n.count/100, fit2_s4$summary.trialsize[6], fit2_s4$summary.trialsize[4]),
  c(fit2_s5$max.n.count/100, fit2_s5$summary.trialsize[6], fit2_s5$summary.trialsize[4]),
  c(fit2_s6$max.n.count/100, fit2_s6$summary.trialsize[6], fit2_s6$summary.trialsize[4]),
  c(fit2_s7$max.n.count/100, fit2_s7$summary.trialsize[6], fit2_s7$summary.trialsize[4]),
  c(fit2_s8$max.n.count/100, fit2_s8$summary.trialsize[6], fit2_s8$summary.trialsize[4])
)

library(dplyr)
Ndata <- Ndata %>%  
  data.frame() %>% 
  mutate(Scenario = c('1: TD25 @-1', '2: TD25 @0', '3: TD25 @1', '4: TD25 @2a',
                      '5: TD25 @2b', '6: TD25 @3', '7: Equal steps', '8: All toxic',
                      '9: TD25 @-1', '10: TD25 @0', '11: TD25 @1', '12: TD25 @2a',
                      '13: TD25 @2b', '14: TD25 @3', '15: Equal steps', '16: All toxic'),
         Mean = round(Mean, digits = 2)) %>% 
  select(Scenario, 'Max no. of patients' = Max., '% max reached' = V1,
         'Mean no. of patients' = Mean)
  
temp3 <- kable(Ndata, "latex", booktabs = T, linesep = "", align = "c", 
               caption = 'Summary of simulated patient numbers for each scenario.') %>% 
  kable_styling(latex_options = c("striped", "HOLD_position",  "scale_down"),
                position = "center")
  
cat(temp3)

rm(list = ls(all.names = TRUE))
################################################################################
# Comparison Tables scenarios 1-8
################################################################################
# Load all data
load('ADePT-DDR_OC.RData')
load('TITE_sims.RData')
load('PO-CRM_sims.RData')
load('Modified_sims.RData')

# Table with selection probabilities
SelectProb <- round(rbind(round(c(skeleton, 0, 0, 0),2), c(fit_s1$true.prob, 0, 0, 0),
                          c(fit_s1$MTD.selection, fit_s1$stop, fit_s1$months, fit_s1$mean.n ), 
                          c(s1$summary$mtd, as.numeric(s1$summary$prob_stop[2]), s1$summary$months, sum(s1$summary$doses_given) ),
                          c(po_s1$MTD.selection, po_s1$stop, po_s1$months, po_s1$mean.n), 
                          c(mod30_s1$MTD.selection, mod30_s1$stop, mod30_s1$months,  mod30_s1$mean.n),
                          c(mod60_s1$MTD.selection, mod60_s1$stop, mod60_s1$months,  mod60_s1$mean.n),
                          c(modco_s1$MTD.selection, modco_s1$stop, modco_s1$months,  modco_s1$mean.n), 
                          c(fit_s2$true.prob, 0, 0, 0),
                          c(fit_s2$MTD.selection, fit_s2$stop, fit_s2$months, fit_s2$mean.n ), 
                          c(s2$summary$mtd, as.numeric(s2$summary$prob_stop[2]), s2$summary$months, sum(s2$summary$doses_given)),
                          c(po_s2$MTD.selection, po_s2$stop, po_s2$months, po_s2$mean.n), 
                          c(mod30_s2$MTD.selection, mod30_s2$stop, mod30_s2$months,  mod30_s2$mean.n),
                          c(mod60_s2$MTD.selection, mod60_s2$stop, mod60_s2$months,  mod60_s2$mean.n),
                          c(modco_s2$MTD.selection, modco_s2$stop, modco_s2$months,  modco_s2$mean.n),
                          c(fit_s3$true.prob, 0, 0, 0),
                          c(fit_s3$MTD.selection, fit_s3$stop, fit_s3$months, fit_s3$mean.n), 
                          c(s3$summary$mtd, as.numeric(s3$summary$prob_stop[2]), s3$summary$months, sum(s3$summary$doses_given)),
                          c(po_s3$MTD.selection, po_s3$stop, po_s3$months, po_s3$mean.n), 
                          c(mod30_s3$MTD.selection, mod30_s3$stop, mod30_s3$months,  mod30_s3$mean.n),
                          c(mod60_s3$MTD.selection, mod60_s3$stop, mod60_s3$months,  mod60_s3$mean.n),
                          c(modco_s3$MTD.selection, modco_s3$stop, modco_s3$months,  modco_s3$mean.n),
                          c(fit_s4$true.prob, 0, 0, 0),
                          c(fit_s4$MTD.selection, fit_s4$stop, fit_s4$months, fit_s4$mean.n), 
                          c(s4$summary$mtd, as.numeric(s4$summary$prob_stop[2]), s4$summary$months, sum(s4$summary$doses_given)),
                          c(po_s4$MTD.selection, po_s4$stop, po_s4$months, po_s4$mean.n), 
                          c(mod30_s4$MTD.selection, mod30_s4$stop, mod30_s4$months,  mod30_s4$mean.n),
                          c(mod60_s4$MTD.selection, mod60_s4$stop, mod60_s4$months,  mod60_s4$mean.n),
                          c(modco_s4$MTD.selection, modco_s4$stop, modco_s4$months,  modco_s4$mean.n),
                          c(fit_s5$true.prob, 0, 0, 0),
                          c(fit_s5$MTD.selection, fit_s5$stop, fit_s5$months, fit_s5$mean.n), 
                          c(s5$summary$mtd, as.numeric(s5$summary$prob_stop[2]), s5$summary$months, sum(s5$summary$doses_given)),
                          c(po_s5$MTD.selection, po_s5$stop, po_s5$months, po_s5$mean.n), 
                          c(mod30_s5$MTD.selection, mod30_s5$stop, mod30_s5$months,  mod30_s5$mean.n),
                          c(mod60_s5$MTD.selection, mod60_s5$stop, mod60_s5$months,  mod60_s5$mean.n),
                          c(modco_s5$MTD.selection, modco_s5$stop, modco_s5$months,  modco_s5$mean.n),
                          c(fit_s6$true.prob, 0, 0, 0),
                          c(fit_s6$MTD.selection, fit_s6$stop, fit_s6$months, fit_s6$mean.n), 
                          c(s6$summary$mtd, as.numeric(s6$summary$prob_stop[2]), s6$summary$months, sum(s6$summary$doses_given)),
                          c(po_s6$MTD.selection, po_s6$stop, po_s6$months, po_s6$mean.n), 
                          c(mod30_s6$MTD.selection, mod30_s6$stop, mod30_s6$months,  mod30_s6$mean.n),
                          c(mod60_s6$MTD.selection, mod60_s6$stop, mod60_s6$months,  mod60_s6$mean.n),
                          c(modco_s6$MTD.selection, modco_s6$stop, modco_s6$months,  modco_s6$mean.n),
                          c(fit_s7$true.prob, 0, 0, 0),
                          c(fit_s7$MTD.selection, fit_s7$stop, fit_s7$months, fit_s7$mean.n), 
                          c(s7$summary$mtd, as.numeric(s7$summary$prob_stop[2]), s7$summary$months, sum(s7$summary$doses_given)),
                          c(po_s7$MTD.selection, po_s7$stop, po_s7$months, po_s7$mean.n), 
                          c(mod30_s7$MTD.selection, mod30_s7$stop, mod30_s7$months,  mod30_s7$mean.n),
                          c(mod60_s7$MTD.selection, mod60_s7$stop, mod60_s7$months,  mod60_s7$mean.n),
                          c(modco_s7$MTD.selection, modco_s7$stop, modco_s7$months,  modco_s7$mean.n),
                          c(fit_s8$true.prob, 0, 0, 0),
                          c(fit_s8$MTD.selection, fit_s8$stop, fit_s8$months, fit_s8$mean.n), 
                          c(s8$summary$mtd, as.numeric(s8$summary$prob_stop[2]), s8$summary$months, sum(s8$summary$doses_given)),
                          c(po_s8$MTD.selection, po_s8$stop, po_s8$months, po_s8$mean.n), 
                          c(mod30_s8$MTD.selection, mod30_s8$stop, mod30_s8$months,  mod30_s8$mean.n),
                          c(mod60_s8$MTD.selection, mod60_s8$stop, mod60_s8$months,  mod60_s8$mean.n),
                          c(modco_s8$MTD.selection, modco_s8$stop, modco_s8$months,  modco_s8$mean.n)),2)


for (i in 1:nrow(SelectProb)) {
  if(SelectProb[i,7] == 0 | is.na(SelectProb[i,7])){
    SelectProb[i,7] <- c(' ')
  }
  if(SelectProb[i,8] == 0 ){
    SelectProb[i,8] <- c(' ')
  }
  if(SelectProb[i,9] == 0 ){
    SelectProb[i,9] <- c(' ')
  }
  
}

colnames(SelectProb)<- c("-1", "0", "1", "2a", "2b", "3", "Stop", "Duration", 
                         "Mean N")

SelectProbTab <- as.matrix(cbind(
  c('Scenario', rep('1: TD25 @-1',7), rep('2: TD25 @0',7), rep('3: TD25 @1',7),
    rep('4: TD25 @2a',7), rep('5: TD25 @2b',7), rep('6: TD25 @3',7), 
    rep('7: Equal steps',7), rep('8: All toxic',7)), 
  c('CRM details', rep(c(' ', 'PO-TITE', 'TITE', 'PO', 'N = 30', 'N = 60', 
                         'CS = 1'),8)),
  c('Prior DLT', rep(c('True DLT rate', rep('P(select)',6)),8)), 
  SelectProb))


SelectProbTabCode <-kable(SelectProbTab, "latex", booktabs = T, linesep = "", 
                          align = "c", caption = '\\label{tab_adept:Design_Comparison}Selection percentage of the TD25 and expected trial duration (in months) for the PO-TITE, TITE and PO CRM designs for scenarios 1-8 based on 2000 simulated trials.'
) %>% 
  kable_styling(latex_options = c("striped", "scale_down"),
                position = "center", font_size = 9,
                stripe_index = c(2:8, 16:22, 30:36, 44:50)) %>% 
  add_header_above(c(" " = 3,"Dose Levels" = 6)) %>% 
  collapse_rows(columns = 1:2, latex_hline = "major",  valign = "middle") 
cat(SelectProbTabCode)

rm(list = ls(all.names = TRUE))

################################################################################
# Comparison Tables scenarios 9-16
################################################################################
# Load all data
load('ADePT-DDR_OC.RData')
load('TITE_sims.RData')
load('PO-CRM_sims.RData')
load('Modified_sims.RData')

# Table with selection probabilities
SelectProb <- round(rbind(round(c(skeleton, 0, 0, 0),2), c(fit2_s1$true.prob, 0, 0, 0),
                          c(fit2_s1$MTD.selection, fit2_s1$stop, fit2_s1$months, fit2_s1$mean.n ), 
                          c(s9$summary$mtd, as.numeric(s9$summary$prob_stop[2]), s9$summary$months, sum(s9$summary$doses_given) ),
                          c(po_s9$MTD.selection, po_s9$stop, po_s9$months, po_s9$mean.n), 
                          c(mod30_s9$MTD.selection, mod30_s9$stop, mod30_s9$months,  mod30_s9$mean.n),
                          c(mod60_s9$MTD.selection, mod60_s9$stop, mod60_s9$months,  mod60_s9$mean.n),
                          c(modco_s9$MTD.selection, modco_s9$stop, modco_s9$months,  modco_s9$mean.n), 
                          c(fit2_s2$true.prob, 0, 0, 0),
                          c(fit2_s2$MTD.selection, fit2_s2$stop, fit2_s2$months, fit2_s2$mean.n ), 
                          c(s10$summary$mtd, as.numeric(s10$summary$prob_stop[2]), s10$summary$months, sum(s10$summary$doses_given)),
                          c(po_s10$MTD.selection, po_s10$stop, po_s10$months, po_s10$mean.n), 
                          c(mod30_s10$MTD.selection, mod30_s10$stop, mod30_s10$months,  mod30_s10$mean.n),
                          c(mod60_s10$MTD.selection, mod60_s10$stop, mod60_s10$months,  mod60_s10$mean.n),
                          c(modco_s10$MTD.selection, modco_s10$stop, modco_s10$months,  modco_s10$mean.n),
                          c(fit2_s3$true.prob, 0, 0, 0),
                          c(fit2_s3$MTD.selection, fit2_s3$stop, fit2_s3$months, fit2_s3$mean.n), 
                          c(s11$summary$mtd, as.numeric(s11$summary$prob_stop[2]), s11$summary$months, sum(s11$summary$doses_given)),
                          c(po_s11$MTD.selection, po_s11$stop, po_s11$months, po_s11$mean.n), 
                          c(mod30_s11$MTD.selection, mod30_s11$stop, mod30_s11$months,  mod30_s11$mean.n),
                          c(mod60_s11$MTD.selection, mod60_s11$stop, mod60_s11$months,  mod60_s11$mean.n),
                          c(modco_s11$MTD.selection, modco_s11$stop, modco_s11$months,  modco_s11$mean.n),
                          c(fit2_s4$true.prob, 0, 0, 0),
                          c(fit2_s4$MTD.selection, fit2_s4$stop, fit2_s4$months, fit2_s4$mean.n), 
                          c(s12$summary$mtd, as.numeric(s12$summary$prob_stop[2]), s12$summary$months, sum(s12$summary$doses_given)),
                          c(po_s12$MTD.selection, po_s12$stop, po_s12$months, po_s12$mean.n), 
                          c(mod30_s12$MTD.selection, mod30_s12$stop, mod30_s12$months,  mod30_s12$mean.n),
                          c(mod60_s12$MTD.selection, mod60_s12$stop, mod60_s12$months,  mod60_s12$mean.n),
                          c(modco_s12$MTD.selection, modco_s12$stop, modco_s12$months,  modco_s12$mean.n),
                          c(fit2_s5$true.prob, 0, 0, 0),
                          c(fit2_s5$MTD.selection, fit2_s5$stop, fit2_s5$months, fit2_s5$mean.n), 
                          c(s13$summary$mtd, as.numeric(s13$summary$prob_stop[2]), s13$summary$months, sum(s13$summary$doses_given)),
                          c(po_s13$MTD.selection, po_s13$stop, po_s13$months, po_s13$mean.n), 
                          c(mod30_s13$MTD.selection, mod30_s13$stop, mod30_s13$months,  mod30_s13$mean.n),
                          c(mod60_s13$MTD.selection, mod60_s13$stop, mod60_s13$months,  mod60_s13$mean.n),
                          c(modco_s13$MTD.selection, modco_s13$stop, modco_s13$months,  modco_s13$mean.n),
                          c(fit2_s6$true.prob, 0, 0, 0),
                          c(fit2_s6$MTD.selection, fit2_s6$stop, fit2_s6$months, fit2_s6$mean.n), 
                          c(s14$summary$mtd, as.numeric(s14$summary$prob_stop[2]), s14$summary$months, sum(s14$summary$doses_given)),
                          c(po_s14$MTD.selection, po_s14$stop, po_s14$months, po_s14$mean.n), 
                          c(mod30_s14$MTD.selection, mod30_s14$stop, mod30_s14$months,  mod30_s14$mean.n),
                          c(mod60_s14$MTD.selection, mod60_s14$stop, mod60_s14$months,  mod60_s14$mean.n),
                          c(modco_s14$MTD.selection, modco_s14$stop, modco_s14$months,  modco_s14$mean.n),
                          c(fit2_s7$true.prob, 0, 0, 0),
                          c(fit2_s7$MTD.selection, fit2_s7$stop, fit2_s7$months, fit2_s7$mean.n), 
                          c(s15$summary$mtd, as.numeric(s15$summary$prob_stop[2]), s15$summary$months, sum(s15$summary$doses_given)),
                          c(po_s15$MTD.selection, po_s15$stop, po_s15$months, po_s15$mean.n), 
                          c(mod30_s15$MTD.selection, mod30_s15$stop, mod30_s15$months,  mod30_s15$mean.n),
                          c(mod60_s15$MTD.selection, mod60_s15$stop, mod60_s15$months,  mod60_s15$mean.n),
                          c(modco_s15$MTD.selection, modco_s15$stop, modco_s15$months,  modco_s15$mean.n),
                          c(fit2_s8$true.prob, 0, 0, 0),
                          c(fit2_s8$MTD.selection, fit2_s8$stop, fit2_s8$months, fit2_s8$mean.n), 
                          c(s16$summary$mtd, as.numeric(s16$summary$prob_stop[2]), s16$summary$months, sum(s16$summary$doses_given)),
                          c(po_s16$MTD.selection, po_s16$stop, po_s16$months, po_s16$mean.n), 
                          c(mod30_s16$MTD.selection, mod30_s16$stop, mod30_s16$months,  mod30_s16$mean.n),
                          c(mod60_s16$MTD.selection, mod60_s16$stop, mod60_s16$months,  mod60_s16$mean.n),
                          c(modco_s16$MTD.selection, modco_s16$stop, modco_s16$months,  modco_s16$mean.n)),2)


for (i in 1:nrow(SelectProb)) {
  if(SelectProb[i,7] == 0 | is.na(SelectProb[i,7])){
    SelectProb[i,7] <- c(' ')
  }
  if(SelectProb[i,8] == 0 ){
    SelectProb[i,8] <- c(' ')
  }
  if(SelectProb[i,9] == 0 ){
    SelectProb[i,9] <- c(' ')
  }
  
}

colnames(SelectProb)<- c("-1", "0", "1", "2a", "2b", "3", "Stop", "Duration", 
                         "Mean N")

SelectProbTab <- as.matrix(cbind(
  c('Scenario', rep('9: TD25 @-1',7), rep('10: TD25 @0',7), rep('11: TD25 @1',7),
    rep('12: TD25 @2a',7), rep('13: TD25 @2b',7), rep('14: TD25 @3',7), 
    rep('15: Equal steps',7), rep('16: All toxic',7)), 
  c('CRM details', rep(c(' ', 'PO-TITE', 'TITE', 'PO', 'N = 30', 'N = 60', 
                         'CS = 1'),8)),
  c('Prior DLT', rep(c('True DLT rate', rep('P(select)',6)),8)), 
  SelectProb))


SelectProbTabCode <-kable(SelectProbTab, "latex", booktabs = T, linesep = "", 
                          align = "c", caption = '\\label{tab_adept:Design_Comparison}Selection percentage of the TD25 and expected trial duration (in months) for the PO-TITE, TITE and PO CRM designs for scenarios 9-16 based on 2000 simulated trials.'
) %>% 
  kable_styling(latex_options = c("striped", "scale_down"),
                position = "center", font_size = 9,
                stripe_index = c(2:8, 16:22, 30:36, 44:50)) %>% 
  add_header_above(c(" " = 3,"Dose Levels" = 6)) %>% 
  collapse_rows(columns = 1:2, latex_hline = "major",  valign = "middle") 
cat(SelectProbTabCode)

rm(list = ls(all.names = TRUE))
