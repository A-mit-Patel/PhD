load('ADePT-DDR_OC.RData')
library(kableExtra)

################################################################################
# ADePT-DDR Full simulations 
################################################################################

temp <- kable(OC_tab, "latex", booktabs = T, linesep = "", align = "c", 
      caption = " Operating Characteristics of the two-stage PO-TITE-CRM (with true DLT rates that imply 2b is more toxic than 2a) based on 2000 simulated trials. Definitions: DLT: Dose-limiting toxicity. P(select):
Probability of selecting a dose as the TD25.") %>%
  kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"),
                position = "center",
                stripe_index = c(2:5, 10:13, 18:21, 26:29)) %>% 
  add_header_above(c(" " = 2,"Dose Levels" = 6)) %>% 
  collapse_rows(columns = 1:2, latex_hline = "major",  valign = "middle")

cat(temp)

temp2 <- kable(OC_tab2, "latex", booktabs = T, linesep = "", align = "c", 
               caption = " Operating Characteristics of the two-stage PO-TITE-CRM (with true DLT rates that imply 2a is more toxic than 2b) based on 2000 simulated trials. Definitions: DLT: Dose-limiting toxicity. P(select):
Probability of selecting a dose as the TD25.") %>%
  kable_styling(latex_options = c("striped", "HOLD_position", "scale_down"),
                position = "center",
                stripe_index = c(2:5, 10:13, 18:21, 26:29)) %>% 
  add_header_above(c(" " = 2,"Dose Levels" = 6)) %>% 
  collapse_rows(columns = 1:2, latex_hline = "major",  valign = "middle") 

cat(temp2)


################################################################################
# Comparison Tables
################################################################################
# Load all data
load('ADePT-DDR_OC.RData')
load('TITE_sims.RData')
load('PO-CRM_sims.RData')
load('Modified_sims.RData')

# Table with selection probabilities
SelectProb <- round(rbind(round(c(skeleton, 0, 0),2), c(fit_s1$true.prob, 0, 0),
      c(fit_s1$MTD.selection, fit_s1$stop, fit_s1$months), 
      c(s1$summary$mtd, as.numeric(s1$summary$prob_stop[2]), s1$summary$months),
      c(po_s1$MTD.selection, po_s1$stop, po_s1$months), 
      c(mod30_s1$MTD.selection, mod30_s1$stop, mod30_s1$months),
      c(mod60_s1$MTD.selection, mod60_s1$stop, mod60_s1$months),
      c(modco_s1$MTD.selection, modco_s1$stop, modco_s1$months), 
      c(fit_s2$true.prob, 0, 0),
      c(fit_s2$MTD.selection, fit_s2$stop, fit_s2$months), 
      c(s2$summary$mtd, as.numeric(s2$summary$prob_stop[2]), s2$summary$months),
      c(po_s2$MTD.selection, po_s2$stop, po_s2$months), 
      c(mod30_s2$MTD.selection, mod30_s2$stop, mod30_s2$months),
      c(mod60_s2$MTD.selection, mod60_s2$stop, mod60_s2$months),
      c(modco_s2$MTD.selection, modco_s2$stop, modco_s2$months),
      c(fit_s3$true.prob, 0, 0),
      c(fit_s3$MTD.selection, fit_s3$stop, fit_s3$months), 
      c(s3$summary$mtd, as.numeric(s3$summary$prob_stop[2]), s3$summary$months),
      c(po_s3$MTD.selection, po_s3$stop, po_s3$months), 
      c(mod30_s3$MTD.selection, mod30_s3$stop, mod30_s3$months),
      c(mod60_s3$MTD.selection, mod60_s3$stop, mod60_s3$months),
      c(modco_s3$MTD.selection, modco_s3$stop, modco_s3$months),
      c(fit_s4$true.prob, 0, 0),
      c(fit_s4$MTD.selection, fit_s4$stop, fit_s4$months), 
      c(s4$summary$mtd, as.numeric(s4$summary$prob_stop[2]), s4$summary$months),
      c(po_s4$MTD.selection, po_s4$stop, po_s4$months), 
      c(mod30_s4$MTD.selection, mod30_s4$stop, mod30_s4$months),
      c(mod60_s4$MTD.selection, mod60_s4$stop, mod60_s4$months),
      c(modco_s4$MTD.selection, modco_s4$stop, modco_s4$months),
      c(fit_s5$true.prob, 0, 0),
      c(fit_s5$MTD.selection, fit_s5$stop, fit_s5$months), 
      c(s5$summary$mtd, as.numeric(s5$summary$prob_stop[2]), s5$summary$months),
      c(po_s5$MTD.selection, po_s5$stop, po_s5$months), 
      c(mod30_s5$MTD.selection, mod30_s5$stop, mod30_s5$months),
      c(mod60_s5$MTD.selection, mod60_s5$stop, mod60_s5$months),
      c(modco_s5$MTD.selection, modco_s5$stop, modco_s5$months),
      c(fit_s6$true.prob, 0, 0),
      c(fit_s6$MTD.selection, fit_s6$stop, fit_s6$months), 
      c(s6$summary$mtd, as.numeric(s6$summary$prob_stop[2]), s6$summary$months),
      c(po_s6$MTD.selection, po_s6$stop, po_s6$months), 
      c(mod30_s6$MTD.selection, mod30_s6$stop, mod30_s6$months),
      c(mod60_s6$MTD.selection, mod60_s6$stop, mod60_s6$months),
      c(modco_s6$MTD.selection, modco_s6$stop, modco_s6$months),
      c(fit_s7$true.prob, 0, 0),
      c(fit_s7$MTD.selection, fit_s7$stop, fit_s7$months), 
      c(s7$summary$mtd, as.numeric(s7$summary$prob_stop[2]), s7$summary$months),
      c(po_s7$MTD.selection, po_s7$stop, po_s7$months), 
      c(mod30_s7$MTD.selection, mod30_s7$stop, mod30_s7$months),
      c(mod60_s7$MTD.selection, mod60_s7$stop, mod60_s7$months),
      c(modco_s7$MTD.selection, modco_s7$stop, modco_s7$months),
      c(fit_s8$true.prob, 0, 0),
      c(fit_s8$MTD.selection, fit_s8$stop, fit_s8$months), 
      c(s8$summary$mtd, as.numeric(s8$summary$prob_stop[2]), s8$summary$months),
      c(po_s8$MTD.selection, po_s8$stop, po_s8$months), 
      c(mod30_s8$MTD.selection, mod30_s8$stop, mod30_s8$months),
      c(mod60_s8$MTD.selection, mod60_s8$stop, mod60_s8$months),
      c(modco_s8$MTD.selection, modco_s8$stop, modco_s8$months)
      ),2)

for (i in 1:nrow(SelectProb)) {
  if(SelectProb[i,7] == 0 | is.na(SelectProb[i,7])){
    SelectProb[i,7] <- c(' ')
  }
  if(SelectProb[i,8] == 0 ){
    SelectProb[i,8] <- c(' ')
  }

}

colnames(SelectProb)<- c("-1", "0", "1", "2a", "2b", "3", "Stop", "Duration")

SelectProbTab <- as.matrix(cbind(
  c('Scenario', rep('1: TD25 @-1',7), rep('2: TD25 @0',7), rep('3: TD25 @1',7),
    rep('4: TD25 @2a',7), rep('5: TD25 @2b',7), rep('6: TD25 @3',7), 
    rep('7: Equal steps',7), rep('8: All toxic',7)), 
  c('CRM details', rep(c(' ', 'PO-TITE', 'TITE', 'PO', 'N = 30', 'N = 60', 
                   'No cohorts'),8)),
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
