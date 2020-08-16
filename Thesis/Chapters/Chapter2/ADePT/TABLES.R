load('ADePT-DDR_OC.RData')
library(kableExtra)

################################################################################
# Need to change columnt that says MTD to TD25 if i ever run again. 
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
