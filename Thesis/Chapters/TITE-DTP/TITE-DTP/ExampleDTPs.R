library(dfcrm)
library(escalation)
library(dplyr)
library(kableExtra)
library(dtpcrm)
library(tidyr)
library(diagram)

# Set-up 
skeleton <- getprior(target = 0.25, nu =4, nlevel =5, halfwidth = 0.05)
target <- 0.25 

model <- get_dfcrm(skeleton = skeleton, target = target)

paths <- model %>% 
  get_dose_paths(cohort_sizes = c(3,3,3), next_dose =2)

#DTPs
spread_paths(as_tibble(paths)) %>%
  select(o0 = 'outcomes0', d0 = 'next_dose0', 
         o1 = 'outcomes1', d1 = 'next_dose1',
         o2 = 'outcomes2', d2 = 'next_dose2', 
         o3 = 'outcomes3', d3 = 'next_dose3') %>%
  print(n=100)

# Node plot
if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths)
}

# Code for the LaTeX table 
spread_paths(as_tibble(paths)) %>%
  select(o0 = 'outcomes0', d0 = 'next_dose0', 
         o1 = 'outcomes1', d1 = 'next_dose1',
         o2 = 'outcomes2', d2 = 'next_dose2', 
         o3 = 'outcomes3', d3 = 'next_dose3') %>%
  print(n=100) %>% 
  data.frame() %>% 
  mutate(o0 = 1:64) %>% 
  kable('latex', booktabs = T, linesep = "", align = "c", 
        col.names = c('Pathway', 'Dose', 'Outcomes', 'Dose', 'Outcomes', 'Dose',
                      'Outcomes', 'Dose'),
        caption = '\\label{tab_tite-dtp:InitialDTPExample}Initial DTP for the first three cohorts of our example CRM.'
        ) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position",  "scale_down"),
                position = "center", font_size = 8) %>% 
  add_header_above(c('', 'Cohort 1' = 2, 'Cohort 2' = 2, 'Cohort 3' = 2,
                     'Cohort 4' = 1)) %>% 
  cat()

# Check using dtpcrm 
calculate_dtps(2, cohort_sizes = c(3, 3, 3), dose_func = applied_crm,
                       prior = skeleton, target = target, no_skip_esc = FALSE)

# matches results form escalation

# DTP flow diagram 

calculate_dtps(2, cohort_sizes = c(3, 3, 3), dose_func = applied_crm,
               prior = skeleton, target = target, no_skip_esc = FALSE) %>% 
  dtpflow()

###############################################################################
# DTPs with stopping rules 

model1 <- get_dfcrm(skeleton = skeleton, target = target) %>% 
  dont_skip_doses(when_escalating = TRUE) %>% 
  stop_when_too_toxic(dose = 1, tox_threshold = 0.35, confidence = 0.9)

paths <- model %>% 
  get_dose_paths(cohort_sizes = c(3,3,3), next_dose =2)

#DTPs
spread_paths(as_tibble(paths)) %>%
  select(o0 = 'outcomes0', d0 = 'next_dose0', 
         o1 = 'outcomes1', d1 = 'next_dose1',
         o2 = 'outcomes2', d2 = 'next_dose2', 
         o3 = 'outcomes3', d3 = 'next_dose3') %>%
  print(n=100)

# Node plot
if(Sys.getenv("RSTUDIO") == "1") {
  graph_paths(paths)
}

# Code for the LaTeX table 
spread_paths(as_tibble(paths)) %>%
  select(o0 = 'outcomes0', d0 = 'next_dose0', 
         o1 = 'outcomes1', d1 = 'next_dose1',
         o2 = 'outcomes2', d2 = 'next_dose2', 
         o3 = 'outcomes3', d3 = 'next_dose3') %>%
  print(n=100) %>% 
  data.frame() %>% 
  mutate(o0 = 1:55) %>%
  replace_na(list(d2 = 'STOP', d3 = 'STOP')) %>%  
  kable('latex', booktabs = T, linesep = "", align = "c", 
        col.names = c('Pathway', 'Dose', 'Outcomes', 'Dose', 'Outcomes', 'Dose',
                      'Outcomes', 'Dose'),
        caption = '\\label{tab_tite-dtp:UpdatedDTPExample}Updated DTPs for the first three cohorts of our example CRM with additional rules.'
  ) %>% 
  kable_styling(latex_options = c("striped", "HOLD_position",  "scale_down"),
                position = "center", font_size = 4) %>% 
  add_header_above(c('', 'Cohort 1' = 2, 'Cohort 2' = 2, 'Cohort 3' = 2,
                     'Cohort 4' = 1)) %>% 
  cat()

# Check using dtpcrm 

stop_func <- function(x) {
  x = stop_for_excess_toxicity_empiric(x, tox_lim = 0.35, prob_cert = 0.90, 
                                       dose = 1, nsamps = 10000)
}
calculate_dtps(2, cohort_sizes = c(3, 3, 3), dose_func = applied_crm,
               prior = skeleton, target = target, stop_func = stop_func, 
               global_coherent_esc = FALSE)

# matches results form escalation

# DTP flow diagram 

calculate_dtps(2, cohort_sizes = c(3, 3, 3), dose_func = applied_crm,
               prior = skeleton, target = target, stop_func = stop_func, 
               global_coherent_esc = FALSE) %>% 
  dtpflow()

prior <- c(0.05, 0.10, 0.20, 0.35, 0.50, 0.70)
target <- 0.2
level <- c(2, 2, 2, 3, 3, 3, 4, 4, 4)
y <- c(0, 0, 0, 0, 0, 0, 0, 0, 1)
foo <- crm(skeleton, target, y, level)
summary(foo)

x0 <- c(2,2,2,3,3,3,4,4,4,rep(5,24)) # initial design
foo <- cohere(skeleton,target,x0)
foo$message
