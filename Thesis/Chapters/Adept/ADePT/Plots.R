
# Plot of the weight function. 
Weight <- c(0.6,0.8,1)
Weeks <- c(8+7,12+7,52+7)
plot(Weeks, Weight)
library(ggplot2)

data <- data.frame(Weight, Weeks)

ggplot(data, aes(x = Weeks, y = Weight))+
  geom_line() +
  geom_vline(xintercept = 7, linetype = 'longdash', col = 'red') +
  geom_vline(xintercept = 15, linetype = 'longdash', col = 'red') +
  geom_vline(xintercept = 19, linetype = 'longdash', col = 'red') +
  geom_rect(aes(xmin=0, xmax=7, ymin=0, ymax=1), alpha = 0.05, fill = 'blue')+
  geom_rect(aes(xmin=7, xmax=15, ymin=0, ymax=1), alpha = 0.05, fill = 'darkgreen')+
  geom_rect(aes(xmin=15, xmax=59, ymin=0, ymax=1), alpha = 0.05, fill = 'lightgreen')+
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  annotate('text', x =3.5 , y = 0.5, label = 'Treatment Period')+
  annotate('text', x =3.5 , y = 0.45, label = '(7 Weeks)')+
  annotate('text', x =11 , y = 0.5, label = 'Minimum Follow-up')+
  annotate('text', x =11 , y = 0.45, label = '(8 Weeks)')+
  annotate('text', x =17 , y = 0.5, label = 'Follow-up')+
  annotate('text', x =17 , y = 0.45, label = '(4 Weeks)')+
  annotate('text', x =39.5 , y = 0.5, label = 'Follow-up')+
  annotate('text', x =39.5 , y = 0.45, label = '(40 Weeks)')+
  ggtitle('Weight function across follow-up period')+
  theme_bw()
  

# Plot of TRUE DLT Rates 

dltrate <- c(0.25, 0.40, 0.45, 0.50, 0.55, 0.60,
             0.12, 0.25, 0.40, 0.45, 0.50, 0.55,
             0.09, 0.12, 0.25, 0.40, 0.45, 0.50, 
             0.06, 0.09, 0.12, 0.25, 0.40, 0.45, 
             0.03, 0.06, 0.09, 0.12, 0.25, 0.40, 
             0.01, 0.03, 0.06, 0.09, 0.12, 0.25, 
             0.05, 0.10, 0.15, 0.20, 0.25, 0.30,
             0.50, 0.60, 0.65, 0.70, 0.75, 0.80)
dose <- rep(c(-1,0,1,'2a','2b',3), times = 8)
dose <- rep(1:6, times = 8)
doselabel <- c(-1,0,1,'2a','2b',3)

scenario <- rep(c('TD25@-1','TD25@0', 'TD25@1','TD25@2a','TD25@2b','TD25@3',
                  'Equal','All toxic'), each = 6)
data <- data.frame(dltrate, dose, scenario)

ggplot(data, aes(x = dose, y = dltrate, group=scenario)) +
  geom_line(aes(color = scenario), size = 1.5) +
  ggtitle('True DLT rates across scenarios used for simulations') +
  labs(x = 'Dose Level', y = 'DLT Rates', color = 'Scenario') +
  scale_x_continuous(expand = c(0, 0), labels = doselabel) +
  geom_hline(yintercept = 0.25, linetype = 'longdash', col = 'red') +
  theme_bw() 

# Plot of simulation results comparing multiple designs 
# Load all data
load('ADePT-DDR_OC.RData')
load('TITE_sims.RData')
load('PO-CRM_sims.RData')
load('Modified_sims.RData')
library(dplyr)
library(tidyr)
library(ggplot2)

data <- 
  round(rbind( 
    c(fit_s1$MTD.selection, fit_s1$stop, fit_s1$months, fit_s1$mean.n ), 
    c(s1$summary$mtd, as.numeric(s1$summary$prob_stop[2]), s1$summary$months, sum(s1$summary$doses_given)),
    c(po_s1$MTD.selection, po_s1$stop, po_s1$months, po_s1$mean.n), 
    c(mod30_s1$MTD.selection, mod30_s1$stop, mod30_s1$months,  mod30_s1$mean.n),
    c(mod60_s1$MTD.selection, mod60_s1$stop, mod60_s1$months,  mod60_s1$mean.n),
    c(modco_s1$MTD.selection, modco_s1$stop, modco_s1$months,  modco_s1$mean.n), 
 
    c(fit_s2$MTD.selection, fit_s2$stop, fit_s2$months, fit_s2$mean.n ),
    c(s2$summary$mtd, as.numeric(s2$summary$prob_stop[2]), s2$summary$months, sum(s2$summary$doses_given)),
    c(po_s2$MTD.selection, po_s2$stop, po_s2$months, po_s2$mean.n), 
    c(mod30_s2$MTD.selection, mod30_s2$stop, mod30_s2$months,  mod30_s2$mean.n),
    c(mod60_s2$MTD.selection, mod60_s2$stop, mod60_s2$months,  mod60_s2$mean.n),
    c(modco_s2$MTD.selection, modco_s2$stop, modco_s2$months,  modco_s2$mean.n),

    c(fit_s3$MTD.selection, fit_s3$stop, fit_s3$months, fit_s3$mean.n), 
    c(s3$summary$mtd, as.numeric(s3$summary$prob_stop[2]), s3$summary$months, sum(s3$summary$doses_given)),
    c(po_s3$MTD.selection, po_s3$stop, po_s3$months, po_s3$mean.n), 
    c(mod30_s3$MTD.selection, mod30_s3$stop, mod30_s3$months,  mod30_s3$mean.n),
    c(mod60_s3$MTD.selection, mod60_s3$stop, mod60_s3$months,  mod60_s3$mean.n),
    c(modco_s3$MTD.selection, modco_s3$stop, modco_s3$months,  modco_s3$mean.n),

    c(fit_s4$MTD.selection, fit_s4$stop, fit_s4$months, fit_s4$mean.n), 
    c(s4$summary$mtd, as.numeric(s4$summary$prob_stop[2]), s4$summary$months, sum(s4$summary$doses_given)),
    c(po_s4$MTD.selection, po_s4$stop, po_s4$months, po_s4$mean.n), 
    c(mod30_s4$MTD.selection, mod30_s4$stop, mod30_s4$months,  mod30_s4$mean.n),
    c(mod60_s4$MTD.selection, mod60_s4$stop, mod60_s4$months,  mod60_s4$mean.n),
    c(modco_s4$MTD.selection, modco_s4$stop, modco_s4$months,  modco_s4$mean.n),

    c(fit_s5$MTD.selection, fit_s5$stop, fit_s5$months, fit_s5$mean.n), 
    c(s5$summary$mtd, as.numeric(s5$summary$prob_stop[2]), s5$summary$months, sum(s5$summary$doses_given)),
    c(po_s5$MTD.selection, po_s5$stop, po_s5$months, po_s5$mean.n), 
    c(mod30_s5$MTD.selection, mod30_s5$stop, mod30_s5$months,  mod30_s5$mean.n),
    c(mod60_s5$MTD.selection, mod60_s5$stop, mod60_s5$months,  mod60_s5$mean.n),
    c(modco_s5$MTD.selection, modco_s5$stop, modco_s5$months,  modco_s5$mean.n),

    c(fit_s6$MTD.selection, fit_s6$stop, fit_s6$months, fit_s6$mean.n), 
    c(s6$summary$mtd, as.numeric(s6$summary$prob_stop[2]), s6$summary$months, sum(s6$summary$doses_given)),
    c(po_s6$MTD.selection, po_s6$stop, po_s6$months, po_s6$mean.n), 
    c(mod30_s6$MTD.selection, mod30_s6$stop, mod30_s6$months,  mod30_s6$mean.n),
    c(mod60_s6$MTD.selection, mod60_s6$stop, mod60_s6$months,  mod60_s6$mean.n),
    c(modco_s6$MTD.selection, modco_s6$stop, modco_s6$months,  modco_s6$mean.n),

    c(fit_s7$MTD.selection, fit_s7$stop, fit_s7$months, fit_s7$mean.n), 
    c(s7$summary$mtd, as.numeric(s7$summary$prob_stop[2]), s7$summary$months, sum(s7$summary$doses_given)),
    c(po_s7$MTD.selection, po_s7$stop, po_s7$months, po_s7$mean.n), 
    c(mod30_s7$MTD.selection, mod30_s7$stop, mod30_s7$months,  mod30_s7$mean.n),
    c(mod60_s7$MTD.selection, mod60_s7$stop, mod60_s7$months,  mod60_s7$mean.n),
    c(modco_s7$MTD.selection, modco_s7$stop, modco_s7$months,  modco_s7$mean.n),

    c(fit_s8$MTD.selection, fit_s8$stop, fit_s8$months, fit_s8$mean.n), 
    c(s8$summary$mtd, as.numeric(s8$summary$prob_stop[2]), s8$summary$months, sum(s8$summary$doses_given)),
    c(po_s8$MTD.selection, po_s8$stop, po_s8$months, po_s8$mean.n), 
    c(mod30_s8$MTD.selection, mod30_s8$stop, mod30_s8$months,  mod30_s8$mean.n),
    c(mod60_s8$MTD.selection, mod60_s8$stop, mod60_s8$months,  mod60_s8$mean.n),
    c(modco_s8$MTD.selection, modco_s8$stop, modco_s8$months,  modco_s8$mean.n)),2)



colnames(data)<- c("minusone", "zero", "one", "twoa", "twob", "three", "stop", "duration", 
                         "meann")
data <- data.frame(data)%>% 
  mutate(scenario = rep(c(1,2,3,4,5,6,7,8), each = 6),
         method = rep(c('PO-TITE', 'TITE', 'PO', 'N = 30', 'N = 60',
                        'No cohorts'), times = 8),
         stop = ifelse(is.na(stop), 0, stop)) %>% 
  select("minusone", "zero", "one",  "twoa", "twob", "three", "stop", scenario,
         method)

longdata <- data %>% 
  gather(key = 'measurement', value = 'Probability', "minusone", "zero", "one", 
         "twoa", "twob", "three", 'stop' )

ggplot(longdata, aes(x=measurement, y= Probability, col = method )) +
  geom_point() +
  facet_wrap(~scenario)


sprob <- c(0.67, 0.71, 0.57, 0.65, 0.79, 0.63, 
           0.52, 0.53, 0.53, 0.50, 0.69, 0.45, 
           0.55, 0.56, 0.59, 0.53, 0.70, 0.49,
           0.48, 0.56, 0.49, 0.45, 0.62, 0.46, 
           0.42, 0.58, 0.44, 0.41, 0.62, 0.41, 
           0.77, 0.74, 0.80, 0.77, 0.85, 0.83,
           0.29, 0.29, 0.26, 0.32, 0.34, 0.24, 
           0.73, 0.63, 0.86, 0.76, 0.86, 0.67)
scenario <- rep(c(1,2,3,4,5,6,7,8), each = 6)
scenario
method <- rep(c('PO-TITE', 'TITE', 'PO', 'N = 30', 'N = 60',
               'No cohorts'), times = 8)
method
data2 <- data.frame(cbind(as.numeric(sprob), scenario, method))
data2

ggplot(data2, aes(x = scenario, y = sprob, col = method)) +
  geom_point()
