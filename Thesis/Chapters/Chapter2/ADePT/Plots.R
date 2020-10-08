
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
