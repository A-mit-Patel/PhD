
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
  

