library(ggplot2)
library(tidyverse)

data <- data.frame(resp = seq(0, 30, 1)) %>% 
  mutate(a = 1+resp,
         b=1+30-resp,
         Prob10 = pbeta(q = 0.1, shape1 = a, shape2 = b, lower.tail = F),
         Prob20 = pbeta(q = 0.2, shape1 = a, shape2 = b, lower.tail = F),
         Prob30 = pbeta(q = 0.3, shape1 = a, shape2 = b, lower.tail = F),
         Prob40 = pbeta(q = 0.4, shape1 = a, shape2 = b, lower.tail = F)
         ) %>% 
  select(-a, -b) %>% 
  pivot_longer(cols = starts_with("Prob"), 
                 names_to = "Prob")
data

data %>% 
  ggplot(aes(x = resp, y = value, col = Prob))+
  geom_line(linewidth = 1.5)+
  scale_x_continuous(breaks = seq(0, 30, by = 1))+
  labs(x = "Number of Responses", y = "Posterior Probaility", 
       col = "Decision Rule")+
  geom_vline(xintercept = 9, linetype="dotdash", size = 1)+
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), 
                     labels = expression(paste("P(", theta>=10, "%", ")"),
                                         paste("P(", theta>=20, "%", ")"),
                                         paste("P(", theta>=30, "%", ")"),
                                         paste("P(", theta>=40, "%", ")")))+
  expand_limits(x = 0, y = 0)+
  theme(legend.position = c(.9, .2),
        strip.background = element_rect(colour="white", fill="white"), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        legend.key = element_blank(),
        legend.text=element_text(size=7),
        legend.key.size = unit(2,"line")) 
  

