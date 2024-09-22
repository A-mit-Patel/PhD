source('tite.pocrm.R')

# ADePT Setup  
orders<-matrix(nrow=2,ncol=6)
orders[1,]<-c(1,2,3,4,5,6)
orders[2,]<-c(1,2,3,5,4,6)

skeleton <- c(0.012,0.036,0.084,0.157,0.25,0.355)

prior.s <- getwm(orders,skeleton)
prior.o <- rep(1/2,2)
target <- 0.25

###############################################################################
# No DLT till 3 (6)
dose <-   c(2,2,2,
            3,3,3,
            4,4,4,
            5,5,5,
            6,6,6)
dlt <-    c(0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,
            0,1,0) # 2nd patient 
weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            0.95, 0.91, 0.87,
            0.82, 1, 0.6)

tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target,
           dlt = dlt, dose = dose, weight = weight) -> fit1

# Recommends 3 (6)
# Next cohort 3 DLTs 

dose <-   c(2,2,2,
            3,3,3,
            4,4,4,
            5,5,5,
            6,6,6,
            6,6,6)
dlt <-    c(0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,
            0,1,0,
            1,1,1) # all 3 patients 
weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            1, 0.97, 0.93,
            0.88, 1, 0.84,
            1, 1, 1)

tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target,
           dlt = dlt, dose = dose, weight = weight) -> fit2

# Recommends 2b (5)
# Next cohort 2 DLT 
dose <-   c(2,2,2,
            3,3,3,
            4,4,4,
            5,5,5,
            6,6,6,
            6,6,6,
            5,5,5)
dlt <-    c(0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,
            0,1,0,
            1,1,1,
            0,1,1) # 2nd patient and 3rd patient 
weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            0.97, 1, 0.93,
            1, 1, 1,
            0.83, 1, 1)

tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target,
           dlt = dlt, dose = dose, weight = weight) -> fit3

# Recommends 2a (4)
# Next cohort no DLT 

dose <-   c(2,2,2,
            3,3,3,
            4,4,4,
            5,5,5,
            6,6,6,
            6,6,6,
            5,5,5,
            4,4,4)
dlt <-    c(0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,
            0,1,0,
            1,1,1,
            0,1,1,
            0,0,0) # no DLT 
weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            0.9,1,1,
            0.81, 0.8, 0.6)

tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target,
           dlt = dlt, dose = dose, weight = weight) -> fit4

# Recommends 2b (5)
# Next cohort no DLT, DLT in previous cohort 

dose <-   c(2,2,2,
            3,3,3,
            4,4,4,
            5,5,5,
            6,6,6,
            6,6,6,
            5,5,5,
            4,4,4,
            5,5,5)
dlt <-    c(0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,
            0,1,0,
            1,1,1,
            0,1,1,
            0,0,1, # previous cohort DLT 
            0,0,0) # no DLT
weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            0.96,1,0.915,
            0.87, 0.86, 1,
            0.81, 0.8, 0.6)

tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target,
           dlt = dlt, dose = dose, weight = weight) -> fit5

# Recommends 2a (4)
# Next cohort 0 DLT 

dose <-   c(2,2,2,
            3,3,3,
            4,4,4,
            5,5,5,
            6,6,6,
            6,6,6,
            5,5,5,
            4,4,4,
            5,5,5,
            4,4,4)
dlt <-    c(0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,
            0,1,0,
            1,1,1,
            0,1,1,
            0,0,1,  
            0,0,0, 
            0,0,0)# no DLT  
weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            0.96, 0.95, 1,
            0.9, 0.89, 0.87,
            0.83, 0.82, 0.6)

tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target,
           dlt = dlt, dose = dose, weight = weight) -> fit6

# Recommends 2b (5)
# Next cohort 0 DLT

dose <-   c(2,2,2,
            3,3,3,
            4,4,4,
            5,5,5,
            6,6,6,
            6,6,6,
            5,5,5,
            4,4,4,
            5,5,5,
            4,4,4,
            5,5,5)
dlt <-    c(0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,
            0,1,0,
            1,1,1,
            0,1,1,
            0,0,1,  
            0,0,0, 
            0,0,0,
            0,0,0)# no DLT  
weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            0.97, 0.96, 0.94,
            0.9, 0.88, 0.85)

tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target,
           dlt = dlt, dose = dose, weight = weight) -> fit7

# Recommends 2b (5)
# Next cohort 0 DLT 

dose <-   c(2,2,2,
            3,3,3,
            4,4,4,
            5,5,5,
            6,6,6,
            6,6,6,
            5,5,5,
            4,4,4,
            5,5,5,
            4,4,4,
            5,5,5,
            5,5,5)
dlt <-    c(0,0,0,
            0,0,0,
            0,0,0,
            0,0,0,
            0,1,0,
            1,1,1,
            0,1,1,
            0,0,1,  
            0,0,0, 
            0,0,0,
            0,0,0,
            0,0,0)# no DLT  
weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            0.99, 0.97, 0.94,
            0.84, 0.83, 0.6)

weight <- c(1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            1,1,1,
            0.99, 0.97, 0.94,
            0.84, 0.83, 0.6)

tite.pocrm(prior.s = prior.s , prior.o = prior.o, target = target,
           dlt = dlt, dose = dose, weight = weight) -> fit8

###############################################################################

# Table code 
library(dplyr)
library(kableExtra)
cbind(1:12,
      c(2, 3, 4, 5, 6, fit1$dose.rec, fit2$dose.rec, fit3$dose.rec, 
        fit4$dose.rec, fit5$dose.rec, fit6$dose.rec, fit7$dose.rec),
      c(0, 0, 0, 0, 1, 3, 2, 0, "0*", 0, 0, 0),
      rbind(
        c(rep(NA, 8), 3), c(rep(NA, 8), 4), c(rep(NA, 8), 5), c(rep(NA, 8), 6),
        c(fit1$ptox.est, fit1$ord.prob, fit1$dose.rec) %>% round(2),
        c(fit2$ptox.est, fit2$ord.prob, fit2$dose.rec) %>% round(2),
        c(fit3$ptox.est, fit3$ord.prob, fit3$dose.rec) %>% round(2),
        c(fit4$ptox.est, fit4$ord.prob, fit4$dose.rec) %>% round(2),
        c(fit5$ptox.est, fit5$ord.prob, fit5$dose.rec) %>% round(2),
        c(fit6$ptox.est, fit6$ord.prob, fit6$dose.rec) %>% round(2),
        c(fit7$ptox.est, fit7$ord.prob, fit7$dose.rec) %>% round(2),
        c(fit8$ptox.est, fit8$ord.prob, fit8$dose.rec) %>% round(2)
      )
      ) %>% 
  data.frame() %>% 
  mutate(X2 = case_when(X2 == 2 ~ "0",
                        X2 == 3 ~ "1",
                        X2 == 4 ~ "2a",
                        X2 == 5 ~ "2b",
                        X2 == 6 ~ "3"),
         X12 = case_when(X12 == 2 ~ "0",
                        X12 == 3 ~ "1",
                        X12 == 4 ~ "2a",
                        X12 == 5 ~ "2b",
                        X12 == 6 ~ "3")) %>% 
  replace(is.na(.), " ") -> tabData



kable(tabData, "latex", booktabs = T, linesep = "", align = "c",
      col.names = c("Cohort", "Dose", "DLT", "-1", "0", "1", "2a", "2b", "3",
                    "1", "2", "Recommended Dose")) %>% 
  footnote(symbol = "This is my footnote") %>% 
  kable_styling(latex_options = c("striped", "scale_down"),
                position = "center", font_size = 9) %>% 
  add_header_above(c(" " = 3, "Estimated prob. tox" = 6, "Order prob" = 2)) -> Toytable

cat(Toytable)

###############################################################################
# Plots of example 
library(ggplot2)
# Data plot 

Dataplot12 <- data.frame(
  TNO = c(1:36),
  Cohort = rep(12, 36),
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,5,5,5,4,4,4,5,5,5,4,4,4,5,5,5,
           5,5,5),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,
          0,0,0),
  Weight = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
             0.99,0.97,0.94,0.84,0.83,0.6)
)

Dataplot11 <- data.frame(
  TNO = c(1:33),
  Cohort = rep(11, 33),
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,5,5,5,4,4,4,5,5,5,4,4,4,5,5,5),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0),
  Weight = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0.97,0.96, 
             0.94,0.9,0.88,0.85)
)

Dataplot10 <- data.frame(
  TNO = c(1:30),
  Cohort = rep(10, 30),
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,5,5,5,4,4,4,5,5,5,4,4,4),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,1,1,0,0,1,0,0,0,0,0,0),
  Weight = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0.96, 0.95, 1, 0.9, 0.89,
             0.87,0.83, 0.82, 0.6)
)

Dataplot9 <- data.frame(
  TNO = c(1:27),
  Cohort = rep(9, 27),
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,5,5,5,4,4,4,5,5,5),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,1,1,0,0,1,0,0,0),
  Weight = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0.96,1,0.915,0.87, 0.86, 1,
             0.81, 0.8, 0.6)
)

Dataplot8 <- data.frame(
  TNO = c(1:24),
  Cohort = rep(8, 24),
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,5,5,5,4,4,4),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,1,1,0,0,0),
  Weight = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0.9,1,1,0.81, 0.8, 0.6)
)

Dataplot7 <- data.frame(
  TNO = c(1:21),
  Cohort = rep(7, 21),
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,5,5,5),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,1,1),
  Weight = c(1,1,1,1,1,1,1,1,1,1,1,1,0.97, 1, 0.93,1, 1, 1,0.83, 1, 1)
)

Dataplot6 <- data.frame(
  TNO = c(1:18),
  Cohort = rep(6, 18),
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1),
  Weight = c(1,1,1,1,1,1,1,1,1,1,0.97,0.93,0.88,1,0.84,1, 1, 1)
)

Dataplot5 <- data.frame(
  TNO = c(1:15),
  Cohort = rep(5, 15),
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
  Weight = c(1,1,1,1,1,1,1,1,1,0.95, 0.91, 0.87, 0.82, 1, 0.6)
)

Dataplot <- c(rbind(Dataplot12, Dataplot11, Dataplot10,
                    Dataplot9, Dataplot8, Dataplot7,
                    Dataplot6, Dataplot5)) %>% data.frame() %>% 
  mutate(DLT = if_else(DLT == 1, "Yes", "No"),
         Dose = case_when(Dose == 2 ~ "0",
                          Dose == 3 ~ "1",
                          Dose == 4 ~ "2a",
                          Dose == 5 ~ "2b",
                          Dose == 6 ~ "3"))

Dataplot %>% 
  ggplot(aes(x = TNO, y = Cohort, col = Dose, shape = DLT, alpha = Weight))+
  geom_point(size = 5.5)+
  scale_alpha(guide = 'none')+
  scale_color_brewer(palette = "Dark2")+
  scale_y_continuous(trans = "reverse",breaks=seq(5,12,1))+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# dose recommendations over time plot 

Doseplot <- data.frame(
  TNO = 1:36, 
  Dose = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,6,6,6,5,5,5,4,4,4,5,5,5,4,4,4,5,5,5,
                   5,5,5),
  DLT = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,1,1,0,0,2,0,0,0,0,0,0,0,0,0,
          0,0,0)
) %>% 
  mutate(DLT = factor(DLT))

Doseplot %>% 
  ggplot(aes(x = TNO, y = Dose, shape = DLT))+
  geom_point(size = 5.5) +
  scale_shape_discrete(labels = c("No", "Yes", "After"))+
  scale_y_continuous(breaks = seq(1,6,1), labels = c("-1", "0", "1", "2a", "2b",
                                                     "3"))+
  labs(y = "Dose Level")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
