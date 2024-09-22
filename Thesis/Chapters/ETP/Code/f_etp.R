#------------------------------------#
# Efficacy Transition Pathways       #
#------------------------------------#

## Program Details ---- 
# Author: Amit Patel
# Version number: 0.2 
# Version date: 9 Nov 2022

## Background ---- 
# This program aims to automatically produce efficacy transition pathways 

## Development history ---- 
# version 0.1, 07 November 2022: Initial development (developed in R 4.2.0)

# version 0.2, 09 November 2022: Added options to output key data that is used 
# in the plot as well as the option to output the data with x,y co-ordinates
# (this was introduced to help with the development of the app) 

## To do: ---- 
# Add in a custom legend to identify what each of the numbers are, Doesn't seem 
# to be an easy solution that would make it compatible/look good with varying 
# numbers of cohorts and cohort sizes 

## Required packages: ----
library(ggplot2)
library(tidyverse)

## Parameters: ----
# cohort.size -      number of patients in each cohort (cohort size)
# cohorts -          number of cohorts 
# obj.resp.rate -    target objective response rate (0-1)
# accept.prob -      acceptably probability level to make a go/no go decision 
#                    at final analysis (0-1)
# ppos.accept.prob - acceptably probability level for PPoS go/no go decision at
#                    interim (0-1)
# prior -            vector of parameters for beta(a,b) prior distribution,
#                    default is a beta(1,1) which is entered as c(1,1)
# cred.int.side -    specify either a "two.sided" or "one.sided " credible 
#                    interval
# cred.int.level -   specify credible interval level e.g. 95% (0.95)
# align -            specify the alignment of the plot options are either "left" 
#                    or "centre",  
# size -             changes size of the text on the plot default is 5 
# legend.off -       turn off the legend on the plot default is on == FALSE
# output.data -      function will return key data along with the plot, default 
#                    is off == FALSE
# verbose.data -     returns data with additional columns with x,y coordinates for 
#                    plotting, default is off == FALSE

# Function: ---- 
etp <- function(cohort.size, 
                cohorts, 
                obj.resp.rate, 
                accept.prob, 
                ppos.accept.prob, 
                prior = c(1,1),
                cred.int.side = c("two.sided", "one.sided"),
                cred.int.level = 0.95,
                align = c("centre", "left"),
                size = 5,
                legend.off = FALSE,
                output.data = FALSE, 
                verbose.data = FALSE){
  
  # Calculate total sample size
  N <- cohort.size*cohorts 
  # Extract prior parameters 
  a0 <- prior[1]
  b0 <- prior[2]
 
  # Function to calculate PPoS 
  PPOS <- function(n2, apost, bpost, obj.resp.rate, accept.prob){
    # If remaining number of patients is 0 don't need to calculate PPoS
    if(n2 == 0){
      PPOS = NA
    }
    else{
      Pr2 <- sapply(0:n2, 
                    function(i) (gamma(apost+bpost)*gamma(n2+1)*gamma(i+apost)*
                                   gamma(n2-i+bpost)) / 
                      (gamma(apost)*gamma(bpost)*gamma(i+1)*gamma(n2-i+1)*
                         gamma(n2+apost+bpost)) )
      
      B <- sapply(0:n2, function(i) 1-pbeta(obj.resp.rate, apost+i, bpost+n2-i))
      
      temp <- cbind(Pr2, B) %>% 
        data.frame() %>% 
        mutate(GO = if_else(B > accept.prob, 1, 0),
               PPP = Pr2*GO) 
      PPOS = sum(temp$PPP)
    }
    return(PPOS)
  }
  
  # Generate data frame with all the data required for the etp plot 
  df <- data.frame(
    # produces a vector of responses for each cumulative cohort
    r = rep(0:(cohort.size*cohorts), rev(c(rep(1:cohorts, each = cohort.size),
                                           cohorts)))) %>% 
    group_by(r) %>%
    mutate(
      # produces y co-ords for plot 
      y = cohorts-row_number(),
      # generic weighting for size of each rectangle on the plot 
      w = rep(1, length(r)),
      # Establish the specific cohort each row belongs to
      Cohort = 1+y,
      # Different fonts for the plot 
      bolditalic = rep('bold.italic', length(r)),
      bold = rep('bold', length(r)),
      italic = rep('italic', length(r)),
      # For each cohort n1 = how may patients with data, n2 = how many left to recruit
      n1 = Cohort * cohort.size,
      n2 = N - n1,
      # At interim parameters for probability calculations
      apost = a0 + r,
      bpost = b0 + n1 - r, 
      # Labels to identify what probability needs calculating and analysis stage
      Prob_label = if_else(Cohort == cohorts, 'PP', 'PPOS'),
      Analysis = if_else(Cohort == cohorts, 'Final', 'Interim'),
      # Calculates estimates 
      Estimate = signif(qbeta(p = 0.5, shape1 = apost, shape2 = bpost)*100, 2), 
      # Calculate two-sided credible interval
      LB = signif(qbeta(p = 0 + (1-cred.int.level)/2, shape1 = apost, 
                        shape2 = bpost)*100, 2) , 
      UB = signif(qbeta(p = 1 - (1-cred.int.level)/2, shape1 = apost, 
                        shape2 = bpost)*100, 2) ,
      # Calculate one-sided credible interval 
      OSB = signif(qbeta(p = 1-cred.int.level, shape1 = apost, shape2 = bpost)*100,
                  2)
    )%>% 
    # Ungroup and do probability calcs for each row 
    ungroup() %>% 
    rowwise() %>% 
    mutate(
      # Calculate PPoS 
      Prob = PPOS(n2 = n2, apost = apost, bpost = bpost, obj.resp.rate, 
                  accept.prob), 
      # If NA means that its the final cohort so need posterior prob 
      Prob = if_else(is.na(Prob),
                     round(pbeta(q = obj.resp.rate, shape1 = apost, 
                                 shape2 = bpost, lower.tail = F), 7),
                     Prob),
      Prob = round(Prob,3),
      # Determine what the decision outcome would be 
      Decision = case_when(Cohort == cohorts & Prob >= accept.prob ~ 'GO', 
                           Cohort == cohorts & Prob < accept.prob ~ 'No GO',
                           Cohort != cohorts & Prob >= ppos.accept.prob ~ 'GO', 
                           Cohort != cohorts & Prob < ppos.accept.prob ~ 'No GO'), 
      DecRules = factor(paste(Decision, Analysis, sep = " "), 
                        levels = c("GO Final", "No GO Final", "GO Interim",
                                   "No GO Interim"))
      )
  
  # Produce x coords based on alignment 
  if (align == "centre") {
    df <- df %>% 
      mutate(x = ifelse(y < cohorts, r+(cohorts-y)*(cohort.size/2), r),
             xmin = x - w/2,
             xmax = x + w/2)

  } 
  
  if (align == "left"){
    df <- df %>% 
      mutate(x = r +0.5,
             xmin = r + w - 1,
             xmax = r + w )
  }
  
  # Code to generate plot 
  plot <-   
    ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = y, ymax = y + 1)) +
    # plot rectangles for each response in each cohort 
    geom_rect(aes( colour = DecRules , lty = DecRules), fill= "white",
              size = 1) +
    # reverse the y axis 
    scale_y_reverse()+ 
    scale_color_manual("Decision Rules", 
                       values = c("dark green", "red", "dark green", "red"),
                       labels = c(bquote(P(theta >= .(obj.resp.rate*100) ~ '%') >=
                                           .(accept.prob)  :Go),
                                  bquote(P(theta >= .(obj.resp.rate*100)  ~ '%') <
                                           .(accept.prob) ~ ':No Go'),
                                  bquote(PPoS>= .(ppos.accept.prob) ~ ':Go'),
                                  bquote(PPoS< .(ppos.accept.prob) ~ ':No Go')),
                       drop = FALSE)+
    
    scale_linetype_manual("Decision Rules",
                          values = c(1,1,2,2),
                          labels = c(bquote(P(theta >= .(obj.resp.rate*100) ~ '%') >=
                                              .(accept.prob)  :Go),
                                     bquote(P(theta >= .(obj.resp.rate*100)  ~ '%') <
                                              .(accept.prob) ~ ':No Go'),
                                     bquote(PPoS>= .(ppos.accept.prob) ~ ':Go'),
                                     bquote(PPoS< .(ppos.accept.prob) ~ ':No Go')),
                          drop = FALSE)+
  
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          legend.spacing =  unit(0.00, 'cm'))
    
    if(cred.int.side == "one.sided"){
      plot <- plot +
        geom_text(aes(label=r, x = x , y = y+0.2, fontface = bolditalic), 
                  size = size, colour = '#313639')+
        
        geom_text(data = df %>% filter(Prob_label == 'PPOS'),
                  aes(label=Prob, x = x, y = y +0.4, fontface = bold),
                  colour = 'blue', size = size)+
        
        geom_text(data = df %>% filter(Prob_label == 'PP'),
                  aes(label=Prob, x = x, y = y +0.4, fontface = bold), 
                  colour = 'black', size = size)+
        
        geom_text(aes(label=paste0(Estimate, '%'), x = x, y = y +0.6), 
                  size = size) + 
        
        geom_text(aes(label = paste0('(', OSB,'%', ')'), x = x, y = y + 0.8 ,
                      fontface = italic), size = size)
      
      data <- df %>% 
        mutate(CrI = paste('(', OSB,'%', ')', sep = ''))

    }
  if(cred.int.side == "two.sided"){
    plot <- plot +
      geom_text(aes(label=r, x = x , y = y+0.1, fontface = bolditalic), 
                size = size, colour = '#313639')+
      
      geom_text(data = df %>% filter(Prob_label == 'PPOS'),
                aes(label=Prob, x = x, y = y +0.3, fontface = bold),
                colour = 'blue', size = size)+
      
      geom_text(data = df %>% filter(Prob_label == 'PP'),
                aes(label=Prob, x = x, y = y +0.3, fontface = bold), 
                colour = 'black', size = size)+
      
      geom_text(aes(label=paste0(Estimate, '%'), x = x, y = y +0.5), 
                size = size) + 
      geom_text(aes(label = paste0('(', LB,'%'), x = x, y = y + 0.7 ,
                    fontface = italic), size = size) + 
      geom_text(aes(label = paste0(UB,'%',')'), x = x, y = y + 0.9,
                    fontface = italic), size = size) 
    
    data <- df %>% 
      mutate(CrI = paste('(',LB,'%', ' - ', UB, '%', ')' , sep = ''))
  }
  
  if (isTRUE(legend.off)) {
    plot <- plot + 
      theme(legend.position="none")
  }
  
  if(isTRUE(verbose.data)){
    data <- data %>% 
      select(Cohort, n = n1, r, Analysis, Prob, Estimate, CrI, Decision,  
             x, xmin, xmax, y, apost, bpost, LB, UB, OSB) %>% 
      arrange(Cohort, r)
  }
  else{
    data <- data %>% 
      select(Cohort, n = n1, r, Analysis, Prob, Estimate, CrI, Decision) %>% 
      arrange(Cohort, r)
  }

  
  if(isTRUE(output.data)){
    return(list(ETP = plot, Data = data))
  }
  else{
    plot 
  }
  
}

################################################################################
# Examples 
# 
# etp(cohort.size = 3, cohorts = 6, obj.resp.rate = 0.4, accept.prob = 0.8,
#     ppos.accept.prob = 0.1, prior = c(1,1), cred.int.side = "two.sided",
#     cred.int.level = 0.95, align = "centre", size = 5 ) -> temp1
# 
# temp1
# 
# etp(cohort.size = 3, cohorts = 5, obj.resp.rate = 0.2, accept.prob = 0.8,
#     ppos.accept.prob = 0.01, prior = c(1,1), cred.int.side = "two.sided",
#     cred.int.level = 0.95, align = "centre", size = 5,
#     output.data = TRUE) -> temp2
# 
# temp2$ETP
# temp2$Data
# 
# etp(cohort.size = 3, cohorts = 5, obj.resp.rate = 0.2, accept.prob = 0.8,
#     ppos.accept.prob = 0.01, prior = c(1,1), cred.int.side = "two.sided",
#     cred.int.level = 0.95, align = "centre", size = 5,
#     output.data = TRUE, verbose.data = TRUE) -> temp3
# 
# temp3$ETP
# temp3$Data
# 
# etp(cohort.size = 3, cohorts = 6, obj.resp.rate = 0.2, accept.prob = 0.8,
#     ppos.accept.prob = 0.01, prior = c(1,1), cred.int.side = "one.sided",
#     cred.int.level = 0.8, align = "left", size = 4, legend.off = TRUE) -> temp4
# 
# temp4
# 
# etp(cohort.size = 3, cohorts = 6, obj.resp.rate = 0.2, accept.prob = 0.8,
#     ppos.accept.prob = 0.01, prior = c(1,1), cred.int.side = "one.sided",
#     cred.int.level = 0.8, align = "left", size = 4, legend.off = TRUE,
#     output.data = TRUE) -> temp5
# 
# temp5$ETP
# temp5$Data
# 
# etp(cohort.size = 3, cohorts = 6, obj.resp.rate = 0.2, accept.prob = 0.8,
#     ppos.accept.prob = 0.01, prior = c(1,1), cred.int.side = "one.sided",
#     cred.int.level = 0.8, align = "left", size = 4, legend.off = TRUE,
#     output.data = TRUE, verbose.data = TRUE) -> temp6
# 
# temp6$ETP
# temp6$Data
# 
# rm(temp1, temp2, temp3, temp4, temp5, temp6)
################################################################################
# Test code 
# Code for custom legend
# df_legend <- 
#   df %>% 
#   filter(r == N)
# 
# plot <- 
#   plot + 
#   geom_rect(data = df_legend, 
#             aes(xmin = xmin-1, xmax = xmax, ymin = 0, ymax = 0 + 1,
#                 colour = Decision,lty = Analysis), fill = 'white', size = 1) +
#   annotate("text",x = df_legend$x -0.5, y= 0 + 0.1, 
#            label = "Number of \nresponses", size = 2.5) + 
#   annotate("text",x = df_legend$x -0.5, y= 0 + 0.4, 
#            label = "Predicted Probability of \nSuccess (PPoS)/ \nPosterior Probability \n(PP)", size = 2.5) +
#   annotate("text",x = df_legend$x -0.5, y= 0 + 0.80, 
#            label = "Bayesian estimate of \nresponse rate \n(credible interval)",
#            size = 2.5) +
#   labs(caption = "")
