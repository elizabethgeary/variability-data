# written by K. Garner and Elizabeth Greary, 2020
# this code binds datasets and performs regression analyses

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to the location of this file

# Load packages, source function files and define path variables
library(tidyverse)
library(cowplot)
library(interactions)
library(purrr)
library(broom)
library(dplyr)
source("R_rainclouds.R")
source("KG_data-wrangling.R")

factors <- read.csv('facsol.csv')
behavioural <- read.csv('behav.csv')

factors$sub <- as.factor(factors$sub)
behavioural$sub <- as.factor(behavioural$sub)

reg.dat <- inner_join(factors, behavioural,  by=c('sub')) %>%
  unique() %>% na.omit
reg.dat$sub <- as.factor(reg.dat$sub)

write.csv(reg.dat, paste(file = "regres.csv", sep="/"), row.names=  FALSE)

#------------------------------------------------------------------------------

#multiple regression of three factors against multi1
mod1 <- lm (formula = multi1 ~ ipsi_rightput*Dorsal_acc_contraput*ACC_caud_contracaud + medm1,
        data = reg.dat)
summary(mod1)
#Overall model insignificant, adjusted R2 0.04405, F(8,75) = 1.478, p = 0.180
#significant interaction of ipsirightput and acc caud contracaud

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.87431 -0.32758 -0.09402  0.21863  1.46330 
# 
# Coefficients:
#                                                         Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                                             1.22630    0.19521   6.282 1.99e-08 ***
#   ipsi_rightput                                        -0.03330    0.05514  -0.604   0.5476    
# Dorsal_acc_contraput                                   -0.09104    0.05709  -1.595   0.1150    
# ACC_caud_contracaud                                    -0.01116    0.05416  -0.206   0.8374    
# medm1                                                  -0.19334    0.16507  -1.171   0.2452    
# ipsi_rightput:Dorsal_acc_contraput                      0.04234    0.05145   0.823   0.4131    
# ipsi_rightput:ACC_caud_contracaud                      -0.18353    0.07672  -2.392   0.0193 *  
#   Dorsal_acc_contraput:ACC_caud_contracaud             -0.01380    0.06067  -0.228   0.8206    
# ipsi_rightput:Dorsal_acc_contraput:ACC_caud_contracaud  0.05934    0.07021   0.845   0.4007    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4742 on 75 degrees of freedom
# Multiple R-squared:  0.1362,	Adjusted R-squared:  0.04405 
# F-statistic: 1.478 on 8 and 75 DF,  p-value: 0.1796

### PLOT Interaction
library(interactions)
interact_plot(mod1, pred=ipsi_rightput, modx=ACC_caud_contracaud, plot.points = TRUE)

### Model 2
mod2 <- lm (formula = multi2 ~ ipsi_rightput*Dorsal_acc_contraput*ACC_caud_contracaud + medm2,
            data = reg.dat)
summary (mod2)
#Overall model significant, adjusted R2 0.1148, F(8,75) = 2.346, p = 0.026
#Sig. main effect of mediator, and sig. interaction of ipsi_rightput - ACC_caudcontracaud

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.83120 -0.23987 -0.05419  0.16320  1.45946 
# 
# Coefficients:
#                                                        Estimate   Std. Error t value Pr(>|t|)
# (Intercept)                                             1.255843   0.177417   7.078 6.62e-10***
# ipsi_rightput                                          -0.021032   0.046439  -0.453  0.65194
# Dorsal_acc_contraput                                   -0.061359   0.047785  -1.284  0.20307
# ACC_caud_contracaud                                     0.009560   0.044995   0.212  0.83231
# medm2                                                  -0.248679   0.122001  -2.038  0.04504*
# ipsi_rightput:Dorsal_acc_contraput                      0.034894   0.043966   0.794  0.42989
# ipsi_rightput:ACC_caud_contracaud                      -0.211521   0.064198  -3.295  0.00151**
# Dorsal_acc_contraput:ACC_caud_contracaud               -0.004176   0.050843  -0.082  0.93476
# ipsi_rightput:Dorsal_acc_contraput:ACC_caud_contracaud  0.051424   0.059145   0.869  0.38737
# 
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3974 on 75 degrees of freedom
# Multiple R-squared:  0.2001,	Adjusted R-squared:  0.1148 
# F-statistic: 2.346 on 8 and 75 DF,  p-value: 0.0262

# PLOT 
interact_plot(mod2, pred=ipsi_rightput, modx=ACC_caud_contracaud, plot.points = TRUE)

