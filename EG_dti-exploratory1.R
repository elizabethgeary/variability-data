# written by K. Garner and Elizabeth Geary, 2020
# this code reads in the DTI data, tidies it, plots boxplot and qqplots to 
# detect outliers and determine normality. We then remove outliers and save the
# remaining data as a csv file.

rm(list=ls())
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages and source data-wrangling functions
# --------------------------------------------------------------------------------
library(tidyverse)
source("KG_data-wrangling.R")
source("R_rainclouds.R")

# load data
# --------------------------------------------------------------------------------
fpath <- '~/Documents/THESIS/pathways-to-practice/dti-data/'
tracts <- list(c("Superior_Frontal_gyrus_dorsolateral_Left", "Anterior_cingulate_and_paracingulate_gyri_Left"), # amend this to contain which tracts you seek
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Anterior_cingulate_and_paracingulate_gyri_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Anterior_cingulate_and_paracingulate_gyri_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Anterior_cingulate_and_paracingulate_gyri_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Caudate_nucleus_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Caudate_nucleus_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Left"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Left", "Lenticular_nucleus_putamen_Right"),
               c("Superior_Frontal_gyrus_dorsolateral_Right", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Lenticular_nucleus_putamen_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Lenticular_nucleus_putamen_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Lenticular_nucleus_putamen_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Caudate_nucleus_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Caudate_nucleus_Right"),
               c("Anterior_cingulate_and_paracingulate_gyri_Right", "Caudate_nucleus_Left"),
               c("Anterior_cingulate_and_paracingulate_gyri_Left", "Caudate_nucleus_Right"))

sub.data <- GetDTIData(fpath, tracts)

sub.data$group <- as.factor(sub.data$group)
levels(sub.data$group) <- c("practice", "control")

sub.data$session <- as.factor(sub.data$session)
levels(sub.data$session) <- c("pre-training", "post-training")

sub.data$tract_names <- c("lDLPFC_lACC", "rDLPFC_rACC", "lDLPFC_rACC", "rDLPFC_lACC",
                          "lDLPFC_lCN", "rDLPFC_rCN", "lDLPFC_rCN", "rDLPFC_lCN",
                          "lDLPFC_lLNP", "rDLPFC_rLNP", "lDLPFC_rLNP", "rDLPFC_lLNP", 
                          "rACC_rLNP", "lACC_lLNP", "rACC_lLNP", "lACC_rLNP",
                          "lACC_lCN", "rACC_rCN", "rACC_lCN", "lACC_rCN")

sub.data$tract_names <- as.factor(sub.data$tract_names)

sub.data$sub <- as.factor(sub.data$sub)


summary <- sub.data %>% group_by(group, session) %>%
                        summarise(N=length(unique(sub)))
# group    session        N
# <fct>    <fct>      <int>
#   1 practice pre-trial   45
# 2 practice post-trial    39
# 3 control  pre-trial     45
# 4 control  post-trial    46


length(unique(sub.data)) == nrow(sub.data)
#[1] FALSE

#The FALSE result tells me that there is some suplicates

sum(duplicated(sub.data))
#[1] 140

#This tells me that there might be 140 cases of duplicated data for your reference I used this 
# https://stackoverflow.com/questions/16905425/find-duplicate-values-in-r

sub.data <- sub.data %>% distinct(sub, tract_names, .keep_all = TRUE)

s1.data <- sub.data %>% filter(session == 0)
s1.data$sub <- as.factor(s1.data$sub)
# because we lost some session 1 DTI data in the great back up miss of 2014, I am going to work out who does not have session 1 data, and I’ll add their session 2 data to this dataframe
missed.subs <- unique(sub.data$sub)[!(unique(sub.data$sub) %in% unique(s1.data$sub))]
s1.data <- rbind(s1.data, sub.data[sub.data$sub %in% missed.subs, ])

summary <- sub.data %>% group_by(group) %>%
  summarise(N=length(unique(sub)))
#N: int [1:2] 49, 47

sub.data %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))
#  sub group session tract_start tract_end FA tract_names
#1   0     0       0           0         0  0           0

#No NA FA values - perhaps its under something else? I chose this from one of the links you sent through
# the Sebastian Sauer one :)

which(sub.data$FA == 0.0000, arr.ind=TRUE)
# [1]  95 147 587 1060 1327 1328 1512

#This shows me the rows that have 0.0000 as an FA value - given I couldn't see anything for NA I thought 
#perhaps it could be 0.000 - it was I don't think you can have a 0.000 FA value surely? For this I was 
# having a bit of trouble when I was entering the '0.000' it kept giving me error messages so I had a google
# found this and it seemed to work - although I havent split it into groups I can see that theres 3 practice
# and 7 control.

s1.data <- s1.data[s1.data$FA > 0,]

#Remove the 7 outliers, I used this https://www.reddit.com/r/Rlanguage/comments/7mo56h/delete_entire_row_if_column_contains_zero/

#________________________________________________________________________
#Part 3

ggplot(s1.data, aes(FA, tract_names)) + geom_boxplot()
![boxplotimage] ("documents/THESIS/pathways-to-practice/boxplot.png")

#This shows me that there is a difference between the ipsilateral and contralateral tracts
# also that most of the ranges seem to be about the same size, as in there aren't any huge 
#big ranges or tiny ones. Also, that there appear to be a few outliers that might need to 
# be culled, not sure yet if they breach the 3 SD rule.

ggplot(s1.data, aes(sample = FA)) + stat_qq() + stat_qq_line() +
  facet_wrap (~tract_names)
![qqplotimage] ("documents/THESIS/pathways-to-practice/qqplotfw.png")

#This graph shows me that the data is fairly normal, there is a little bit of wiggling about
#but I don't think its anything major. 

s1.data [(s1.data$FA < mean(s1.data$FA) - 3*sd(s1.data$FA)) | (s1.data$FA > mean(s1.data$FA) + 3*sd(s1.data$FA)),]
# sub    group      session                                     tract_start
# 520  127 practice pre-training  Anterior_cingulate_and_paracingulate_gyri_Left
# 780  140 practice pre-training  Anterior_cingulate_and_paracingulate_gyri_Left
# 1019 202  control pre-training Anterior_cingulate_and_paracingulate_gyri_Right
# 1042 205  control pre-training       Superior_Frontal_gyrus_dorsolateral_Right
# 1339 219  control pre-training Anterior_cingulate_and_paracingulate_gyri_Right
# tract_end        FA tract_names
# 520                            Caudate_nucleus_Right 0.5643915    lACC_rCN
# 780                            Caudate_nucleus_Right 0.5664093    lACC_rCN
# 1019                            Caudate_nucleus_Left 0.6218959    rACC_lCN
# 1042 Anterior_cingulate_and_paracingulate_gyri_Right 0.2295450 rDLPFC_rACC
# 1339                            Caudate_nucleus_Left 0.6241918    rACC_lCN

mean(s1.data$FA) - 3*sd(s1.data$FA)
mean(s1.data$FA) + 3*sd(s1.data$FA)
#0.2431041 - 0.5617937

s1.data<- s1.data %>% filter(FA > (mean(FA) - (3*sd(FA))))
s1.data <- s1.data %>% filter(FA < (mean(FA) + (3*sd(FA))))
  
# I found this helped me to figure out how to apply the  code: https://stackoverflow.com/questions/43817485/finding-outliers-further-than-certain-standard-deviations-from-mean-for-a-data-f

library(cowplot)
p <- ggplot(s1.data, aes(x=tract_names, y=FA, fill = tract_names, colour = tract_names)) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0), adjust =2, trim =
                     TRUE) +
  geom_point(position=position_jitter(width=.15), size=.25) +
  geom_boxplot(aes(x = tract_names, y = FA), outlier.shape = NA,
               alpha = 0.3, width = .1, colour = "BLACK") +
  scale_y_continuous(limits=c(0.2,0.65)) + coord_flip() +
  ylab('FA') + xlab('connection') + theme_cowplot() + 
  guides(fill = FALSE, colour = FALSE) +
  theme(axis.title.x = element_text(face = "italic"))
p
# ggsave(paste(save.name, '.pdf', sep=''), plot = p, width=20, height=30, units="cm")
![raincloudplot] ("documents/THESIS/pathways-to-practice/raincloud.png")

save.image(file = dti_exploratory.RData)
