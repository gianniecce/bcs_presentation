library(mtusRlocal) # load the package 

# Extras package used
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(knitr)
library(RColorBrewer)
library(TraMineR)
library(foreign)
library(ggplot2)
########

source('/Users/giacomovagni/Rprojects/analysis_bcs/rscripts/source_timetolongfunction.R')

tb = function(x) table(x)
df = function(x) as.data.frame(x)
kol = brewer.pal(12, 'Set3')

load(file = '/Users/giacomovagni/Rprojects/analysis_bcs/data/2015_october_new/dta3.RData')
########

n_distinct(dta3$pid)
dta3 %>% group_by(SEX, diaryday) %>% summarise( n_distinct(pid) )

# how you calculate duration per act 
dta4 = select(dta3, duration, act_rec, diaryday, SEX, pid) 
dta4 %>% group_by(pid, act_rec, diaryday) %>% summarise(sdur = sum(duration)) %>% group_by(pid, diaryday) %>% mutate(sum(sdur))
# 
# beautiful 
ActAggregateMean = dta4 %>% group_by(act_rec, diaryday) %>% summarise(sdur = sum(duration) / 4033) %>% group_by(diaryday) %>% mutate(sum(sdur))
# 
ggplot(ActAggregateMean, aes(x = diaryday, fill = factor(act_rec), y = sdur)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal()

# by sex  
ActAggregateMeanSEX = dta4 %>% group_by(SEX) %>% mutate(n_sex = n_distinct(pid) ) %>% 
  group_by(act_rec, diaryday, SEX, n_sex) %>% summarise(sdur = sum(duration)) %>% 
  group_by(act_rec, diaryday, SEX) %>% mutate(mean= sdur / n_sex) %>% group_by(diaryday, SEX) %>% mutate(sum(mean))
# 
ggplot(ActAggregateMeanSEX, aes(x = diaryday, fill = factor(act_rec), y = mean)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~SEX)
