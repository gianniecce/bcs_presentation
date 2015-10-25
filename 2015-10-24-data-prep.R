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

################ 
################ It is easier to separate everything - compute means and then put back together 
################ 

boys = filter(dta3, SEX == 1)
girls = filter(dta3, SEX == 2)

dta3%>% group_by(SEX) %>% summarise(n_distinct(pid))

################ #####################
################ Boys ################ 
################ #####################

# all days - boys 
dta = select(boys, id = id, av = act_rec, time = duration)
dta = as.data.frame(dta)

# sequence generation 
seqDay = dta[rep(1:nrow(dta), dta[,'time'] ), -3] %>%
  group_by(id) %>% 
  mutate( Time = 1:n() ) 

# retreive pid and diaryday  
seqDay$diaryday = substr(seqDay$id, start = 8, stop = 8) 
seqDay$pid = substr(seqDay$id, start = 1, stop = 7) 

seqDay = seqDay %>% group_by() 
# seqDay2 = seqDay %>% group_by(Time, av, diaryday) %>% summarise(n = n())

# mean by individual 
seqDay %>% group_by(pid, av, diaryday) %>% summarise(meanpid = n()) %>% group_by(pid, diaryday) %>% mutate(sum(meanpid))
# aggregate mean - correct / divided by the boys number 
seqDay %>% group_by(av, diaryday) %>% summarise(meanav = n() / 1790) %>% group_by(diaryday) %>% mutate(sum(meanav))

# aggregate by minutes of sequences ! 
seqDay2 = select(seqDay, av, Time, diaryday, pid)
#
seqDay3 = seqDay2 %>% group_by(Time, diaryday, av) %>% summarise(n = n()) 
# 
seqDay4 = seqDay3 %>% group_by(Time, diaryday) %>% mutate(tot = sum(n)) %>% mutate(SharePerHour= round( n / tot, 4) * 100)
seqDay4

# gender 
seqDay4$SEX = 'boys'
seqBoys = seqDay4 

# write.csv(seqBoys, file = '/Users/giacomovagni/Rprojects/bcs_presentation/data/seqBoys.csv')

################ #####################
################ girls ############### 
################ #####################

# all days - girls 
dta = select(girls, id = id, av = act_rec, time = duration)
dta = as.data.frame(dta)

# sequence generation 
seqDay = dta[rep(1:nrow(dta), dta[,'time'] ), -3] %>%
  group_by(id) %>% 
  mutate( Time = 1:n() ) 

# retreive pid and diaryday  
seqDay$diaryday = substr(seqDay$id, start = 8, stop = 8) 
seqDay$pid = substr(seqDay$id, start = 1, stop = 7) 

seqDay = seqDay %>% group_by() 
# seqDay2 = seqDay %>% group_by(Time, av, diaryday) %>% summarise(n = n())

# mean by individual 
seqDay %>% group_by(pid, av, diaryday) %>% summarise(meanpid = n()) %>% group_by(pid, diaryday) %>% mutate(sum(meanpid))
# aggregate mean - correct / divided by the boys number 
seqDay %>% group_by(av, diaryday) %>% summarise(meanav = n() / 1790) %>% group_by(diaryday) %>% mutate(sum(meanav))

# aggregate by minutes of sequences ! 
seqDay2 = select(seqDay, av, Time, diaryday, pid)
#
seqDay3 = seqDay2 %>% group_by(Time, diaryday, av) %>% summarise(n = n()) 
# 
seqDay4 = seqDay3 %>% group_by(Time, diaryday) %>% mutate(tot = sum(n)) %>% mutate(SharePerHour= round( n / tot, 4) * 100)
seqDay4

# gender 
seqDay4$SEX = 'girls'
seqGirls = seqDay4 

# write.csv(seqGirls, file = '/Users/giacomovagni/Rprojects/bcs_presentation/data/seqGirls.csv')

################ #####################
################ insime ############## 
################ #####################

seqDta = rbind(seqBoys, seqGirls)
# test = filter(seqDta, diaryday == 1)

kol = brewer.pal(12, 'Set3')
ggplot(seqDta, aes(x = Time, fill = factor(av), y = SharePerHour)) + facet_grid(.~SEX) +
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal()

# 
# write.csv(seqDta, file = '/Users/giacomovagni/Rprojects/bcs_presentation/data/seqDtaCompleteAggregate.csv')
# 


