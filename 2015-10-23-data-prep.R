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

# day1
# filter(dta3, diaryday == 1)
dta = select(dta3, id = id, av = act_rec, time = duration)
dta = as.data.frame(dta)

seqDay = dta[rep(1:nrow(dta), dta[,'time'] ), -3] %>%
  group_by(id) %>% 
  mutate( Time = 1:n() ) 

seqDay

# unique(nchar(seqDay$id) ) 
seqDay$diaryday = substr(seqDay$id, start = 8, stop = 8) 
seqDay$pid = substr(seqDay$id, start = 1, stop = 7) 

seqDay = seqDay %>% group_by()
seqDay2 = seqDay %>% group_by(Time, av, diaryday) %>% summarise(n = n())

as.data.frame(filter(seqDay2, diaryday == 1)) 
head(seqDay2)

seqDay3 = seqDay2 %>% group_by(Time, diaryday) %>% mutate(tot = sum(n)) %>% mutate(SharePerHour= round( n / tot, 4) * 100)
df(filter(seqDay3, Time == 1))

seqDay3 = seqDay3 %>% group_by() 
min(seqDay3$Time)
max(seqDay3$Time)

timeframe = df( cbind(Time= 1:1440, hours = TimeClock(1:1440) ) )
head(timeframe)

seqDay4 = merge(seqDay3, timeframe, by = 'Time')
head(seqDay4)

seqDay4$hours

seqDay4 = seqDay4 %>% group_by()
seqDay5 = seqDay4 %>% select(av, Time, SharePerHour, diaryday)

seqDay5 = distinct(seqDay5)

# n_distinct(seqDay$pid)
# 4033 is the number of id !!! 

df( seqDay5 %>% filter(diaryday == 1) )

# days 

seqDay5 = seqDay5 %>% select(av, Time, SharePerHour, diaryday)

# write.csv(seqDay5, file = '/Users/giacomovagni/Rprojects/bcs_presentation/data/seqDay5.csv')

read.csv(file = '/Users/giacomovagni/Rprojects/bcs_presentation/data/seqDay5.csv')


# Day 1 
seqD1 = seqDay5 %>% filter(diaryday == 1) %>% select(av, Time, SharePerHour)
df(seqD1)
# seqD1 = distinct(seqD1)

# good 
df(seqD1)
seqD1 %>% group_by(Time) %>% mutate(sum(SharePerHour))
# 

kol = brewer.pal(12, 'Set3')
ggplot(seqD1, aes(x = Time, fill = factor(av), y = SharePerHour)) + 
  geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal()

library(shiny)

shinyApp(
  ui = fluidPage(
    sliderInput("time",
                "Choose the time of the day",
                min = 1,
                max = 1440,
                value = c(1,1440)),
    plotOutput("sequence")
  ),
  
  server = function(input, output) {
    output$sequence <- renderPlot({
      x = seq(from = min(input$time), to = max(input$time), by = 1)
      p = ggplot( seqD1[seqD1$Time %in% c(x), ] , aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal()
      print(p)
    })
  }, 
  options = list(height = 1000)
)

shinyApp(
  ui = fluidPage(
    sliderInput("timescale",
                "Choose the time of the day",
                min = 480,
                max = 1440,
                step = 10, 
                animate = animationOptions(interval=400, loop=TRUE), 
                value = 1),  
    plotOutput("sequence")), 
  
  server = function(input, output) {
    output$sequence <- renderPlot({
      # x = seq(from = min(input$time), to = max(input$time), by = 1)
      ggplot( seqD1[seqD1$Time %in% c(input$timescale), ] , aes(x = av, fill = factor(av), y = SharePerHour)) + geom_bar(stat="identity") + scale_fill_manual(values=kol) + theme_minimal() + ylim(0,50) + scale_x_discrete(drop=FALSE) 
      # print(p)
    })
  }, 
  options = list(height = 1000)
)


############ 
############ 

write.csv(seqD1, file = '/Users/giacomovagni/Rprojects/bcs_presentation/seqD1.csv')
shinyApp(
  ui = fluidPage(
    sliderInput("time",
                "Choose the time of the day",
                min = 1,
                max = 1440,
                value = c(1,1440)),
    plotOutput("sequence"), 
    
    sliderInput("timescale",
                "Choose the time of the day",
                min = 480,
                max = 1440,
                step = 10, 
                animate = animationOptions(interval=400, loop=TRUE), 
                value = 1),  
    plotOutput("barchart")), 
  
  server = function(input, output) {
    output$sequence <- renderPlot({
      x = seq(from = min(input$time), to = max(input$time), by = 1)
      p = ggplot( seqD1[seqD1$Time %in% c(x), ] , aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal()
      print(p)
    })
    
    output$barchart <- renderPlot({
      ggplot( seqD1[seqD1$Time %in% c(input$timescale), ] , aes(x = av, fill = factor(av), y = SharePerHour)) + geom_bar(stat="identity") + scale_fill_manual(values=kol) + theme_minimal() + ylim(0,50) + scale_x_discrete(drop=FALSE) 
    })
    
  }, 
  options = list(height = 1000)
) 

##########
##########
##########

library('animation')

dta = spread( select(seqD1, av, Time, SharePerHour), av, SharePerHour)
dta = t(dta) 
dta[-1,]

for(i in 1:1440){
  barplot(dta[-1,i], col = c(kol), main = paste('time', dta[1, i]))
  ani.pause()
}


des = c("TimeStamp")

saveHTML({
  ani.options(interval = 0.05, nmax = 50)
  for(i in 500:600){
    barplot(dta[-1,i], col = c(kol), main = paste('time', dta[1, i]), ylim = c(0,50))
    ani.pause()
  }
}, ani.width=600, ani.height=400, img.name = "unif_plot", 
imgdir = "unif_dir", htmlfile = "timefriday.html", 
description = des, title = "Time")




