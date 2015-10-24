
# library(mtusRlocal)
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(dplyr)
library(knitr)
library(tidyr)

kol = brewer.pal(12, 'Set3')
seqDta = read.csv('data/seqDtaCompleteAggregate.csv')

# TimeClock(1440)
# seqDay5$Time_rec = cut(seqDay5$Time, breaks = c(0, 480, 780, 1080, 1440), labels = c('night', 'morning', 'afternoon', 'evening')) 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$sequence <- renderPlot({
    x = seq(from = min(input$time), to = max(input$time), by = 1)
    p = ggplot( filter(seqDta, diaryday == input$day & Time %in% c(x)), aes(x = Time, fill = factor(av), y = SharePerHour)) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~SEX, drop = T)
    print(p)
  })
  
  output$aggregate <- renderPlot({
    p = seqDta %>% group_by(diaryday, SEX, av, tot) %>% summarise(sdur = sum(n)) %>% group_by(av, diaryday, SEX) %>% mutate(mean= sdur / tot) %>% group_by(diaryday, SEX) %>% mutate(sum(mean)) %>% 
      ggplot( aes(x = diaryday, fill = factor(av), y = mean) ) + geom_bar(stat = "identity", width = 1) + scale_fill_manual(values=kol) + theme_minimal() + facet_grid(.~SEX)
    print(p)
  })
  
  output$table <- renderTable({
    filter(seqDta, diaryday == input$day) %>% group_by(av, diaryday, SEX, tot) %>% summarise(sumN = sum(n)) %>% mutate(total = sumN / tot) %>% group_by(SEX, diaryday) %>% mutate(sum(total)) %>% group_by() %>% select(av, SEX, total) %>% spread(SEX, total) 
    }) 
  
})








