
options(device='cairo')

library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)
library(magrittr)
library(dplyr)
library(ggplot2)





#start Shiny Server func
shinyServer(function(input, output, session) {
  
  
  
  getData <- reactive({
    
    if(input$Deaths_Wouded_Select == "Deaths" & input$PerCapita){
      newData <- gun_violence_2017_by_state %>% filter(population >= input$population_slider[1] & population < input$population_slider[2]) %>% 
        select(state, death_per_capita, population, party)  
    } 
    else if (input$Deaths_Wouded_Select == "Wounded" & input$PerCapita) {
      newData <- gun_violence_2017_by_state %>% filter(population >= input$population_slider[1] & population < input$population_slider[2]) %>% 
        select(state, wounded_per_capita, population, party) 
    }
    else if (input$Deaths_Wouded_Select == "Deaths") {
      newData <- gun_violence_2017_by_state %>% filter(population >= input$population_slider[1] & population < input$population_slider[2]) %>% 
        select(state, deaths, population, party) 
    }
    else {newData <- gun_violence_2017_by_state %>% filter(population >= input$population_slider[1] & population < input$population_slider[2]) %>% 
      select(state, wounded , population, party) }
  })
  
  getData2 <- reactive({
  if(input$Deaths_Wouded_Select2 == "Wounded"){
    newData2 <- gun_violence_total_2017_pop %>%
      filter(state == input$SelectState) %>%
      group_by(state, month) %>%
      summarise(Wounded = sum(n_injured)) %>% mutate(MON = month.abb)
     
    
  } 
    else {newData2 <- gun_violence_total_2017_pop %>%
      filter(state == input$SelectState) %>%
      group_by(state, month) %>%
      summarise(Deaths = sum(n_killed)) %>% mutate(MON = month.abb)
    
    #newData2$MON <- factor(newData2$MON, levels = newData2$MON[order(newData2$month)])
    }
  
  })
  
  getData3 <- reactive({
    if(input$Deaths_Wouded_Select2 == "Wounded"){
      newData3 <- gun_violence_total_2017_pop %>%
        filter(state == input$SelectState2) %>%
        group_by(state, month) %>%
        summarise(Wounded = sum(n_injured))  %>% mutate(MON = month.abb)
      
      #newData3$MON <- factor(newData3$MON, levels = newData3$MON[order(newData3$month)])
      
    } 
    else {newData3 <- gun_violence_total_2017_pop %>% 
      filter(state == input$SelectState2) %>%
      group_by(state, month) %>%
      summarise(Deaths = sum(n_killed))%>% mutate(MON = month.abb)
    
    #newData3$MON <- factor(newData3$MON, levels = newData3$MON[order(newData3$month)])
    }
    
  })
  
 getModel1 <- reactive({
   ifelse(input$Dependent_Variable == "acc_death_child",  rpart.plot(tree_fit_child_death), 
          ifelse(input$Dependent_Variable == "acc_death_teens",  rpart.plot(tree_fit_teen_death),
                 ifelse(input$Dependent_Variable == "acc_inj_child",  rpart.plot(tree_fit_child_inJ),
                        rpart.plot(tree_fit_teen_inj))))
   
  # tree_fit_1 <- tree( X ~ perc + state + population , data = model_train)
  
 })
 
 getData4 <- reactive({
   
   ifelse(input$Dependent_Variable == "acc_death_child", newData4 <- model_data %>% select(perc, acc_death_child), 
          ifelse(input$Dependent_Variable == "acc_death_teens", newData4 <- model_data %>% select(perc,acc_death_teens) ,
                 ifelse(input$Dependent_Variable == "acc_inj_child", newData4 <-model_data %>% select(perc, acc_inj_child),
                      newData4 <- model_data %>% select(perc,acc_inj_teens) )))
   
   as.data.frame(newData4)
 })
  
  
#table output
   output$table1 <-  renderDT(
    getData(), options = list(lengthChange = FALSE)
   )
   
   output$table2 <- renderTable(
     getData2() %>% 
       mutate(month = month.abb)%>% 
       dplyr::select(1:3), 
     align = 'l',
     striped = TRUE
   )
   
   output$table3 <- renderTable(
     getData3() %>% 
       mutate(month = month.abb) %>% 
       dplyr::select(1:3), 
     align = 'l',
     striped = TRUE
   )
   
   output$table4 <- renderDT(
     gun_violence_2017_fil,colnames = c("Date","State", "City/County","Deaths","Wounded"), options = list(pageLength = 15)
   )
  
#end tableoutput
  
#Start graph output
  output$Plot1 <- renderPlot({
#get filtered data
  newData <- getData()
  
#create plot

  if(input$PartyBox  ){
     ggplot(newData, aes(x = reorder(state,-newData[[2]]), y = newData[[2]], fill= as.factor(party))) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual("legend", values = c("Republicans" = "red", "Democrats" = "blue")) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = "State", y = input$Deaths_Wouded_Select)
    
 
    
  } 
  else {
     ggplot(newData, aes(x = reorder(state,-newData[[2]]), y = newData[[2]], fill=state)) + geom_bar(stat = "identity") +
      theme(legend.position = "none",axis.text.x = element_text(angle = 60, hjust = 1)) + labs(x = "State", y = input$Deaths_Wouded_Select)
    
  }
 })

#end creat Plot1
#Create Plot2
output$Plot2 <- renderPlot({
  
  newData2 <- getData2()
  newData2$MON <- factor(newData2$MON, levels = newData2$MON[order(newData2$month)])
  newData3 <- getData3()
  newData3$MON <- factor(newData3$MON, levels = newData3$MON[order(newData2$month)])

  #newData4 <- rbind(newData2, newData3) %>% as.tibble()
  
  ggplot(newData2, aes(x = MON, y = newData2[[3]], group=1)) + geom_line() + geom_point() + 
    geom_line(data = newData3,color = "blue", aes(x = MON, y= newData3[[3]])) + 
    geom_point(data = newData3,color = "blue", aes(x = MON, y= newData3[[3]])) + 
    geom_line(data=gun_violence_NatAvg_Monthly,color="red", aes(x=MON,y= NatAvg))+
    geom_point(data=gun_violence_NatAvg_Monthly,color="red", aes(x=MON,y= NatAvg))+
    labs(x = "Month", y=input$Deaths_Wouded_Select2, title = "States vs. National Average")
 

  })

output$Plot3 <- renderPlot ({
  
  getModel1()
  
  
})

output$Plot4 <- renderPlot({
  
  newData4 <- getData4()
  
   
 

    ggplot(newData4, aes(x=newData4[[2]], y=newData4[[1]] ))+geom_point()+geom_smooth() + labs(xlab="Deaths/Injured a year per state", ylab= "Percent of Firearms Laws adopted by state" )
  
  
})

output$Plot5 <- renderPlot({
  
  newData4 <- getData4()
  
  PCs <- prcomp(newData4, center = TRUE, scale. = TRUE)
  
  biplot(PCs, cex = 1)
  
})
#Create Download out put for current data

# Downloadable csv of selected dataset ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste(Sys.time(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(getData(), file)
  }
 )
# End download csv of selected data set
# Download CSV from second tab
output$downloadData2 <- downloadHandler(
  filename = function() {
    paste(Sys.time(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(rbind(getData2(),getData3()), file)
  }
)

output$downloadData3 <- downloadHandler(
  filename = function() {
    paste(Sys.time(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(gun_violence_2017_fil, file)
  }
)

# Download Plot
  output$exportPlot1 = downloadHandler(
    filename = function() {paste(Sys.time(),"-plot.pdf", sep = "")},
    content = function(file) {
      ggsave(file, device = "pdf", width=11, height=8.5)
      
    }
  )
  output$exportPlot2 = downloadHandler(
    filename = function() {paste(Sys.time(),"-plot.pdf", sep = "")},
    content = function(file) {
      ggsave(file, device = "pdf", width=11, height=8.5)
      
    }
  )
})  















