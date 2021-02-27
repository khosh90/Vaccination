required_packages = c("shiny", "DT", "tidyverse","plotly","rstudioapi","png")

need_install <- required_packages[!(required_packages) %in% installed.packages()]
if(length(need_install)>0){
  install.packages(need_install)
}
#load packages
lapply(required_packages, require, character.only = TRUE)


#Setting up directory
#setting directory


dir_script <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir_script)
Data.age <- read.csv("agegroup.csv" , fileEncoding="UTF-8-BOM")
Ont.pop <- read.csv("Ontario.pop.age.csv ",  fileEncoding="UTF-8-BOM")



function(input, output) {
  
  url1 <- a("https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/prevention-risks/covid-19-vaccine-treatment/vaccine-rollout.html#a4",
            href="https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection/prevention-risks/covid-19-vaccine-treatment/vaccine-rollout.html#a4")
  output$tab <- renderUI({
    tagList("Link:", url1)
  }) 
  output$DistVac <- renderDT(
    DisVacData <- read.csv("Vacdist.csv", fileEncoding="UTF-8-BOM", sep = ",")
  )
  
  
  url2 <- a("https://www.canada.ca/en/public-services-procurement/services/procuring-vaccines-covid19.html",
            href="https://www.canada.ca/en/public-services-procurement/services/procuring-vaccines-covid19.html")
  output$tab2 <- renderUI({
    tagList("Link:", url2)
  }) 
  url3 <- a("https://www.canada.ca/en/health-canada/services/drugs-health-products/covid19-industry/drugs-vaccines-treatments/vaccines/moderna.html",
            href="https://www.canada.ca/en/health-canada/services/drugs-health-products/covid19-industry/drugs-vaccines-treatments/vaccines/moderna.html")
  output$tab3 <- renderUI({
    tagList("Link:", url3)
  }) 
  
  url4 <- a("https://www.canada.ca/en/health-canada/services/drugs-health-products/covid19-industry/drugs-vaccines-treatments/vaccines/pfizer-biontech.html",
            href="https://www.canada.ca/en/health-canada/services/drugs-health-products/covid19-industry/drugs-vaccines-treatments/vaccines/pfizer-biontech.html")
  output$tab4 <- renderUI({
    tagList("Link:", url4)
  }) 
  
  url5 <- a("https://www.healthline.com/health/r-nought-reproduction-number#covid-19-r-0",
            href="https://www.healthline.com/health/r-nought-reproduction-number#covid-19-r-0")
  output$tab5 <- renderUI({
    tagList("Link:", url5)
  }) 
  
  
  
  output$AgeG <- renderDT(
    AgeGroupData <- read.csv("Ontario.pop.age.csv", fileEncoding="UTF-8-BOM", sep = "," )
    
    
  )
  output$AgreeVac <- renderDT(
    
    AgreeVacData <- read.csv("VacAgrement.csv", fileEncoding="UTF-8-BOM", sep = ",")
    
    
  ) 
  
  
  ################ Calculation for Age
  observeEvent(input$goButton, { 
    set.seed(1234)
    
    
    percentValue = Ont.pop$Total / 14734014
    OntPopAge = cbind(Ont.pop, percentValue)
    
    popOnt = 14734014
    Underseventy =  OntPopAge$Total[1] + OntPopAge$Total[2] + OntPopAge$Total[3] +
      OntPopAge$Total[4] + OntPopAge$Total[5]+OntPopAge$Total[6]+OntPopAge$Total[7]+
      OntPopAge$Total[8]
    OverSeventi = OntPopAge$Total[9] + OntPopAge$Total[10]+ OntPopAge$Total[11]
    UnderseventiRatio = (Underseventy / 14734014)
    OverSeventiRatio = (OverSeventi / 14734014)
    
    
    p = input$popsize
    Tt = input$timeF    # 30 weeks
    Strategy = input$strategy    #c("byAge", "Random", "oneDoseToEveryone" )
    vaccineDaily = input$WeeklyVac
    
    
    underseventith =  round(p*UnderseventiRatio)
    Overseventith = round(p*OverSeventiRatio)
    
    Agepop = c(rep.int(1,underseventith), rep.int(2,Overseventith))
    
    Agepop = sample(Agepop, length(Agepop))
    Agepop = Agepop[1:p]
    
    infection.prob =  302573 / popOnt
    death.prob = 6918 / 302573
    
    ###################### my tibbles
    New.pop = tibble("Number" = NA ,  "Age" = NA , "Infected" = NA ,
                     "Vac1" = NA ,"Vac2" = NA , "Ful.Vac" = NA, "Occupation" = NA ,
                     "HighRisk" = NA, "AffectedWeek" = 0 , .rows = p) %>%
      mutate(Number = c(1:p), Vac1 = 0 ,Infected = rbinom(p,1,0.05), Vac2 = 0,
             Age = Agepop , Occupation = rbinom(p,1,0.3) )%>%mutate_all(.,funs(replace_na(., 44)))  # %>% remove_missing(na.rm = TRUE)
    #view(New.pop)
    
    WeeklyResults = tibble("Week" = 0 ,"Death" = 0, "CumulativeDath" = 0 , "Infected" = 0, "CumulativeInfected" = 0,
                           "Vaccinated" = 0, "CumulativeVaccinated" = 0 , .rows = Tt ) %>%
      mutate(Week = c(1:Tt))
    
    
    ################ my death function   
    
    checkDeath = function(infectedArg , ageArg){
      result = 0
      if(infectedArg == 1){
        if(ageArg == 2){
          if(runif(1)[1]<input$seniorDeath){      #death prob for old = 0.07
            result = 1
          }
        }else{
          if(runif(1)[1]<input$youngDeath){      #death prob for young = 0.02
            result = 1
          }
        }
      }
      return(result)
    }
    
    ##### Infected Function
    
    
    checkInfectedStatus = function(infectedArg , affectedArg, ageArg, occupationArg,
                                   vac1Arg, vac2Arg,infectedSumArg, timeArg){
      
      
      infectedResult = 0
      affectedResult = affectedArg + 0
      if(infectedArg == 1){            #if sick
        if(timeArg - affectedArg < 2.5){
          infectedResult = 1
        } else{
          if(timeArg < 4){
            if(runif(1)[1] < 0.6){
              infectedResult = 0
              
            }
          }
        }
      }else{        #if not sick     calculate    #######change base on true numbers
        
        checkGettingSick = (infectedSumArg/p)*(occupationArg+1)*(0.75 + (ageArg/4))*
          ((1-vac1Arg)*0.52+ 0.48)*(((1-vac2Arg)*0.88+ 0.12)) 
        if(checkGettingSick >0.4){
          checkGettingSick = ((checkGettingSick - 0.4)/4 ) + 0.4
        }
        if(checkGettingSick > runif(1)[1]){
          infectedResult = 1
          affectedResult = timeArg
        }
      }
      
      
      return(infectedResult)
      
    }
    
    
    
    
    
    # I have 4 strategies so I have to make them change my tibble first
    
    ################## Strategies
    
    if(Strategy == 1 ) { # ByAge1
      for (t in 1:Tt) {
        print(t)
        b = vaccineDaily
        a = 0
        infectedSum = 0
        for (rows in 1:nrow(New.pop)) {
          
          a = a+1
          infectedSum = infectedSum + New.pop[a,3]
          if(New.pop[a,2] == 2 & b>0 & New.pop[a,4]!= 1){
            New.pop[a,4] = 1
            b = b-1
            #New.pop%>% mutate(Vac1 =1)
          }
          
        }
        
        d = 0
        for (rows in 1:nrow(New.pop)) {
          d = d+1
          if(New.pop[d,2] == 2 & New.pop[d,4] == 1 & b>0 & New.pop[d,5]!= 1){
            New.pop[d,5] = 1
            b = b-1
            #New.pop%>% mutate(Vac1 =1)
          }
          
        }
        a = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          if(New.pop[a,2] == 1 & b>0 & New.pop[a,4]!= 1){
            New.pop[a,4] = 1
            b = b-1
            #New.pop%>% mutate(Vac1 =1)
          }
        }
        a = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          if(New.pop[a,2] == 1 & b>0 & New.pop[a,4] == 1 & New.pop[a,5]!= 1){
            New.pop[a,5] = 1
            b = b-1
          }
        }
        a = 0 
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          
          oldAffected =  New.pop[a,9]
          infectedResult = checkInfectedStatus(New.pop[a,3] ,New.pop[a,9] , New.pop[a,2] , New.pop[a,7],
                                               New.pop[a,4], New.pop[a,5],infectedSum, t)
          
          New.pop[a,3] = infectedResult
          if(oldAffected == 0 & infectedResult == 1){
            New.pop[a,9] = t
          }
          
          
        }
        
        a = 0
        deaths = 0
        infecteds = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          deaths = deaths + checkDeath(New.pop[a,3], New.pop[a,2])
          infecteds = infecteds + New.pop[a,3] 
          
        }
        WeeklyResults[t,2] = deaths
        WeeklyResults[t,4] = infecteds
        
      } # Time ends
    }   # Strategy Ends
    
    if(Strategy == 2 ) { # Age2
      for (t in 1:Tt) {
        print(t)
        b = vaccineDaily
        a = 0
        infectedSum = 0
        for (rows in 1:nrow(New.pop)) {
          
          a = a+1
          infectedSum = infectedSum + New.pop[a,3]
          if(New.pop[a,2] == 2 & b>0 & New.pop[a,4]!= 1){
            New.pop[a,4] = 1
            b = b-1
          }
          
        }
        
        a = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          if(New.pop[a,2] == 1 & b>0 & New.pop[a,4]!= 1){
            New.pop[a,4] = 1
            b = b-1
          }
        }
        
        d = 0
        for (rows in 1:nrow(New.pop)) {
          d = d+1
          if(New.pop[d,2] == 2 & New.pop[d,4] == 1 & b>0 & New.pop[d,5]!= 1){
            New.pop[d,5] = 1
            b = b-1
          }
          
        }
        a = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          if(New.pop[a,2] == 1 & b>0 & New.pop[a,4] == 1 & New.pop[a,5]!= 1){
            New.pop[a,5] = 1
            b = b-1
          }
        }
        a = 0 
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          
          oldAffected =  New.pop[a,9]
          infectedResult = checkInfectedStatus(New.pop[a,3] ,New.pop[a,9] , New.pop[a,2] , New.pop[a,7],
                                               New.pop[a,4], New.pop[a,5],infectedSum, t)
          
          New.pop[a,3] = infectedResult
          if(oldAffected == 0 & infectedResult == 1){
            New.pop[a,9] = t
          }
          
          
        }
        
        a = 0
        deaths = 0
        infecteds = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          deaths = deaths + checkDeath(New.pop[a,3], New.pop[a,2])
          infecteds = infecteds + New.pop[a,3] 
          
        }
        WeeklyResults[t,2] = deaths
        WeeklyResults[t,4] = infecteds
        
      } # Time ends
    }   # Strategy Ends
    
    if(Strategy == 3 ) { # Occupation 2 dose to occu then other people
      for (t in 1:Tt) {
        print(t)
        b = vaccineDaily
        a = 0
        infectedSum = 0
        for (rows in 1:nrow(New.pop)) {
          
          a = a+1
          infectedSum = infectedSum + New.pop[a,3]
          if(New.pop[a,7] == 1 & b>0 & New.pop[a,4]!= 1){
            New.pop[a,4] = 1
            b = b-1
          }
          
        }
        
        d = 0
        for (rows in 1:nrow(New.pop)) {
          d = d+1
          if(New.pop[d,7] == 1 & New.pop[d,4] == 1 & b>0 & New.pop[d,5]!= 1){
            New.pop[d,5] = 1
            b = b-1
          }
          
        }
        a = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          if(New.pop[a,7] == 0 & b>0 & New.pop[a,4]!= 1){
            New.pop[a,4] = 1
            b = b-1
          }
        }
        a = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          if(New.pop[a,7] == 0 & b>0 & New.pop[a,4] == 1 & New.pop[a,5]!= 1){
            New.pop[a,5] = 1
            b = b-1
          }
        }
        a = 0 
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          
          oldAffected =  New.pop[a,9]
          infectedResult = checkInfectedStatus(New.pop[a,3] ,New.pop[a,9] , New.pop[a,2] , New.pop[a,7],
                                               New.pop[a,4], New.pop[a,5],infectedSum, t)
          
          New.pop[a,3] = infectedResult
          if(oldAffected == 0 & infectedResult == 1){
            New.pop[a,9] = t
          }
          
          
        }
        
        a = 0
        deaths = 0
        infecteds = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          deaths = deaths + checkDeath(New.pop[a,3], New.pop[a,2])
          infecteds = infecteds + New.pop[a,3] 
          
        }
        WeeklyResults[t,2] = deaths
        WeeklyResults[t,4] = infecteds
        
      } # Time ends
    }   # Strategy Ends
    
    
    if(Strategy == 4 ) {   # Occupation 2  one dose first starts with occu and then to 
      #other. then dose two to occu again then other
      for (t in 1:Tt) {
        print(t)
        b = vaccineDaily
        a = 0
        infectedSum = 0
        for (rows in 1:nrow(New.pop)) {
          
          a = a+1
          infectedSum = infectedSum + New.pop[a,3]
          if(New.pop[a,7] == 1 & b>0 & New.pop[a,4]!= 1){
            New.pop[a,4] = 1
            b = b-1
          }
          
        }
        
        a = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          if(New.pop[a,7] == 0 & b>0 & New.pop[a,4]!= 1){
            New.pop[a,4] = 1
            b = b-1
          }
        }
        
        d = 0
        for (rows in 1:nrow(New.pop)) {
          d = d+1
          if(New.pop[d,7] == 1 & New.pop[d,4] == 1 & b>0 & New.pop[d,5]!= 1){
            New.pop[d,5] = 1
            b = b-1
          }
          
        }
        a = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          if(New.pop[a,7] == 0 & b>0 & New.pop[a,4] == 1 & New.pop[a,5]!= 1){
            New.pop[a,5] = 1
            b = b-1
          }
        }
        a = 0 
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          
          oldAffected =  New.pop[a,9]
          infectedResult = checkInfectedStatus(New.pop[a,3] ,New.pop[a,9] , New.pop[a,2] , New.pop[a,7],
                                               New.pop[a,4], New.pop[a,5],infectedSum, t)
          
          New.pop[a,3] = infectedResult
          if(oldAffected == 0 & infectedResult == 1){
            New.pop[a,9] = t
          }
          
          
        }
        
        a = 0
        deaths = 0
        infecteds = 0
        for (rows in 1:nrow(New.pop)) {
          a = a+1
          deaths = deaths + checkDeath(New.pop[a,3], New.pop[a,2])
          infecteds = infecteds + New.pop[a,3] 
          
        }
        WeeklyResults[t,2] = deaths
        WeeklyResults[t,4] = infecteds
        
      } # Time ends
    }   # Strategy Ends
    
    
    #view(New.pop)
    #view(WeeklyResults)
    
    output$plot1 <- renderPlotly({
      
      
      plotInfected =  WeeklyResults %>% ggplot(aes(x = Week , y = Infected))+
        geom_line(col = "blue")+
        geom_point(col = "black")+
        xlab("Week")+
        ylab("Infected")+
        ggtitle("Infected Per Week")+
        ylim(0, input$popsize)
      ggplotly(plotInfected) %>% config(displayModeBar = FALSE)
    })
    
    output$plot2 <- renderPlotly({
      
      
      plotDeath = WeeklyResults %>% ggplot(aes(x = Week , y = Death))+
        geom_line(col = "red")+
        geom_point(col = "black")+
        xlab("Week")+
        ylab("Death")+
        ggtitle("Death Per Week")+
        ylim(0, input$popsize/5)
      ggplotly(plotDeath) %>% config(displayModeBar = FALSE)
    })
    
  }) #obsereveents end
  
  output$barPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    plot(x="Provinces",y="number of cases in a perion of time",xlim = c(1,31),ylim = c(1,10000))
    
    
  })
  
  
  
  output$Cumulative <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    plot(x="Date",y="cumulative vaccine doses for each province",xlim = c(1,31),ylim = c(1,10000))
    
    
  })
  url8 <- a("https://www.linkedin.com/in/elhamkalantari/", href=" https://www.linkedin.com/in/elhamkalantari/")
  output$linkedin <- renderUI({
    tagList("Link:", url8)
  })
  
  
  
  
}