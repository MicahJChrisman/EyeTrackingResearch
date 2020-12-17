# Purpose of this file was to create plots that showed the amount of s/m/i transitions for each 1 second increment to see 
# what times to ignore due to seaching patterns. Eventually decided on two seconds. 

source("C:/Users/mchri/Desktop/R Stuff/S20_research/Surface_Or_Meaningful_Lists.R")
library(ggplot2)

# These are the people who were clearly a novice or expert, no middle ground
novices <- c("s1","s2","s5","s6","s7","s8","s12","s14","s15","s17","s18","s24","s26")
experts <- c("p1","p2","p3","p4","p5","p6","p8")

# Combines all things
kineticPlotsAll = function(){
  for(counter in 1:length(experts)){
    plotKinetic(experts[[counter]])
  }
  for(counter in 1:length(novices)){
    plotKinetic(novices[[counter]])
  }
  
  plotKineticTotal(experts)
  plotKineticTotal(novices)
}

# Adds all individual kinetic plots together 
plotKineticTotal = function(participantList){
  totalData <- list()
  
  for(participant in 1:length(participantList)){
    if(participantList == experts){
     dataFrame <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros",paste0(experts[[participant]],".csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
    }else if(participantList == novices){
      dataFrame <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros",paste0(novices[[participant]],".csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
    }
    
    prevSlide <- dataFrame$slide[[1]]
    slideCount <- 1
    timer <- 0
    invalid <- 0
    surface <- 0
    meaningful <- 0
    transitionData <- data.frame(Meaningful = integer(), Surface = integer(), Invalid = integer())
    
    for(fixation in 1:nrow(dataFrame)){
      if(dataFrame$slide[[fixation]] == prevSlide){
        timer <- timer + dataFrame$fixationTime[[fixation]]
        if(dataFrame$Transition[[fixation]] == "Surface"){
          surface <- surface + 1
        }else if(dataFrame$Transition[[fixation]] == "Meaningful"){
          meaningful <- meaningful + 1
        }else if(dataFrame$Transition[[fixation]] == "Invalid"){
          invalid <- invalid + 1
        }
        if(timer > 1){
          transitionData <- rbind(transitionData, list(Meaningful = meaningful, Surface = surface, Invalid = invalid))
          meaningful <- 0
          surface <- 0
          invalid <- 0
        }
      }else{
        if(is.null(totalData[[prevSlide]])){
          totalData[[prevSlide]] <- transitionData
        }else{
          # totalData[[prevSlide]] <- rbind(totalData[[slideCount]], transitionData)
          
          if(nrow(transitionData) > nrow(totalData[[prevSlide]])){
            for(dataCount in 1:nrow(totalData[[prevSlide]])){
              totalData[[prevSlide]][dataCount,] <- totalData[[prevSlide]][dataCount,] + transitionData[dataCount,]
            }
            
            for(dataCount in nrow(totalData[[prevSlide]]):nrow(transitionData)){
             totalData[[prevSlide]] <- rbind(totalData[[prevSlide]],transitionData[dataCount,])
            }
          }else{
            for(dataCount in 1:nrow(transitionData)){
              totalData[[prevSlide]][dataCount,] <- totalData[[prevSlide]][dataCount,] + transitionData[dataCount,]
            }
          }
        }

        timer <- 0 
        transitionData <- transitionData[0,]
        prevSlide <- dataFrame$slide[[fixation]]
      }
    }
  } 
  
  for(slideCount in 1:13){
    pdf(file = paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/KineticPlots/Totals/",names(pages)[slideCount],"_",deparse(substitute(participantList)),".pdf"),
        width = 9,
        height = 6)
    Size <- 1:nrow(totalData[[slideCount]])
    plots <- ggplot(totalData[[slideCount]], mapping = aes(x = Size, y = Meaningful, color = "Meaningful")) + geom_line()
    plots <- plots + geom_line(totalData[[slideCount]], mapping = aes(x = Size, y = Surface, color = "Surface"))
    plots <- plots + geom_line(totalData[[slideCount]], mapping = aes(x = Size, y = Invalid, color = "Invalid"))
    plots <- plots + theme(legend.position = c(0.85, 0.8))
    plots$labels$colour <- "Transition Type"
    
    print(plots)
    dev.off()
  }
}


# Creates a kinetic plot for each slide to see at what point the transitions were occuring
plotKinetic = function(participant){
  dataFrame <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/",paste0(participant,".csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
  
  prevSlide <- dataFrame$slide[[1]]
  timer <- 0
  invalid <- 0
  surface <- 0
  meaningful <- 0
  transitionData <- data.frame(Meaningful = integer(), Surface = integer(), Invalid = integer())
  
  for(fixation in 1:nrow(dataFrame)){
    if(dataFrame$slide[[fixation]] == prevSlide){
      timer <- timer + dataFrame$fixationTime[[fixation]]
      if(dataFrame$Transition[[fixation]] == "Surface"){
        surface <- surface + 1
      }else if(dataFrame$Transition[[fixation]] == "Meaningful"){
        meaningful <- meaningful + 1
      }else if(dataFrame$Transition[[fixation]] == "Invalid"){
        invalid <- invalid + 1
      }
      if(timer > 2){
        transitionData <- rbind(transitionData, list(Meaningful = meaningful, Surface = surface, Invalid = invalid))
        meaningful <- 0
        surface <- 0
        invalid <- 0
      }
    }else{
      if(nrow(transitionData) > 0){
        pdf(file = paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/KineticPlots/",participant,"_",prevSlide,".pdf"),
            width = 9,
            height = 6)
        size <- 1:nrow(transitionData)
        plots <- ggplot(transitionData, mapping = aes(x = size, y = Meaningful)) + geom_point() + geom_line(color = "blue")
        plots <- plots + geom_line(transitionData, mapping = aes(x = size, y = Surface), color = "green")
        plots <- plots + geom_line(transitionData, mapping = aes(x = size, y = Invalid), color = "red")
        # plots <- plots + ggplot(transitionData, mapping = aes(x = size, y = Meaningful)) + geom_point() + geom_line()
        
        
        # plots <- ggplot(transitionData, aes(x = , y = Meaningful))
        # plots <- ggplot(dataFrame, aes(x = variable, y = value)) 
        # plots <- plots + geom_boxplot((aes(fill = NovEx))) 
        # plots <- plots + geom_segment(aes(x = 0.6, y = meaningfulPercent, xend = 1.4, yend = meaningfulPercent), linetype = "dashed")
        # plots <- plots + geom_segment(aes(x = 1.6, y = surfacePercent, xend = 2.4, yend = surfacePercent), linetype = "dashed")
        # plots <- plots + geom_segment(aes(x = 2.6, y = invalidPercent, xend = 3.4, yend = invalidPercent), linetype = "dashed")
        # plots <- plots + expand_limits(y = c(0,NA))
        # plots <- plots + xlab("Validation") + ylab("Percentage")
        
        print(plots)
        dev.off()
      }
      
      timer <- 0 
      transitionData <- transitionData[0,]
      prevSlide <- dataFrame$slide[[fixation]]
    }
  }
}

