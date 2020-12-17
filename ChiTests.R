# Creates the plot that shows the total percentage of s/m/i transitions and the lines with what would have been expected by chance
chiPlotsTotal = function(expertList, noviceList){
  surfacePercent <- 0
  meaningfulPercent <- -.125
  dataFrame <- data.frame(NovEx = character(), Meaningful = double(), Surface = double(), Invalid = double())
  
  usefulSlides <- c(3,4,5,6,7,8,9,10,11,12,13)
  expertTrans <- 54.92857
  noviceTrans <- 34.51282
  
  for(slideCount in usefulSlides){
    # print(names(pages)[[slideCount]])
    surfacePercent <- surfacePercent + length(pages[[slideCount]][["Surface"]]) * 3 / 72
    meaningfulPercent <- meaningfulPercent + length(pages[[slideCount]][["Meaningful"]])* 3 / 72
    
    for(expertCount in 1:length(expertList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",experts[[expertCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Meaningful"){
        dataFrame <- rbind(dataFrame, list(NovEx = "Expert",
                                           Meaningful = expertList[[expertCount]]$MeaningfulPercentage[[slideCount]],
                                           Surface = expertList[[expertCount]]$SurfacePercentage[[slideCount]],
                                           Invalid = 1 - expertList[[expertCount]]$SurfacePercentage[[slideCount]] - expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]))
      }
      # if(clickData$Validation[[slideCount]] == "Meaningful"){
      #   meaningful = expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]
      #   surface = expertList[[expertCount]]$SurfacePercentage[[slideCount]]
      #   invalid = 1 - expertList[[expertCount]]$SurfacePercentage[[slideCount]] - expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]
      #   meaningful <- meaningful * expertTrans
      #   surface <- surface * expertTrans
      #   invalid <- invalid * expertTrans
      #   dataFrame <- rbind(dataFrame, list(NovEx = "Expert", Meaningful = meaningful, Surface = surface, Invalid = invalid))
      # }
    } 
    for(noviceCount in 1:length(noviceList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",novices[[noviceCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Surface"){
        dataFrame <- rbind(dataFrame, list(NovEx = "Novice",
                                           Meaningful = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]],
                                           Surface = noviceList[[noviceCount]]$SurfacePercentage[[slideCount]],
                                           Invalid = 1 - noviceList[[noviceCount]]$SurfacePercentage[[slideCount]] - noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]]))
      }
      # if(clickData$Validation[[slideCount]] == "Meaningful"){
      #   meaningful = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]]
      #   surface = noviceList[[noviceCount]]$SurfacePercentage[[slideCount]]
      #   invalid = 1 - noviceList[[noviceCount]]$SurfacePercentage[[slideCount]] - noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]]
      #   meaningful <- meaningful * noviceTrans
      #   surface <- surface * noviceTrans
      #   invalid <- invalid * noviceTrans
      #   dataFrame <- rbind(dataFrame, list(NovEx = "Novice", Meaningful = meaningful, Surface = surface, Invalid = invalid))
      # }
    } 
  }
  
  surfacePercent <- surfacePercent / length(usefulSlides)
  meaningfulPercent <- meaningfulPercent / (length(usefulSlides) - 1)
  invalidPercent <- 1 - surfacePercent - meaningfulPercent
  print(surfacePercent)
  print(meaningfulPercent)
  
  if(nrow(dataFrame) > 0 ){
    dataFrame <- melt(dataFrame, id.var="NovEx")
    for(dataRow in 1:nrow(dataFrame)){
      if(dataFrame$NovEx[[dataRow]] == "Expert"){
        dataFrame$value[[dataRow]] <- dataFrame$value[[dataRow]] * expertTrans
      }else{
        dataFrame$value[[dataRow]] <- dataFrame$value[[dataRow]] * noviceTrans
      }
    }
    pdf(file = paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/ChiTests/Total.pdf"),
        width = 9,
        height = 6)
    plots <- ggplot(dataFrame, aes(x = variable, y = value)) 
    plots <- plots + geom_boxplot((aes(fill = NovEx))) 
    # plots <- plots + geom_segment(aes(x = 0.6, y = meaningfulPercent, xend = 1.4, yend = meaningfulPercent), linetype = "dashed")
    # plots <- plots + geom_segment(aes(x = 1.6, y = surfacePercent, xend = 2.4, yend = surfacePercent), linetype = "dashed")
    # plots <- plots + geom_segment(aes(x = 2.6, y = invalidPercent, xend = 3.4, yend = invalidPercent), linetype = "dashed")
    plots <- plots + expand_limits(y = c(0,NA))
    plots <- plots + xlab("Validation") + ylab("Transition Count")
    
    
    print(plots)
    dev.off()
  }
  a <- 0
}

# Creates plot for each slide showing s/m/i transitions and line showing what was expected by random
chiPlots = function(expertList, noviceList){
  for(slideCount in 1:nrow(expertList[[1]])){
    print(names(pages)[[slideCount]])
    surfacePercent <- length(pages[[slideCount]][["Surface"]]) * 3 / 72
    meaningfulPercent <- length(pages[[slideCount]][["Meaningful"]])* 3 / 72
    invalidPercent <- 1 - surfacePercent - meaningfulPercent
    
    chiPercentages <- c(meaningfulPercent, surfacePercent, invalidPercent)
    
    # dataFrame <- data.frame(NovEx = character(),SurMean = character(), Percentage = double())
    dataFrame <- data.frame(NovEx = character(), Meaningful = double(), Surface = double(), Invalid = double())
    for(expertCount in 1:length(expertList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",experts[[expertCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Meaningful"){
        dataFrame <- rbind(dataFrame, list(NovEx = "Expert",
                                           Meaningful = expertList[[expertCount]]$MeaningfulPercentage[[slideCount]],
                                           Surface = expertList[[expertCount]]$SurfacePercentage[[slideCount]],
                                           Invalid = 1 - expertList[[expertCount]]$SurfacePercentage[[slideCount]] - expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]))
        
      }
      a <- 0
    } 
    for(noviceCount in 1:length(noviceList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",novices[[noviceCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Surface"){
        dataFrame <- rbind(dataFrame, list(NovEx = "Meaningful",
                                           Meaningful = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]],
                                           Surface = noviceList[[noviceCount]]$SurfacePercentage[[slideCount]],
                                           Invalid = 1 - noviceList[[noviceCount]]$SurfacePercentage[[slideCount]] - noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]]))
        
      }
    } 
    if(nrow(dataFrame) > 0 ){
      dataFrame <- melt(dataFrame, id.var="NovEx")
      pdf(file = paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/ChiTests/",names(pages)[[slideCount]],".pdf"),
          width = 9,
          height = 6)
      plots <- ggplot(dataFrame, aes(x = variable, y = value)) 
      plots <- plots + geom_boxplot((aes(fill = NovEx)))  
      print(ggplot_build(plots)$data)
      
      plots <- plots + geom_segment(aes(x = 0.6, y = meaningfulPercent, xend = 1.4, yend = meaningfulPercent), linetype = "dashed")
      plots <- plots + geom_segment(aes(x = 1.6, y = surfacePercent, xend = 2.4, yend = surfacePercent), linetype = "dashed")
      plots <- plots + geom_segment(aes(x = 2.6, y = invalidPercent, xend = 3.4, yend = invalidPercent), linetype = "dashed")
      plots <- plots + expand_limits(y = c(0,NA))
      plots <- plots + xlab("Validation") + ylab("Percentage")
      
      print(plots)
      dev.off()
    }
    a <- 0
  }
  
}

# Runs a chi-squared test comparing expert transitions to the average found from novices for each slide 
chiTestToStudent = function(expertList){
  slidePercentages <- list("fruit.bmp" = c(0,0,1),
                           "intro.bmp" = c(0,0,1),
                           "0.bmp" = c(.31250,.22222,.46528),
                           "1.bmp" = c(.14394,.45317,.40289),
                           "2.bmp" = c(.22034,.41667,.36299),
                           "3.bmp" = c(.15207,.40100,.44693),
                           "4.bmp" = c(.17424,.42272,.40304),
                           "5.bmp" = c(.22222,.34211,.43567),
                           "6.bmp" = c(.16025,.42262,.41713),
                           "7.bmp" = c(.04478,.35294,.60228),
                           "8.bmp" = c(.09722,.56349,.33929),
                           "9.bmp" = c(.09545,.42335,.4812),
                           "10.bmp" = c(.07142,.31571,.61287),
                           "11.bmp" = c(0,0,1))
  for(slideCount in 1:nrow(expertList[[1]])){
    # print(names(pages)[[slideCount]])
    
    dataFrame <- data.frame(Meaningful = double(), Surface = double(), Invalid = double())
    for(expertCount in 1:length(expertList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",experts[[expertCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Meaningful"){
        dataFrame <- rbind(dataFrame, list(Meaningful = expertList[[expertCount]]$Meaningful[[slideCount]],
                                           Surface = expertList[[expertCount]]$Surface[[slideCount]],
                                           Invalid = expertList[[expertCount]]$Transitions[[slideCount]] - expertList[[expertCount]]$Surface[[slideCount]] - expertList[[expertCount]]$Meaningful[[slideCount]]))
      }
    } 
    if(nrow(dataFrame) > 0 ){
      print(names(pages)[[slideCount]])
      print(chisq.test(x = dataFrame, p = slidePercentages[[slideCount]]))
    }
    a <- 0
  }
}

# I don't think this is relevent, see chiTest()
chiTest2 = function(experList, NoviceList){
  for(slideCount in 1:nrow(expertList[[1]])){
    print(names(pages)[[slideCount]])
    surfacePercent <- length(pages[[slideCount]][["Surface"]]) * 3 / 72
    meaningfulPercent <- length(pages[[slideCount]][["Meaningful"]])* 3 / 72
    invalidPercent <- 1 - surfacePercent - meaningfulPercent
    
    chiPercentages <- c(meaningfulPercent, surfacePercent, invalidPercent)
    
    # dataFrame <- data.frame(NovEx = character(),SurMean = character(), Percentage = double())
    dataFrame <- data.frame(Meaningful = double(), Surface = double(), Invalid = double())
    for(expertCount in 1:length(expertList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",experts[[expertCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Meaningful"){
        tester <- c(Meaningful = expertList[[expertCount]]$Meaningful[[slideCount]],
                    Surface = expertList[[expertCount]]$Surface[[slideCount]],
                    Invalid = expertList[[expertCount]]$Transitions[[slideCount]] -expertList[[expertCount]]$Surface[[slideCount]] -expertList[[expertCount]]$Meaningful[[slideCount]])
        print(chisq.test(tester[x = c(1,3)], p = c(meaningfulPercent, 1 - meaningfulPercent)))
        print(chisq.test(tester[x = c(2,3)], p = c(surfacePercent, 1 - surfacePercent)))
        
        # chiTester <- chisq.test(x = tester, p = chiPercentages)
        # print(chiTester$p.value)
      }
      a <- 0
    } 
    if(nrow(dataFrame) > 0 ){
      # print(names(pages)[[slideCount]])
      # print(goodnessOfFitTest(x = dataFrame, p = chiPercentages))
      # print(chisq.test(x = dataFrame, p = chiPercentages))
    }
    a <- 0
  }
  
  for(slideCount in 1:nrow(expertList[[1]])){
    print(names(pages)[[slideCount]])
    
    surfacePercent <- length(pages[[slideCount]][["Surface"]]) * 3 / 72
    meaningfulPercent <- length(pages[[slideCount]][["Meaningful"]])* 3 / 72
    invalidPercent <- 1 - surfacePercent - meaningfulPercent
    
    chiPercentages <- c(meaningfulPercent, surfacePercent, invalidPercent)
    
    dataFrame <- data.frame(Meaningful = double(), Surface = double(), Invalid = double())
    for(noviceCount in 1:length(noviceList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",novices[[noviceCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Surface"){
        dataFrame <- rbind(dataFrame, list(Meaningful = noviceList[[noviceCount]]$Meaningful[[slideCount]],
                                           Surface = noviceList[[noviceCount]]$Surface[[slideCount]],
                                           Invalid = noviceList[[noviceCount]]$Transitions[[slideCount]] - noviceList[[noviceCount]]$Surface[[slideCount]] - noviceList[[noviceCount]]$Meaningful[[slideCount]]))
        tester <- c(Meaningful = noviceList[[noviceCount]]$Meaningful[[slideCount]],
                    Surface = noviceList[[noviceCount]]$Surface[[slideCount]],
                    Invalid = noviceList[[noviceCount]]$Transitions[[slideCount]] - noviceList[[noviceCount]]$Surface[[slideCount]] - noviceList[[noviceCount]]$Meaningful[[slideCount]])
        
        print(chisq.test(tester[x = c(1,3)], p = c(meaningfulPercent, 1 - meaningfulPercent)))
        print(chisq.test(tester[x = c(2,3)], p = c(surfacePercent, 1 - surfacePercent)))
        # chiTested <- chisq.test(x = tester, p = chiPercentages)
        # print(chiTested$p.value)
      }
    } 
    if(nrow(dataFrame) > 0 ){
      # print(names(pages)[[slideCount]])
      # print(chisq.test(x = dataFrame, p = chiPercentages))
    }
    a <- 0
  }
}

# Tests expert and novice deviation from what was expected by random for each slide
chiTest = function(expertList, noviceList){
  for(slideCount in 1:nrow(expertList[[1]])){
    # print(names(pages)[[slideCount]])
    surfacePercent <- length(pages[[slideCount]][["Surface"]]) * 3 / 72
    meaningfulPercent <- length(pages[[slideCount]][["Meaningful"]])* 3 / 72
    invalidPercent <- 1 - surfacePercent - meaningfulPercent
    
    chiPercentages <- c(meaningfulPercent, surfacePercent, invalidPercent)
    
    # dataFrame <- data.frame(NovEx = character(),SurMean = character(), Percentage = double())
    dataFrame <- data.frame(Meaningful = double(), Surface = double(), Invalid = double())
    for(expertCount in 1:length(expertList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",experts[[expertCount]],".csv")))
      # if(clickData$Validation[[slideCount]] == "Meaningful"){
      dataFrame <- rbind(dataFrame, list(Meaningful = expertList[[expertCount]]$Meaningful[[slideCount]],
                                         Surface = expertList[[expertCount]]$Surface[[slideCount]],
                                         Invalid = expertList[[expertCount]]$Transitions[[slideCount]] -expertList[[expertCount]]$Surface[[slideCount]] -expertList[[expertCount]]$Meaningful[[slideCount]]))
      # tester <- c(Meaningful = expertList[[expertCount]]$Meaningful[[slideCount]],
      #             Surface = expertList[[expertCount]]$Surface[[slideCount]],
      #             Invalid = expertList[[expertCount]]$Transitions[[slideCount]] -expertList[[expertCount]]$Surface[[slideCount]] -expertList[[expertCount]]$Meaningful[[slideCount]])
      # chiTester <- chisq.test(x = tester, p = chiPercentages)
      # print(chiTester$p.value)
      # print(chisq.test(c(Meaningful = expertList[[expertCount]]$Meaningful[[slideCount]], 
      #                     Surface = expertList[[expertCount]]$Surface[[slideCount]],
      #                     Invalid = expertList[[expertCount]]$Transitions[[slideCount]] -expertList[[expertCount]]$Surface[[slideCount]] -expertList[[expertCount]]$Meaningful[[slideCount]]), chiPercentages))
      # }
      a <- 0
    } 
    if(nrow(dataFrame) > 0 ){
      print(names(pages)[[slideCount]])
      # print(goodnessOfFitTest(x = dataFrame, p = chiPercentages))
      print(chisq.test(x = dataFrame, p = chiPercentages))
    }
    a <- 0
  }
  
  for(slideCount in 1:nrow(expertList[[1]])){
    # print(names(pages)[[slideCount]])
    
    surfacePercent <- length(pages[[slideCount]][["Surface"]]) * 3 / 72
    meaningfulPercent <- length(pages[[slideCount]][["Meaningful"]])* 3 / 72
    invalidPercent <- 1 - surfacePercent - meaningfulPercent
    
    chiPercentages <- c(meaningfulPercent, surfacePercent, invalidPercent)
    
    dataFrame <- data.frame(Meaningful = double(), Surface = double(), Invalid = double())
    for(noviceCount in 1:length(noviceList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",novices[[noviceCount]],".csv")))
      # if(clickData$Validation[[slideCount]] == "Surface"){
      dataFrame <- rbind(dataFrame, list(Meaningful = noviceList[[noviceCount]]$Meaningful[[slideCount]],
                                         Surface = noviceList[[noviceCount]]$Surface[[slideCount]],
                                         Invalid = noviceList[[noviceCount]]$Transitions[[slideCount]] - noviceList[[noviceCount]]$Surface[[slideCount]] - noviceList[[noviceCount]]$Meaningful[[slideCount]]))
      # tester <- c(Meaningful = noviceList[[noviceCount]]$Meaningful[[slideCount]],
      #                Surface = noviceList[[noviceCount]]$Surface[[slideCount]],
      #                Invalid = noviceList[[noviceCount]]$Transitions[[slideCount]] - noviceList[[noviceCount]]$Surface[[slideCount]] - noviceList[[noviceCount]]$Meaningful[[slideCount]])
      # chiTested <- chisq.test(x = tester, p = chiPercentages)
      # print(chiTested$p.value)
      # }
    } 
    if(nrow(dataFrame) > 0 ){
      print(names(pages)[[slideCount]])
      print(chisq.test(x = dataFrame, p = chiPercentages))
    }
    a <- 0
  }
}