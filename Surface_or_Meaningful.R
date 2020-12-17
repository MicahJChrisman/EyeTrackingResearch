# i think that this is outdated and ModifiedDataPreparer.R is needed. 

setwd(dir = "C:/Users/mchri/Desktop/R Stuff/S20_research/wd")
# source("C:/Users/mchri/Desktop/R Stuff/S20_research/NovExpDecodedPics.R")
source("C:/Users/mchri/Desktop/R Stuff/S20_research/Surface_Or_Meaningful_Lists.R")

# These are the people who were clearly a novice or expert, no middle ground
novices <- c("s1","s2","s5","s6","s7","s8","s12","s14","s15","s17","s18","s24","s26")
experts <- c("p1","p2","p3","p4","p5","p6","p7","p8")

AOIs <- LETTERS[1:9]

fixationAll = function(zeros = TRUE){
  fixationsMeaning(experts, zeros)
  fixationsMeaning(novices, zeros)
}

fixationsMeaning = function(lists, zeros){
  for(counter in 1:length(lists)){
    if(zeros){
      fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/",lists[counter],".csv")),
                                header = TRUE, sep=",", stringsAsFactors = FALSE)
    }else{
      fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros/",lists[counter],".csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
    }

    surMean <- c("Invalid")
    pastSurface <- c()
    pastMeaningful <- c()
    # pastFixation <- fixationData[["correctedAOI"]][[1]]
    
    for(fixationNumber in 1:nrow(fixationData)){
      surface <- c()
      meaningful <- c()
      currentFixation <- fixationData[["correctedAOI"]][[fixationNumber]]
      currentSlide <- pages[[fixationData[["slide"]][[fixationNumber]]]]
      for(surCount in 1:length(currentSlide[["Surface"]])){
        if(length(currentSlide[["Surface"]]) > 0){
          splits <- strsplit(currentSlide[["Surface"]][[surCount]],"")
        }else{
          break
        }
        for(value in 1:3){
          if(currentFixation == match(splits[[1]][[value]],AOIs)){
            surface <- c(surface, currentSlide[["Surface"]][[surCount]])
          }
        }
      }

      for(meanCount in 1:length(currentSlide[["Meaningful"]])){
        if(length(currentSlide[["Meaningful"]]) > 0){
          splits <- strsplit(currentSlide[["Meaningful"]][[meanCount]],"")
        }else{
          break
        }
        for(value in 1:3){
          if(currentFixation == match(splits[[1]][[value]],AOIs)){
            meaningful <- c(meaningful, currentSlide[["Meaningful"]][[meanCount]])
          }
        }
      }
      
      if(fixationNumber > 1){
        if(fixationData[["slide"]][[fixationNumber]] != fixationData[["slide"]][[fixationNumber -1]]){
          surMean <- c(surMean, "Invalid")
        }else{
          if(length(surface) > 0 ){
            for(surCount in 1:length(surface)){
              if(surface[surCount] %in% pastSurface){
                surMean <- c(surMean,"Surface")
              }
              # if(match(surface[[surCount]],AOIs) == pastFixation){
              #   surMean <- c(surMean,"Surface")
              # }
            }
          }
          if(length(meaningful) > 0 ){
            for(meanCount in 1:length(meaningful)){
              if(meaningful[meanCount] %in% pastMeaningful){
                surMean <- c(surMean,"Meaningful")
              }
              # if(match(meaningful[[meanCount]],AOIs) == pastFixation){
              #   surMean <- c(surMean,"Meaningful")
              # }
            }
          }
          if(length(surMean) < fixationNumber){
            surMean <- c(surMean,"Invalid")
          }
        }
      }
      pastMeaningful <- c(meaningful)
      pastSurface <- c(surface)
      # print(counter)
      # pastFixation <- currentFixation
    }
    # cbind(fixationData, "Transition" = surMean)
    fixationData$X <- NULL
    if(zeros){
      write.csv(cbind(fixationData,"Transition" = surMean), file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified",paste0(lists[counter],".csv")))
    }else{
      write.csv(cbind(fixationData,"Transition" = surMean), file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros",paste0(lists[counter],".csv")))
    }
  }
}


clicksMeaning = function(participantList){
  if(participantList == experts){
    validationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/ExValidation.csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
  }else if(participantList == novices){
    validationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/NovValidation.csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
  }
  
  for(counter in 1:length(participantList)){
    surMean <- c()
    clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/Click_Raw/",toupper(participantList[counter])," .csv")),
                         header = TRUE, sep=",", stringsAsFactors = FALSE)
    for(page in 1:length(pages)){
      for(surface in 1:length(pages[[page]][["Surface"]])){
        if(length(pages[[page]][["Surface"]]>0)){
          splitClicks <- strsplit(pages[[page]][["Surface"]][[surface]],"")
        }else{
          break
        }
        surfaceClicks <- 0
        for(count in 1:3){
          splitToNum <- match(splitClicks[[1]][count],AOIs)
          for(count2 in 3:5){
            if(!is.na(clickData[[count2]][page]) && clickData[[count2]][page] == splitToNum){
              surfaceClicks <- surfaceClicks + 1
              break
            }
          }
          if(surfaceClicks != count){
            break
          }
        }
        if(surfaceClicks == 3){
          surMean <- c(surMean,"Surface")
          break
        }
      }
      for(meaningful in 1:length(pages[[page]][["Meaningful"]])){
        if(length(pages[[page]][["Meaningful"]] > 0)){
          splitClicks <- strsplit(pages[[page]][["Meaningful"]][[meaningful]],"")
        }else{
          break
        }
        meaningfulClicks <- 0
        for(count in 1:3){
          splitToNum <- match(splitClicks[[1]][count],AOIs)
          for(count2 in 3:5){
            if(!is.na(clickData[[count2]][page]) && clickData[[count2]][page] == splitToNum){
              meaningfulClicks <- meaningfulClicks + 1
              break
            }
          }
          if(meaningfulClicks != count){
            break
          }
        }
        if(meaningfulClicks == 3){
          surMean <- c(surMean,"Meaningful")
          break
        }
      }
      if(length(surMean) != page){
        surMean <- c(surMean,"Invalid")
      }
    }
    clickData <- cbind(clickData,"Transition" = surMean)
    validationVector <- validationData[[counter]]
    validationVector[1] <- clickData$Transition[[1]]
    validationVector[2] <- clickData$Transition[[2]]
    clickData <- cbind(clickData, "Validation" = validationVector)
    clickData$X <- NULL
    write.csv(clickData, file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",paste0(participantList[counter],".csv")))
  }
}