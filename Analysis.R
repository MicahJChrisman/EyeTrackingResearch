# Primary file for analysis. 

setwd(dir = "C:/Users/mchri/Desktop/R Stuff/S20_research/wd")
source("C:/Users/mchri/Desktop/R Stuff/S20_research/Surface_Or_Meaningful_Lists.R")
source("C:/Users/mchri/Desktop/R Stuff/S20_research/ChiTests.R")
source("C:/Users/mchri/Desktop/R Stuff/S20_research/AnovaTests.R")

library(lsr)
library(sciplot)
library(car)
library(agricolae)
library(DunnettTests)
library(multcomp)
library(PMCMRplus)
library(lsmeans)
library(reshape2)
library(ggplot2)

# These are the people who were clearly a novice or expert, no middle ground
novices <- c("s1","s2","s5","s6","s7","s8","s12","s14","s15","s17","s18","s24","s26")
experts <- c("p1","p2","p3","p4","p5","p6","p8")

AOIs <- LETTERS[1:9]

allAnalysis = function(zeros = TRUE){
  expertList <- list()
  noviceList <- list()
  for(counter in 1:length(experts)){
    expertList[[experts[counter]]] <- analyzeInitial(experts[counter], zeros)
  }
  for(counter in 1:length(novices)){
    noviceList[[novices[counter]]] <- analyzeInitial(novices[counter], zeros)
  }
  slideAnalysis(expertList, noviceOrExpert = "expert")
  slideAnalysis(noviceList, noviceOrExpert = "novice")
  
  # anovaSecondA(expertList, noviceList)
  
  ttestMeaningful(expertList, noviceList)
  anovaSecondB(expertList, noviceList)
  
  chiPlotsTotal(expertList, noviceList)
  chiPlots(expertList, noviceList)
  chiTest(expertList, noviceList)
  chiTestToStudent(expertList)
  
  transitionAmount(expertList, noviceList)
}

# Runs a t-test for each slide comparing the total amount of transitions made by experts and novices
transitionAmount = function(expertList, noviceList){
  transitionData <- list("fruit.bmp" = data.frame(NovEx = character(), Transitions = double()),
                         "intro.bmp" = data.frame(NovEx = character(), Transitions = double()),
                         "0.bmp" = data.frame(NovEx = character(), Transitions = double()),
                         "1.bmp" = data.frame(NovEx = character(), Transitions = double()), 
                         "2.bmp" = data.frame(NovEx = character(), Transitions = double()),
                         "3.bmp" = data.frame(NovEx = character(), Transitions = double()), 
                         "4.bmp" = data.frame(NovEx = character(), Transitions = double()), 
                         "5.bmp" = data.frame(NovEx = character(), Transitions = double()), 
                         "6.bmp" = data.frame(NovEx = character(), Transitions = double()),
                         "7.bmp" = data.frame(NovEx = character(), Transitions = double()),
                         "8.bmp" = data.frame(NovEx = character(), Transitions = double()),
                         "9.bmp" = data.frame(NovEx = character(), Transitions = double()), 
                         "10.bmp" = data.frame(NovEx = character(), Transitions = double()), 
                         "11.bmp" = data.frame(NovEx = character(), Transitions = double()))
  
  for(expertCount in 1:length(experts)){
    fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros/",expertList[expertCount],".csv")),
                             header = TRUE, sep=",", stringsAsFactors = FALSE)
    prevSlide <- fixationData$slide[[1]]
    transitionData[[prevSlide]] <- rbind(transitionData[[prevSlide]], list(NovEx = "Expert",Transitions = 0))
    for(fixationCount in 1:nrow(fixationData)){
      if(fixationData$slide[[fixationCount]] == prevSlide){
        transitionData[[prevSlide]][["Transitions"]][[expertCount]] <- transitionData[[prevSlide]][["Transitions"]][[expertCount]] + 1
      }else{
        prevSlide <- fixationData$slide[[fixationCount]]
        transitionData[[prevSlide]] <- rbind(transitionData[[prevSlide]], list(NovEx = "Expert",Transitions = 0))
      }
    }
  }
  
  for(noviceCount in 1:length(novices)){
    fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros/",noviceList[noviceCount],".csv")),
                             header = TRUE, sep=",", stringsAsFactors = FALSE)
    prevSlide <- fixationData$slide[[1]]
    transitionData[[prevSlide]] <- rbind(transitionData[[prevSlide]], list(NovEx = "Novice",Transitions = 0))
    for(fixationCount in 1:nrow(fixationData)){
      if(fixationData$slide[[fixationCount]] == prevSlide){
        transitionData[[prevSlide]][["Transitions"]][[length(transitionData[[prevSlide]][["Transitions"]])]] <- transitionData[[prevSlide]][["Transitions"]][[length(transitionData[[prevSlide]][["Transitions"]])]] + 1
      }else{
        prevSlide <- fixationData$slide[[fixationCount]]
        transitionData[[prevSlide]] <- rbind(transitionData[[prevSlide]], list(NovEx = "Novice",Transitions = 0))
      }
    }
  }
  
  for(slideCount in 1:length(transitionData)){
    print(names(pages)[[slideCount]])
    
    # abc <- t.test(formula = Transitions ~ NovEx, data = transitionData[[slideCount]])
    # print(abc[["p.value"]])
    # print(abc[["estimate"]][["mean in group Expert"]])
    # print(abc[["estimate"]][["mean in group Novice"]])
    
    print(t.test(formula = Transitions ~ NovEx, data = transitionData[[slideCount]]))
  }
}

# Runs a t-test between the percentage of meaningful transitions that were made be experts and novices for each slide
# Actually used in the paper
ttestMeaningful = function(expertList, noviceList){
  for(slideCount in 1:nrow(expertList[[1]])){
    dataFrame <- data.frame(NovEx = character(),SurMean = character(), Percentage = double())
    
    for(expertCount in 1:length(expertList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",experts[[expertCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Meaningful"){
        # dataFrame <- rbind(dataFrame, list(NovEx = "Expert",SurMean = "Surface",Percentage = expertList[[expertCount]]$SurfacePercentage[[slideCount]]))
        dataFrame <- rbind(dataFrame, list(NovEx = "Expert",SurMean = "Meaningful",Percentage = expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]))
      }
    } 

    for(noviceCount in 1:length(noviceList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",novices[[noviceCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Surface"){
        if(noviceCount == 1 || noviceCount == 6){
          if(slideCount > 1){
            # dataFrame <- rbind(dataFrame, list(NovEx = "Novice",SurMean = "Surface",Percentage = noviceList[[noviceCount]]$SurfacePercentage[[slideCount-1]]))
            dataFrame <- rbind(dataFrame, list(NovEx = "Novice",SurMean = "Meaningful",Percentage = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount-1]]))
          }
        }else{
          # dataFrame <- rbind(dataFrame, list(NovEx = "Novice",SurMean = "Surface",Percentage = noviceList[[noviceCount]]$SurfacePercentage[[slideCount]]))
          dataFrame <- rbind(dataFrame, list(NovEx = "Novice",SurMean = "Meaningful",Percentage = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]]))
        }
      }
    } 
    print(names(pages)[[slideCount]])
    
    if(length(which(dataFrame$NovEx == "Novice")) > 1 && length(which(dataFrame$NovEx == "Expert")) > 1){
      print(t.test(formula = Percentage ~ NovEx, data = dataFrame))
    }else if(length(which(dataFrame$NovEx == "Novice")) > 0 && length(which(dataFrame$NovEx == "Expert")) > 0){
      matchNovice <- which("Novice" == dataFrame$NovEx)
      matchExpert <- dataFrame$Percentage[which("Expert" == dataFrame$NovEx)]
      print(t.test(matchExpert, mu = dataFrame$Percentage[[matchNovice]]))
      # print(t.test(dataFrame$Percentage[[matchExpert]], mu = dataFrame$Percentage[[matchNovice]]))
    }
    
    # Only Used for ProfSur-StudSur
    # if(length(which(dataFrame$NovEx == "Novice")) > 1 && length(which(dataFrame$NovEx == "Expert")) > 1){
    #   print(t.test(formula = Percentage ~ NovEx, data = dataFrame))
    # }else if(length(which(dataFrame$NovEx == "Novice")) > 0 && length(which(dataFrame$NovEx == "Expert")) > 0){
    #   matchExpert <- which("Expert" == dataFrame$NovEx)
    #   matchNovice <- dataFrame$Percentage[which("Novice" == dataFrame$NovEx)]
    #   print(t.test(matchNovice, mu = dataFrame$Percentage[[matchExpert]]))
    # }
  }
}

# Runs a t-test on the percentage of surface and meaningful, but expert and novice are grouped so I think its useless. 
ttest = function(expertList, noviceList){
  for(slideCount in 1:nrow(expertList[[1]])){
    dataFrame <- data.frame(NovEx = character(),SurMean = character(), Percentage = double())
    for(expertCount in 1:length(expertList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",experts[[expertCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Meaningful"){
        dataFrame <- rbind(dataFrame, list(NovEx = "Expert",SurMean = "Surface",Percentage = expertList[[expertCount]]$SurfacePercentage[[slideCount]]))
        dataFrame <- rbind(dataFrame, list(NovEx = "Expert",SurMean = "Meaningful",Percentage = expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]))
      }
    } 
    # if(nrow(dataFrame) > 0){
    #   print(names(pages)[[slideCount]])
    #   print(t.test(formula = Percentage ~ SurMean, data = dataFrame))
    # }
  }
  
  for(slideCount in 1:nrow(expertList[[1]])){
    dataFrame <- data.frame(NovEx = character(),SurMean = character(), Percentage = double())
    for(noviceCount in 1:length(noviceList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",novices[[noviceCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Surface"){
        if(noviceCount == 1 || noviceCount == 6){
          if(slideCount > 1){
            dataFrame <- rbind(dataFrame, list(NovEx = "Novice",SurMean = "Surface",Percentage = noviceList[[noviceCount]]$SurfacePercentage[[slideCount-1]]))
            dataFrame <- rbind(dataFrame, list(NovEx = "Novice",SurMean = "Meaningful",Percentage = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount-1]]))
          }
        }else{
          dataFrame <- rbind(dataFrame, list(NovEx = "Novice",SurMean = "Surface",Percentage = noviceList[[noviceCount]]$SurfacePercentage[[slideCount]]))
          dataFrame <- rbind(dataFrame, list(NovEx = "Novice",SurMean = "Meaningful",Percentage = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]]))
        }
      }
    }
    if(nrow(dataFrame) > 0){
      print(names(pages)[[slideCount]])
      print(t.test(formula = Percentage ~ SurMean, data = dataFrame))
    }
  }
}

# I think useless
fixationComparisonNew = function(durationDataExpert, durationDataNovice){
  plotData <- NULL
  for(slideCount in 1:length(pages)){
    
    plotData <- rbind(plotData, t(subset(durationDataExpert[[slideCount]], select =c("Duration"))) -
                      t(subset(durationDataNovice[[slideCount]], select = c("Duration"))))
    colnames(plotData) <- durationDataExpert[[slideCount]][,1]
  }
}

# Creates the bar plots comparing the percentage of time that novices and experts spend looking at each possible meaningful/surface section
# Requires plugging in the data from fixationComparisionBuilder
fixationPlotMaker = function(durationDataExpert, durationDataNovice){
  for(slideCount in 1:length(pages)){
    plotData <- rbind(t(subset(durationDataExpert[[slideCount]], select =c("Duration"))),
          t(subset(durationDataNovice[[slideCount]], select = c("Duration"))))
    colnames(plotData) <- durationDataExpert[[slideCount]][,1]
    
    colorList <- c()
    colorData <- t(subset(durationDataExpert[[slideCount]], select = c("Validation")))
    for(color in 1:length(colorData)){
      if(colorData[[color]] == "Surface"){
        colorList <- c(colorList, "orange", "blue")
      }else{
        colorList <- c(colorList, "red", "green")
      }
    }
    if(sum(is.nan(plotData)) == 0){
      pdf(file = paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/pdfs/",names(pages)[[slideCount]],".pdf"),
          width = 9,
          height = 6)
      plots <- barplot(plotData,
              beside = TRUE,
              ylim = range(0,max(plotData)+.05),
              main = "Duration of Gaze",
              xlab = "Possible Focus Area",
              ylab = "Fixation Percentage",
              col = colorList)
      legend("top",
             horiz = TRUE,
             bty = "n",
             c("Expert Surface","Novice Surface","Expert Meaningful", "Novice Meaningful"),
             fill = c("orange","blue","red","green"))
      text(x = plots, y = plotData + 0.008, labels = as.character(round(plotData, digits = 2)))
      dev.off()
    }
  }
}

# Gives the percentage of time that novices and experts focus on a surface or meaningful grouping, later used for fixation comparision plots
fixationComparisonBuilder = function(inputList,zeros){
  durationData <- list("fruit.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "intro.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "0.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "1.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "2.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "3.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "4.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "5.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "6.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "7.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "8.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "9.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "10.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "11.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()))
  for(slideCount in 1:length(pages)){
    if(length(pages[[slideCount]][["Surface"]]) >0){
      for(count in 1:length(pages[[slideCount]][["Surface"]])){
        durationData[[slideCount]][count,] <- list(pages[[slideCount]][["Surface"]][[count]], "Surface", 0, 0)
      }
    }
    if(length(pages[[slideCount]][["Meaningful"]]) >0){
      for(count in 1:length(pages[[slideCount]][["Meaningful"]])){
        durationData[[slideCount]][count + length(pages[[slideCount]][["Surface"]]),] <- list(pages[[slideCount]][["Meaningful"]][[count]], "Meaningful", 0, 0)
      }
    }
  }
  
  for (participant in 1:length(inputList)){
    if(zeros == TRUE){
      fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/",inputList[participant],".csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
    }else{
      fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros/",inputList[participant],".csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
    }
    
    prevSlide <- fixationData$slide[[1]]
    for(fixationCount in 1:nrow(fixationData)){
      if(fixationData$slide[[fixationCount]] != prevSlide){
        prevSlide <- fixationData$slide[[fixationCount]]
      }
      if(length(pages[[prevSlide]][["Surface"]]) > 0){
        for(count in 1:length(pages[[prevSlide]][["Surface"]])){
          splits <- strsplit(pages[[prevSlide]][["Surface"]][[count]],"")
          for(stringCount in 1:3){
            if(match(splits[[1]][[stringCount]],AOIs) == fixationData$correctedAOI[[fixationCount]]){
              durationData[[prevSlide]][[count,4]] <- durationData[[prevSlide]][[count,4]] + fixationData$fixationTime[[fixationCount]]
              durationData[[prevSlide]][[count,3]] <- durationData[[prevSlide]][[count,3]] + 1
              break
            }
          }
        }
      }
      if(length(pages[[prevSlide]][["Meaningful"]]) > 0){
        for(count in 1:length(pages[[prevSlide]][["Meaningful"]])){
          splits <- strsplit(pages[[prevSlide]][["Meaningful"]][[count]],"")
          for(stringCount in 1:3){
            if(match(splits[[1]][[stringCount]],AOIs) == fixationData$correctedAOI[[fixationCount]]){
              durationData[[prevSlide]][[count + length(pages[[prevSlide]][["Surface"]]),4]] <- durationData[[prevSlide]][[count + length(pages[[prevSlide]][["Surface"]]),4]] + fixationData$fixationTime[[fixationCount]]
              durationData[[prevSlide]][[count + length(pages[[prevSlide]][["Surface"]]),3]] <- durationData[[prevSlide]][[count + length(pages[[prevSlide]][["Surface"]]),3]] + 1
              break
            }
          }
        }
      }
    }
  }

  for(slide in 1:length(pages)){
    durationData[[slide]][["Duration"]] <- durationData[[slide]][["Duration"]] / sum(durationData[[slide]][["Duration"]])
  }
  return(durationData)
}

# fixationComparisionBuilder but with a validation factor included to make sure people got things right
fixationComparisonBuilderValidation = function(inputList,zeros,validationFactor){
  durationData <- list("fruit.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "intro.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "0.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "1.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "2.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "3.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "4.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "5.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "6.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "7.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "8.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "9.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "10.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()),
                       "11.bmp" = data.frame("FocusArea" = character(), "Validation" = character(), "Fixations" = double(), "Duration" = double()))

  for(slideCount in 1:length(pages)){
    if(length(pages[[slideCount]][["Surface"]]) >0){
      for(count in 1:length(pages[[slideCount]][["Surface"]])){
        durationData[[slideCount]][count,] <- list(pages[[slideCount]][["Surface"]][[count]], "Surface", 0, 0)
      }
    }
    if(length(pages[[slideCount]][["Meaningful"]]) >0){
      for(count in 1:length(pages[[slideCount]][["Meaningful"]])){
        durationData[[slideCount]][count + length(pages[[slideCount]][["Surface"]]),] <- list(pages[[slideCount]][["Meaningful"]][[count]], "Meaningful", 0, 0)
      }
    }
  }
  
  for (participant in 1:length(inputList)){
    if(zeros == TRUE){
      fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/",inputList[participant],".csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
    }else{
      fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros/",inputList[participant],".csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
    }
    
    clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",inputList[[participant]],".csv")))
    
    prevSlide <- ""
    for(fixationCount in 1:nrow(fixationData)){
      if(fixationData$slide[[fixationCount]] != prevSlide){
        prevSlide <- fixationData$slide[[fixationCount]]
        slideValidation <- match(prevSlide, subset(clickData, select = c("Slide"))[[1]])
      }
      
      if(clickData$Validation[[slideValidation]] == validationFactor){
        if(length(pages[[prevSlide]][["Surface"]]) > 0){
          for(count in 1:length(pages[[prevSlide]][["Surface"]])){
            splits <- strsplit(pages[[prevSlide]][["Surface"]][[count]],"")
            for(stringCount in 1:3){
              if(match(splits[[1]][[stringCount]],AOIs) == fixationData$correctedAOI[[fixationCount]]){
                durationData[[prevSlide]][[count,4]] <- durationData[[prevSlide]][[count,4]] + fixationData$fixationTime[[fixationCount]]
                durationData[[prevSlide]][[count,3]] <- durationData[[prevSlide]][[count,3]] + 1
                break
              }
            }
          }
        }
        if(length(pages[[prevSlide]][["Meaningful"]]) > 0){
          for(count in 1:length(pages[[prevSlide]][["Meaningful"]])){
            splits <- strsplit(pages[[prevSlide]][["Meaningful"]][[count]],"")
            for(stringCount in 1:3){
              if(match(splits[[1]][[stringCount]],AOIs) == fixationData$correctedAOI[[fixationCount]]){
                durationData[[prevSlide]][[count + length(pages[[prevSlide]][["Surface"]]),4]] <- durationData[[prevSlide]][[count + length(pages[[prevSlide]][["Surface"]]),4]] + fixationData$fixationTime[[fixationCount]]
                durationData[[prevSlide]][[count + length(pages[[prevSlide]][["Surface"]]),3]] <- durationData[[prevSlide]][[count + length(pages[[prevSlide]][["Surface"]]),3]] + 1
                break
              }
            }
          }
        }
      }
    }
  }
  
  for(slide in 1:length(pages)){
    durationData[[slide]][["Duration"]] <- durationData[[slide]][["Duration"]] / sum(durationData[[slide]][["Duration"]])
  }
  return(durationData)
}

# Honestly no idea
fixationComparison= function(inputList, zeros){
  zeroList <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  for(slideCount in 1:length(pages)){
    dataFrame <- data.frame("Slides" = names(pages))
    if(length(pages[[slideCount]][["Surface"]]) > 0){
      for(count in 1:length(pages[[slideCount]][["Surface"]])){
        dataFrame <- cbind(dataFrame, zeroList)
        colnames(dataFrame)[count + 1] <- paste0(pages[[slideCount]][["Surface"]][[count]],".Surface") 
      }
    }
    if(length(pages[[slideCount]][["Meaningful"]]) > 0){
      for(count in 1:length(pages[[slideCount]][["Meaningful"]])){
        dataFrame <- cbind(dataFrame, zeroList)
        colnames(dataFrame)[count + 1 + length(pages[[slideCount]][["Surface"]])] <- paste0(pages[[slideCount]][["Meaningful"]][[count]],".Meaningful")
      }
    }
    for(participant in 1:length(inputList)){
      if(zeros == TRUE){
        fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/",inputList[participant],".csv")),
                                 header = TRUE, sep=",", stringsAsFactors = FALSE)
      }else{
        fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros/",inputList[participant],".csv")),
                                 header = TRUE, sep=",", stringsAsFactors = FALSE)
      }
      fixationCount <- 1
      while(fixationData$slide[[fixationCount]] == names(pages)[slideCount]){
        if(length(pages[[slideCount]][["Surface"]]) > 0){
          for(count in 1:length(pages[[slideCount]][["Surface"]])){
            splits <- strsplit(pages[[slideCount]][["Surface"]][[count]],"")
            for(stringCount in 1:3){
              if(match(splits[[1]][[stringCount]],AOIs) == fixationData$correctedAOI[[fixationCount]]){
                dataFrame[[count + 1]][[slideCount]] <- dataFrame[[count + 1]][[slideCount]] + fixationData$fixationTime[[fixationCount]] 
              }
            }
          }
        }
        if(length(pages[[slideCount]][["Meaningful"]]) > 0 ){
          for(count in 1:length(pages[[slideCount]][["Meaningful"]])){
            
          }
        }
        fixationCount <- fixationCount + 1
      }
    }  
    b <- 0
  }
  # write.csv()
  a <- 0
}

# Compares the amount of time that experts and novices spent viewing the parts that they selected and the parts that they did not 
# filtered by invalid/surface/meaningful
fixationAnalysis = function(inputList,zeros, noviceOrExpert){
  pages <- c("fruit.bmp","intro.bmp","0.bmp","1.bmp",
             "2.bmp","3.bmp","4.bmp","5.bmp",
             "6.bmp","7.bmp","8.bmp","9.bmp",
             "10.bmp","11.bmp")
  
  dataFrame <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/FixationTemplate.csv")),
                        header = TRUE, sep=",", stringsAsFactors = FALSE)
  
  for(participantCount in 1:length(inputList)){
    if(zeros == TRUE){
      fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/",inputList[participantCount],".csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
    }else{
      fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros/",inputList[participantCount],".csv")),
                               header = TRUE, sep=",", stringsAsFactors = FALSE)
    }
    validationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",inputList[participantCount],".csv")),
                               header = TRUE, sep = ",", stringsAsFactors = FALSE)
    
    prevSlide <- ""
    validity <- ""
    clickedAOI <- 0
    nonClickedAOI <- 0 
    clicks <- c()
    
    for(counter in 1:nrow(fixationData)){
      if(prevSlide != fixationData$slide[[counter]]){
        if(validity == "Surface"){
          dataFrame$'ClickedAOI.Surface'[[slideMatch]] <- clickedAOI + dataFrame$'ClickedAOI.Surface'[[slideMatch]]
          dataFrame$'OtherAOI.Surface'[[slideMatch]] <- nonClickedAOI + dataFrame$'OtherAOI.Surface'[[slideMatch]]
        }else if(validity == "Meaningful"){
          dataFrame$'ClickedAOI.Meaningful'[[slideMatch]] <- clickedAOI + dataFrame$'ClickedAOI.Meaningful'[[slideMatch]]
          dataFrame$'OtherAOI.Meaningful'[[slideMatch]] <- nonClickedAOI + dataFrame$'OtherAOI.Meaningful'[[slideMatch]]
        }else if(validity == "Invalid"){
          dataFrame$'ClickedAOI.Invalid'[[slideMatch]] <- clickedAOI + dataFrame$'ClickedAOI.Invalid'[[slideMatch]]
          dataFrame$'OtherAOI.Invalid'[[slideMatch]] <- nonClickedAOI + dataFrame$'OtherAOI.Invalid'[[slideMatch]]
        }
        clickedAOI <- 0 
        nonClickedAOI <- 0
        prevSlide <- fixationData$slide[[counter]]
        slideMatch <- match(prevSlide, pages)
        clicks <- c(validationData[slideMatch,3],
                    validationData[slideMatch,4],
                    validationData[slideMatch,5])
        validity <- validationData[slideMatch,7]
        a <- 0
      }
      if(fixationData$correctedAOI[[counter]] %in% clicks){
        clickedAOI <- clickedAOI + fixationData$fixationTime[[counter]]
      }else{
        nonClickedAOI <- nonClickedAOI + fixationData$fixationTime[[counter]]
      }
    }
    if(validity == "Surface"){
      dataFrame$'ClickedAOI.Surface'[[slideMatch]] <- clickedAOI + dataFrame$'ClickedAOI.Surface'[[slideMatch]]
      dataFrame$'OtherAOI.Surface'[[slideMatch]] <- nonClickedAOI + dataFrame$'OtherAOI.Surface'[[slideMatch]]
    }else if(validity == "Meaningful"){
      dataFrame$'ClickedAOI.Meaningful'[[slideMatch]] <- clickedAOI + dataFrame$'ClickedAOI.Meaningful'[[slideMatch]]
      dataFrame$'OtherAOI.Meaningful'[[slideMatch]] <- nonClickedAOI + dataFrame$'OtherAOI.Meaningful'[[slideMatch]]
    }else if(validity == "Invalid"){
      dataFrame$'ClickedAOI.Invalid'[[slideMatch]] <- clickedAOI + dataFrame$'ClickedAOI.Invalid'[[slideMatch]]
      dataFrame$'OtherAOI.Invalid'[[slideMatch]] <- nonClickedAOI + dataFrame$'OtherAOI.Invalid'[[slideMatch]]
    }
    b <- 0
  }
  c <- 0
  for(slideCount in 1:nrow(dataFrame)){
    surfaceTotal <- dataFrame$'ClickedAOI.Surface'[[slideCount]] + dataFrame$'OtherAOI.Surface'[[slideCount]]
    dataFrame$'ClickedAOI.Surface'[[slideCount]] <- dataFrame$'ClickedAOI.Surface'[[slideCount]] / surfaceTotal
    dataFrame$'OtherAOI.Surface'[[slideCount]] <- dataFrame$'OtherAOI.Surface'[[slideCount]] / surfaceTotal
    
    meaningfulTotal <- dataFrame$'ClickedAOI.Meaningful'[[slideCount]] + dataFrame$'OtherAOI.Meaningful'[[slideCount]]
    dataFrame$'ClickedAOI.Meaningful'[[slideCount]] <- dataFrame$'ClickedAOI.Meaningful'[[slideCount]] / meaningfulTotal
    dataFrame$'OtherAOI.Meaningful'[[slideCount]] <- dataFrame$'OtherAOI.Meaningful'[[slideCount]] / meaningfulTotal
    
    invalidTotal <- dataFrame$'ClickedAOI.Invalid'[[slideCount]] + dataFrame$'OtherAOI.Invalid'[[slideCount]]
    dataFrame$'ClickedAOI.Invalid'[[slideCount]] <- dataFrame$'ClickedAOI.Invalid'[[slideCount]] / invalidTotal
    dataFrame$'OtherAOI.Invalid'[[slideCount]] <- dataFrame$'OtherAOI.Invalid'[[slideCount]] / invalidTotal
  }
  
  if(noviceOrExpert == "expert"){
    write.csv(dataFrame,file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/FixationsExpert.csv"))
  }else{
    write.csv(dataFrame,file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/FixationsNovice.csv"))
  }
}

# Creates the percentages and total amount of transitions that is needed for all real analysis
# Actually used
analyzeInitial = function(participant, zeros){

  if(zeros == TRUE){
    fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/",participant,".csv")),
                             header = TRUE, sep=",", stringsAsFactors = FALSE)
  }else{
    fixationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros/",participant,".csv")),
                             header = TRUE, sep=",", stringsAsFactors = FALSE)
  }
  prevSlide <- fixationData[["slide"]][[1]]
  percentageData <- data.frame(Slide=character(0), Transitions=integer(0), Surface=integer(0), Meaningful=integer(0),
                               SurfacePercentage=double(0), MeaningfulPercentage=double(0),stringsAsFactors = FALSE)
  surface <- 0
  meaningful <- 0
  transitionCount <- 0
  timer <- fixationData$fixationTime[[1]]
  
  for(fixationNumber in 2:nrow(fixationData)){
    if(fixationData$slide[[fixationNumber]] == prevSlide){
      timer <- timer + fixationData$fixation[[fixationNumber]]
      if(timer > 2){
        if(fixationData$Transition[[fixationNumber]] == "Surface"){
          surface <- surface + 1
        }else if(fixationData$Transition[[fixationNumber]] == "Meaningful"){
          meaningful <- meaningful + 1
        }
        transitionCount <- transitionCount + 1
      }
    }else{
      meaningfulPercentage <- meaningful / transitionCount
      surfacePercentage <- surface / transitionCount
      percentageData[nrow(percentageData)+1,] <- list(prevSlide,transitionCount,surface,meaningful,
                                                   surfacePercentage,meaningfulPercentage)
      prevSlide <- fixationData$slide[[fixationNumber]]
      surface <- 0
      meaningful <- 0
      transitionCount <- 0
      timer <- 0
    }
  }
  meaningfulPercentage <- meaningful / transitionCount
  surfacePercentage <- surface / transitionCount
  percentageData[nrow(percentageData)+1,] <- list(prevSlide,transitionCount,surface,meaningful,
                                               surfacePercentage,meaningfulPercentage)
  return(percentageData)
}

# Gives the basic percentages of surface/invalid/meaningful percentages of transitions for each slide
slideAnalysis = function(inputList, noviceOrExpert){
  pages <- c("fruit.bmp","intro.bmp","0.bmp","1.bmp",
                "2.bmp","3.bmp","4.bmp","5.bmp",
                "6.bmp","7.bmp","8.bmp","9.bmp",
                "10.bmp","11.bmp")
  if(noviceOrExpert == "expert"){
    dataFrame <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/SlideStatsExpert.csv")),
                             header = TRUE, sep=",", stringsAsFactors = FALSE)
  }else{
    dataFrame <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/SlideStatsNovice.csv")),
                          header = TRUE, sep=",", stringsAsFactors = FALSE)
  }
  
  surfaceCount <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  meaningfulCount <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  invalidCount <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  for(counter in 1:length(inputList)){
    transitionTotal <- 0
    surfaceTotal <- 0
    meaninfulTotal <- 0
    validationData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",names(inputList)[counter],".csv")),
                               header = TRUE, sep = ",", stringsAsFactors = FALSE)

    
    for(slideCounter in 1:nrow(inputList[[counter]])){
      slideMatch <- match(inputList[[counter]][["Slide"]][[slideCounter]], pages)
      transitionTotal <- inputList[[counter]][["Transitions"]][[slideCounter]]
      surfaceTotal <-inputList[[counter]][["Surface"]][[slideCounter]]
      meaningfulTotal <- inputList[[counter]][["Meaningful"]][[slideCounter]]
      
      if(validationData$Validation[[slideMatch]] == "Surface"){
        surfaceCount[slideMatch] <- surfaceCount[slideMatch] + 1
        dataFrame$'Average.Transitions.Surface'[[slideMatch]] <-  dataFrame$'Average.Transitions.Surface'[[slideMatch]]  + transitionTotal
        dataFrame$'Surface.Percentage.Surface'[[slideMatch]] <-  dataFrame$'Surface.Percentage.Surface'[[slideMatch]] + surfaceTotal
        dataFrame$'Meaningful.Percentage.Surface'[[slideMatch]] <-  dataFrame$'Meaningful.Percentage.Surface'[[slideMatch]] + meaningfulTotal
      }else if(validationData$Validation[[slideMatch]] == "Meaningful"){
        meaningfulCount[slideMatch] <- meaningfulCount[slideMatch] + 1
        dataFrame$'Average.Transitions.Meaningful'[[slideMatch]] <-  dataFrame$'Average.Transitions.Meaningful'[[slideMatch]]  + transitionTotal
        dataFrame$'Surface.Percentage.Meaningful'[[slideMatch]] <-  dataFrame$'Surface.Percentage.Meaningful'[[slideMatch]] + surfaceTotal
        dataFrame$'Meaningful.Percentage.Meaningful'[[slideMatch]] <-  dataFrame$'Meaningful.Percentage.Meaningful'[[slideMatch]] + meaningfulTotal
      }else if(validationData$Validation[[slideMatch]] == "Invalid"){
        invalidCount[slideMatch] <- invalidCount[slideMatch] + 1
        dataFrame$'Average.Transitions.Invalid'[[slideMatch]] <-  dataFrame$'Average.Transitions.Invalid'[[slideMatch]]  + transitionTotal
        dataFrame$'Surface.Percentage.Invalid'[[slideMatch]] <-  dataFrame$'Surface.Percentage.Invalid'[[slideMatch]] + surfaceTotal
        dataFrame$'Meaningful.Percentage.Invalid'[[slideMatch]] <-  dataFrame$'Meaningful.Percentage.Invalid'[[slideMatch]] + meaningfulTotal
      }
    }
  }
  for(slideCounter in 1:length(pages)){
    dataFrame$'Surface.Percentage.Surface'[[slideCounter]] <- dataFrame$'Surface.Percentage.Surface'[[slideCounter]] / dataFrame$'Average.Transitions.Surface'[[slideCounter]]
    dataFrame$'Meaningful.Percentage.Surface'[[slideCounter]] <- dataFrame$'Meaningful.Percentage.Surface'[[slideCounter]] / dataFrame$'Average.Transitions.Surface'[[slideCounter]]
    dataFrame$'Average.Transitions.Surface'[[slideCounter]] <- dataFrame$'Average.Transitions.Surface'[[slideCounter]] / surfaceCount[slideCounter]

    dataFrame$'Surface.Percentage.Meaningful'[[slideCounter]] <- dataFrame$'Surface.Percentage.Meaningful'[[slideCounter]] / dataFrame$'Average.Transitions.Meaningful'[[slideCounter]]
    dataFrame$'Meaningful.Percentage.Meaningful'[[slideCounter]] <- dataFrame$'Meaningful.Percentage.Meaningful'[[slideCounter]] / dataFrame$'Average.Transitions.Meaningful'[[slideCounter]]
    dataFrame$'Average.Transitions.Meaningful'[[slideCounter]] <- dataFrame$'Average.Transitions.Meaningful'[[slideCounter]] / meaningfulCount[slideCounter]

    dataFrame$'Surface.Percentage.Invalid'[[slideCounter]] <- dataFrame$'Surface.Percentage.Invalid'[[slideCounter]] / dataFrame$'Average.Transitions.Invalid'[[slideCounter]]
    dataFrame$'Meaningful.Percentage.Invalid'[[slideCounter]] <- dataFrame$'Meaningful.Percentage.Invalid'[[slideCounter]] / dataFrame$'Average.Transitions.Invalid'[[slideCounter]]
    dataFrame$'Average.Transitions.Invalid'[[slideCounter]] <- dataFrame$'Average.Transitions.Invalid'[[slideCounter]] / invalidCount[slideCounter]
  }
  
  if(noviceOrExpert == "expert"){
    write.csv(dataFrame,file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/SlideStatsExpert.csv"))
  }else{
    write.csv(dataFrame,file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/SlideStatsNovice.csv"))
  }
}









