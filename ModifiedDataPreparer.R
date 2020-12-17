#Program in order to generate the input/verification data for all of the users
#In theory should only need to be run once ever
library("readxl")
setwd(dir = "C:/Users/mchri/Desktop/R Stuff/S20_research/wd")
source("C:/Users/mchri/Desktop/R Stuff/S20_research/NovExpDecodedPics.R")

#Vectors of the students and experts that fall completely under the novice
#or expert category, missing people fall inbetween and analysis will be
#done on them at a different point

# These are the people who were clearly a novice or expert, no middle ground
novices <- c("s1","s2","s5","s6","s7","s8","s12","s14","s15","s17","s18","s24","s26")
experts <- c("p1","p2","p3","p4","p5","p6","p8")

# # All people
# novices <- c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10","s11","s12",
#              "s13","s14","s15","s16","s17","s18","s19","s20","s21","s22",
#              "s23","s24","s25","s26","s27","s28","s29","s30","s31","s32","s33")
# experts <- c("p1","p2","p3","p4","p5","p6","p7","p10","p13","p14",
#              "p15","p19","p20","p21","p22","p23","p24")


#Iterates throught the novice/experts to generate input data for all users
buildAll = function(zeros = TRUE){
  for(i in 1:length(novices)){
    print(paste0("Working on ", novices[i]))
    # buildData(novices[i])
    if(zeros){
      write.csv(buildData(novices[i],zeros), file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified",paste0(novices[i],".csv")))
    }else{
      write.csv(buildData(novices[i],zeros), file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros",paste0(novices[i],".csv")))
    }
  }
  
  for(i in 1:length(experts)){
    print(paste0("Working on ", experts[i]))
    # buildData(experts[i])
    if(zeros){
      write.csv(buildData(experts[i],zeros), file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified",paste0(experts[i],".csv")))
    }else{
      write.csv(buildData(experts[i],zeros), file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv/Modified/NoZeros",paste0(experts[i],".csv")))
    }
  }
}

#Builds the input data for a single user 
buildData = function(participant,zeros){
  #Read in the data from the csv
  initData <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/GazePoint Data/wd/csv/traj/Originals",paste0(participant,"_all_gaze.csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
  
  dataNeeded <- data.frame(slide=character(),initialAOI=integer(),fixationTime=double(),correctedAOI=double())
  
  fixationNum <-initData[1,"FPOGID"]
  secondCount <- 1
  newData <- list()
  currentSlide <- initData[1,"Slide_Image"]
  
  for(counter in 1:nrow(initData)){
    if(initData[counter,"FPOGID"] != fixationNum){
      fixationNum <- initData[counter, "FPOGID"]
      
      if(currentSlide != initData[counter, "Slide_Image"]){
        currentSlide <- initData[counter,"Slide_Image"]
      }
      
      newData[["slide"]] <- currentSlide
      newData[["initialAOI"]] <- aoiCalculator(initData[counter,"Xpos"],initData[counter,"Ypos"],"slides")
      newData[["fixationTime"]] <- initData[counter-1,"FPOGD"]
      newData[["correctedAOI"]] <-  aoiCorrector(newData[["initialAOI"]], participant, initData[counter,"Slide_Image"])
      
      dataNeeded[secondCount, ] <- newData
      secondCount <- secondCount + 1
    }
  }
  if(zeros == TRUE){
    dataNeeded <- cleanData(dataNeeded)
  }else{
    dataNeeded <- cleanDataNoZeros(dataNeeded)
  }
  return(dataNeeded)
}

# Removes the when there is a separate fixation within the same box 
# and when the user is looking at the continue button
cleanData = function(inputData){
  firstAOI <- inputData[["initialAOI"]][[1]] 
  counter <- 0
  dataNeeded <- data.frame(slide=character(),initialAOI=integer(),fixationTime=double(),correctedAOI=double())
  dataNeeded <- rbind(dataNeeded,inputData[1,])
  for(row in 2:nrow(inputData)){
    if(!is.na(inputData[["initialAOI"]][[row]])){
      if(firstAOI == inputData[["initialAOI"]][[row]]){
        counter <- counter + 1
        time <- inputData[["fixationTime"]][[row]] + dataNeeded[["fixationTime"]][[row - counter]]
        # inputData[["fixationTime"]][[row - 1]] <- time
        # inputData <- inputData[-row,]
        dataNeeded[["fixationTime"]][[row - counter]] <- time
      }else{
        firstAOI <- inputData[["initialAOI"]][row]
        dataNeeded <- rbind(dataNeeded,inputData[row,])
      }
    }
  }
  return(dataNeeded)
}

#Same as clean data, but it also removes any instance
#where the fixation is outside of the AOIs
cleanDataNoZeros = function(inputData){
  firstAOI <- 0
  counter <- 0
  dataNeeded <- data.frame(slide=character(),initialAOI=integer(),fixationTime=double(),correctedAOI=double())
  # dataNeeded <- rbind(dataNeeded,inputData[1,])
  for(row in 1:nrow(inputData)){
    if(!is.na(inputData[["initialAOI"]][[row]]) && 
       inputData[["initialAOI"]][[row]] != 0 &&
       inputData[["initialAOI"]][[row]] != 10){
      if(firstAOI == inputData[["initialAOI"]][[row]]){
        counter <- counter + 1
        time <- inputData[["fixationTime"]][[row]] + dataNeeded[["fixationTime"]][[row - counter]]
        # inputData[["fixationTime"]][[row - 1]] <- time
        # inputData <- inputData[-row,]
        dataNeeded[["fixationTime"]][[row - counter]] <- time
      }else{
        firstAOI <- inputData[["initialAOI"]][row]
        dataNeeded <- rbind(dataNeeded,inputData[row,])
      }
    }else{
      counter <- counter + 1
    }
  }
  return(dataNeeded)
}



#Determines what AOI the fixation/click is in
#returns 1-9 for image, 0 for anything outside 
aoiCalculator=function(xpos,ypos,image = "slides"){
  if(image == "fruit.bmp"){
    dims <- read.csv("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPercentage/fruit.csv")
  }else if(image == "intro.bmp"){
    dims <- read.csv("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPercentage/intro.csv")
  }else{
    dims <- read.csv("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPercentage/slides.csv")
  }
  
  for(counter in 1:nrow(dims)){
    if(xpos > dims[counter,"P0x"] & xpos < dims[counter,"P1x"]
       & ypos > dims[counter,"P0y"] & ypos < dims[counter, "P3y"]){
      return(counter)
    }
  }
  return(0)
}

# Adjusts the AOI in order to account for the randomization that occurs 
aoiCorrector = function(initAOI, participant,slide){
  if(initAOI == 0){
    return(0)
  }
  
  if(initAOI == 10){
    return(10)
  }
  
  if(slide == "fruit.bmp" | slide == "intro.bmp" | slide == "0.bmp"){
    return(initAOI)
  }else{
    slide <- as.numeric(strsplit(slide, "[.]")[[1]][1])
    combinedParticipants <- c(novices,experts)
    participant <- match(participant, combinedParticipants)
    aaa <- DecodedPages[[participant]][[slide]][initAOI]
    return(aaa)
  }
}

# Calculates the AOI of the click that occured 
clickCalculator = function(participantList){
  clickData <- list()
  for(counter in 1:14){
    clickData[[counter]] <- read_xlsx(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/results for all - Ordered.xlsx"), sheet = counter)
  }
  
  for(personCounter in 2:length(clickData[[1]][[1]])){
    a <- data.frame("Slide" = character(), "Click1" = character(), "Click2" = character(), "Click3" = character())
    slideList <- c("fruit.bmp","intro.bmp","0.bmp","1.bmp","2.bmp","3.bmp","4.bmp","5.bmp",
                   "6.bmp","7.bmp","8.bmp","9.bmp","10.bmp","11.bmp")
    for(sheetCount in 1:14){
      clicks <- c()
      for(rowCounter in 2:10){
        if(clickData[[sheetCount]][[rowCounter]][[personCounter]] == 1){
          clicks <- c(clicks, rowCounter - 1)
        }else if(clickData[[sheetCount]][[rowCounter]][[personCounter]] == "-"){
          clicks <- c("NA", "NA", "NA")
          break
        }
      }
      if(length(clicks) == 3){
        # a <- rbind(a,c(slideList[1],clicks))
        a[nrow(a)+1,] <- c(slideList[1],clicks)
        slideList <- slideList[-1]
      }
    }
    write.csv(a, file.path(paste("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",clickData[[1]][[1]][[personCounter]],".csv")))
    a <- data.frame()
  }
}

#Converts the pixels of the AOI into the portion of the screen where 0.0 is top left and 1.0 is bottom right
buildAOI = function(slideLength = 1024, slideHeight = 768){
  fruitDim <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPixel/fruit.csv"), header = TRUE, sep=",", stringsAsFactors = FALSE)
  introDim <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPixel/intro.csv"), header = TRUE, sep=",", stringsAsFactors = FALSE)
  slidesDim <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPixel/slides.csv"), header = TRUE, sep=",", stringsAsFactors = FALSE)
  
  for(counter in 1:nrow(fruitDim)){
    fruitDim[counter,"P0x"] = fruitDim[counter,"P0x"]/slideLength
    fruitDim[counter,"P0y"] = fruitDim[counter,"P0y"]/slideHeight
    fruitDim[counter,"P1x"] = fruitDim[counter,"P1x"]/slideLength
    fruitDim[counter,"P1y"] = fruitDim[counter,"P1y"]/slideHeight
    fruitDim[counter,"P2x"] = fruitDim[counter,"P2x"]/slideLength
    fruitDim[counter,"P2y"] = fruitDim[counter,"P2y"]/slideHeight
    fruitDim[counter,"P3x"] = fruitDim[counter,"P3x"]/slideLength
    fruitDim[counter,"P3y"] = fruitDim[counter,"P3y"]/slideHeight
    
    introDim[counter,"P0x"] = introDim[counter,"P0x"]/slideLength
    introDim[counter,"P0y"] = introDim[counter,"P0y"]/slideHeight
    introDim[counter,"P1x"] = introDim[counter,"P1x"]/slideLength
    introDim[counter,"P1y"] = introDim[counter,"P1y"]/slideHeight
    introDim[counter,"P2x"] = introDim[counter,"P2x"]/slideLength
    introDim[counter,"P2y"] = introDim[counter,"P2y"]/slideHeight
    introDim[counter,"P3x"] = introDim[counter,"P3x"]/slideLength
    introDim[counter,"P3y"] = introDim[counter,"P3y"]/slideHeight
    
    slidesDim[counter,"P0x"] = slidesDim[counter,"P0x"]/slideLength
    slidesDim[counter,"P0y"] = slidesDim[counter,"P0y"]/slideHeight
    slidesDim[counter,"P1x"] = slidesDim[counter,"P1x"]/slideLength
    slidesDim[counter,"P1y"] = slidesDim[counter,"P1y"]/slideHeight
    slidesDim[counter,"P2x"] = slidesDim[counter,"P2x"]/slideLength
    slidesDim[counter,"P2y"] = slidesDim[counter,"P2y"]/slideHeight
    slidesDim[counter,"P3x"] = slidesDim[counter,"P3x"]/slideLength
    slidesDim[counter,"P3y"] = slidesDim[counter,"P3y"]/slideHeight
  } 
  write.csv(fruitDim, file = "C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPercentage/fruit.csv")
  write.csv(introDim,"C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPercentage/intro.csv")
  write.csv(slidesDim,"C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Slides/SlidesPercentage/slides.csv")
}



