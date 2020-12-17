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
buildAll = function(){
  for(i in 1:length(novices)){
    print(paste0("Working on ", novices[i]))
    buildData(novices[i])
  }
  
  for(i in 1:length(experts)){
    print(paste0("Working on ", experts[i]))
    buildData(experts[i])
  }
}

# clickAll = function(){
#   # print("banana")
#   for(i in 1:length(novices)){
#     # print("orange")
#     clicks <- clickCalculator(novices[i])
#     write.csv(clicks, file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv",paste0(novices[i],".csv")))
#   }
#   
#   for(i in 1:length(experts)){
#     clicks <- clickCalculator(experts[i])
#     write.csv(clicks, file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv",paste0(novices[1],".csv")))
#   }
#   
#   
# }

#Builds the input data for a single user 
buildData = function(participant){
  #Read in the data from the csv
    #NOTE: must fix so that we can do all files depending on the name
  # initData <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd","csv","_Testing_p6_file.csv"), header = TRUE, sep=",", stringsAsFactors = FALSE)
  initData <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/GazePoint Data/wd/csv/traj/Originals",paste0(participant,"_all_gaze.csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
  
  dataNeeded <- data.frame(slide=character(),initialAOI=integer(),fixationTime=double(),correctedAOI=double())
 
  
  fixationNum <-initData[1,"FPOGID"]
  secondCount <- 1
  newData <- list()
  currentSlide <- initData[1,"Slide_Image"]
  
  for(counter in 1:nrow(initData)){
    if(initData[counter,"FPOGID"] !=fixationNum){
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
  # dataNeeded <- as.list(dataNeeded)
  # dataNeeded <- do.call(c, list(dataNeeded, clicks))
  
  # dataNeeded <- cbind(dataNeeded, clicks)
  
  write.csv(dataNeeded, file = file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_data_csv",paste0(participant,".csv")))
  
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
clickCalculator = function(){
  clickData <- list()
  for(counter in 1:14){
    clickData[[counter]] <- read_xlsx(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/results for all - Ordered.xlsx"), sheet = counter)
  }
  # a <- data.frame("Slide" = slideList)
  
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
  
  # for(sheetCount in 1:14){
  #   for(counter in 1:length(clickData[[sheetCount]][[1]])){
  #     particip <- clickData[[sheetCount]][[1]][counter]
  #     clicks <- c()
  #     for(counter2 in 2:10){
  #       if(clickData[[sheetCount]][[counter2]][[counter]] == "1.0"){
  #         clicks <- c(clicks, counter2)
  #       }
  #     }
  #     if(length(clicks) == 3){
  #       a <- cbind(a,clicks)
  #     }
  #   }
  # }
  # return(a)
  
  # initData <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Originals",paste0(participant,"_all_gaze.csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
  # rawData <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/GazePoint Data/wd/csv/traj",paste0(participant,"_all_gaze.csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
  # 
  # currentSlide <- initData[1,"Slide_Image"]
  # slideEnds <- list()
  # for(counter in 1:nrow(initData)){
  #   if(currentSlide != initData[counter,"Slide_Image"]){
  #     slideEnds[[currentSlide]] <- initData[counter,"CNT"]
  #     currentSlide <- initData[counter, "Slide_Image"]
  #   }
  # }
  # 
  # slideEnds[[currentSlide]] <- rawData[nrow(rawData),"CNT"]
  # 
  # 
  # 
  # 
  # slideList <- c("fruit.bmp","intro.bmp","0.bmp","1.bmp","2.bmp","3.bmp","4.bmp","5.bmp",
  #                "6.bmp","7.bmp","8.bmp","9.bmp","10.bmp","11.bmp")
  # clickList <- list()
  # clicks <- c()
  # for(counter in 1:nrow(rawData)){
  #   if(rawData[counter,"CS"] == 1){
  #     a <- rawData[counter,"CX"]
  #     b <- rawData[counter,"CY"]
  #     c <- slideEnds
  #     d <- aoiCalculator(a,b,c)
  #     # e <- aoiCorrector(d,participant,"slides")
  #     clicks <- c(clicks,d)
  #   }
  # }
  # return(clicks)
  
  # for(counter in 1:nrow(rawData)){
  #   if(rawData[counter,"CS"] ==1){
  #     a <- rawData[counter,"CX"]
  #     b <- rawData[counter,"CY"]
  #     c <- "slides"
  #     d <- aoiCalculator(a,b,c)
  #     
  #     if(d != 0){
  #       if(d %in% clicks){
  #         clicks <- clicks[!clicks == d]
  #       }else{
  #         clicks <- c(clicks,d)
  #       }
  #     }
  #     
  #     if(length(clicks) == 3){
  #       clickList[[slideList[1]]] <- clicks
  #       names(slideList)[1] <- NULL
  #       clicks <- c()
  #     }
  #   }
  # }
  # return(clickList)
  
  # currentSlide <- initData[1,"Slide_Image"]
  # slideEnds <- list()
  # for(counter in 1:nrow(initData)){
  #   if(currentSlide != initData[counter,"Slide_Image"]){
  #     slideEnds[[currentSlide]] <- initData[counter,"CNT"]
  #     currentSlide <- initData[counter, "Slide_Image"]
  #   }
  # }
  # 
  # slideEnds[[currentSlide]] <- rawData[nrow(rawData),"CNT"]
  # # slideEnds[[currentSlide]] <- initData[nrow(initData),"CNT"]
  # 
  # clicks <- c()
  # 
  # for(counter in 1:nrow(rawData)){
  #   if(rawData[counter,"CS"] ==1){
  #     clicks <- c(clicks, rawData[counter,"CNT"])
  #   }
  # }
  # 
  # clickList <- list()    
  # currentClicks <- c()
  # 
  # 
  # for(counter in 1:length(clicks)){
  #   if(clicks[counter] < slideEnds[[1]]){
  #     # rawData[counter,"CNT"]
  #     a <- rawData[clicks[counter],"FPOGX"]
  #     b <- rawData[clicks[counter],"FPOGY"]
  #     c <- names(slideEnds)[1]
  #     d <- aoiCalculator(a,b,c)
  #     
  #     if(d != 0){
  #       if(d %in% currentClicks){
  #         currentClicks <- currentClicks[!currentClicks == d]
  #       }else{
  #         currentClicks <- c(currentClicks,d)
  #         # currentClicks <- c(currentClicks,aoiCalculator(rawData[clicks[counter],"CX"],
  #         #                                               rawData[clicks[counter],"CY"],
  #         #                                               names(slideEnds)[1]))
  #       }
  #     }
  #   }else{
  #     clickList[[names(slideEnds)[1]]] <- currentClicks
  #     currentClicks <- c(aoiCalculator(rawData[clicks[counter],"CX"],
  #                                      rawData[clicks[counter],"CY"],
  #                                      names(slideEnds)[1]))
  #     # slideEnds[1] <- NULL
  #     slideEnds[names(slideEnds)[1]] <- NULL
  #   }
  # }
  # 
  # return(clickList)
  
  
  # initSlide <- initData[1,"Slide_Image"]
  # clickList <- list()
  # aoi <- c()
  # slideEndIndex <- list()
  # 
  # for(counter1 in 2:nrow(initData)){
  #   if(initSlide != initData[counter1-1,"Slide_Image"]){
  #     slideEndIndex[[initSlide]] <- initData[counter1-1,"CNT"]
  #     initSlide <- initData[counter1-1,"Slide_Image"]
  #   }else{
  #     # print(counter1)
  #   }
  # }
  # slideEndIndex[["11.bmp"]] <- initData[nrow(initData), "CNT"]
  # 
  # 
  # for(counter in 1:nrow(rawData)){
  #   if(rawData[counter,"CS"] == 1){
  #     cnt <- rawData[counter,"CNT"]
  #     
  #     abc <- which(initData$CNT == slideEndIndex[[1]])
  #     
  #     initAOI <- aoiCalculator(rawData[counter,"CX"],
  #                              rawData[counter,"CY"],
  #                              names(slideEndIndex)[1])
  #     correctAOI <- aoiCorrector(initAOI,
  #                                participant,
  #                                names(slideEndIndex)[1])
  #     if(cnt > slideEndIndex[[1]]){
  #       clickList[[names(slideEndIndex)[1]]] <- aoi
  #       aoi <- c()
  #       slideEndIndex[names(slideEndIndex)[1]] <- NULL
  #       aoi <- c(aoi,correctAOI)
  #     }else{
  #       aoi <- c(aoi,correctAOI)
  #     }
      
      # if(cnt > slideEndIndex[[1]]){
      #   clickList[[names(slideEndIndex)[1]]] <- aoi
      #   aoi <- c()
      #   slideEndIndex[names(slideEndIndex)[1]] <- NULL
      #   aoi <- c(aoi,initAOI)
      # }else{
      #   aoi <- c(aoi,initAOI)
      # }
      
      
      #get the index for slide image for initData[c,cnt] = cnt
      # a <- which(initData$CNT == cnt)
      # while(length(a) == 0){
      #   a <- a - 1
      #   a<-which(initData$CNT == cnt)
      # }
      
      # if(initSlide != initData[a,"Slide_Image"]){
      #   clickList[[initData[a,"Slide_Image"]]] <- aoi
      #   aoi <- c()
      # }
    # }
    # clickList[[names(slideEndIndex)[1]]] <- aoi
  
  # for(things in 1:length(clickList)){
  #   clickList[[things]] <- clickList[[things]][clickList[[things]] != clickList[[things]][duplicated(clickList[[things]])]]
  #   
  # }
  
  # for (things in 1:length(clickList)){
  #   ls <- clickList[[things]]
  #   # ls <- ls[ls != ls[duplicated(ls)]]
  #   ls <- ls[!(duplicated(ls) | duplicated(ls, fromLast = TRUE))]
  #   clickList[[things]] <- ls
  # }

  # return(clickList)
  
  
  
  
  # initData <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Originals",paste0(participant,"_all_gaze.csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
  # clickList <- list()
  # aoi <- c()
  # slide <- initData[1,"Slide_Image"]
  # slideEnd <- list()
  # rawData <- read.csv(file.path("C:/Users/mchri/Desktop/R Stuff/GazePoint Data/wd/csv/traj",paste0(participant,"_all_gaze.csv")), header = TRUE, sep=",", stringsAsFactors = FALSE)
  # 
  # for(counter in 1:nrow(initData)){
  #   if(initData[counter,"Slide_Image"] != slide){
  #     slideEnd[[initData[counter-1,"Slide_Image"]]] <- initData[counter-1, "CNT"]
  #     slide <- initData[counter-1, "Slide_Image"]
  #   }
  # }
  # slideEnd[[initData[nrow(initData),"Slide_Image"]]] <- initData[nrow(initData),"CNT"]
  # 
  # for(counter in 1:nrow(rawData)){
  #   if(rawData[counter,"CS"] == 1){
  #     aoi <- c(aoi, aoiCalculator(rawData[counter,"CX"],rawData[counter,"CY"],"slides"))
  #     
  #     aaaa <- rawData[counter,"CNT"]
  #     
  #     if(rawData[counter, "CNT"] > slideEnd[1]){
  #       slideNames <- names(slideEnd)
  #       slideEnd[[1]] <- NULL
  #       clickList[[slideNames[[1]]]] <- aoi
  #       slideNames <- NULL
  #       aoi = c()
  #     }
  #   }
  #   if(counter == nrow(rawData)-1){
  #     slideNames <- names(slideEnd)
  #     slideEnd[[1]] <- NULL
  #     clickList[[slideNames[[1]]]] <- aoi
  #   }
  # }
  # 
  # for(counter1 in 1:length(clickList)){
  #   for(counter2 in 1:3){
  #     clickList[[counter1]][counter2] <- aoiCorrector(clickList[[counter1]][counter2],
  #                                                initData[1,"Subject_ID"],
  #                                                names(clickList)[counter1])
  #   }
  # }
  # 
  # return(clickList)

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



