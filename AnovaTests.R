# Many different possible ANOVA tests possible, see commented out sections within the function to figure out what to do
# Actually used in paper
anovaSecondB = function(expertList,noviceList){
  for(slideCount in 1:nrow(expertList[[1]])){
    dataFrame <- data.frame(NovEx = character(),SurMean = character(), Percentage = double())
    for(expertCount in 1:length(expertList)){
      clickData <- read.csv(file.path(paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/cleaned_clicks_csv/",experts[[expertCount]],".csv")))
      if(clickData$Validation[[slideCount]] == "Meaningful"){
        dataFrame <- rbind(dataFrame, list(NovEx = "Expert",SurMean = "Surface",Percentage = expertList[[expertCount]]$SurfacePercentage[[slideCount]]))
        dataFrame <- rbind(dataFrame, list(NovEx = "Expert",SurMean = "Meaningful",Percentage = expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]))
      }
    } 
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
    if("Expert" %in% dataFrame$NovEx && "Novice" %in% dataFrame$NovEx){
      # Create plots to see if there is an interaction
      # pdf(file = paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/",names(pages)[[slideCount]],".pdf"),
      #     width = 9,
      #     height = 6)
      # abc <- lineplot.CI( x.factor = dataFrame$NovEx,
      #              response = dataFrame$Percentage,
      #              group = dataFrame$SurMean,
      #              ci.fun = ciMean,
      # 
      #              xlab = "NovEx",
      #              ylab = "Percentage")
      # dev.off()
      
      print(names(pages)[[slideCount]])
      
      # Two way with interaction
      # print(summary(aov(Percentage ~ SurMean*NovEx, dataFrame)))
      # print(TukeyHSD(aov(Percentage ~ SurMean*NovEx, dataFrame)))
      
      # General Linear Hypothesis
      # dataFrame$NovEx <- as.factor(dataFrame$NovEx)
      # dataFrame$SurMean <- as.factor(dataFrame$SurMean)
      # model <- lm(Percentage ~ NovEx*SurMean, data = dataFrame)
      # tmp <- expand.grid(NovEx = unique(dataFrame$NovEx),
      #                     SurMean = unique(dataFrame$SurMean))
      # X <- model.matrix(~ NovEx*SurMean, data = tmp)
      # # glht(model, linfct = X)
      # Tukey <- contrMat(table(dataFrame$NovEx), "Tukey")
      # K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
      # rownames(K1) <- paste(levels(dataFrame$SuMean)[1], rownames(K1), sep = ":")
      # K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
      # rownames(K2) <- paste(levels(dataFrame$SuMean)[2], rownames(K1), sep = ":")
      # K <- rbind(K1, K2)
      # colnames(K) <- c(colnames(Tukey), colnames(Tukey))
      # print(summary(glht(model, linfct = K %*% X)))
      
      # print((Anova(mod, type = 2)))
      
      # mod <- lm(Percentage ~ NovEx*SurMean, dataFrame)
      # marginal <- lsmeans(mod, pairwise ~ NovEx:SurMean, adjust="none")
      # print(marginal$contrasts)
      
      # mod <- lm(Percentage ~ NovEx*SurMean, dataFrame)
      # marginal <- lsmeans(mod, pairwise ~ NovEx:SurMean, adjust="Bonferroni")
      # print(marginal$contrasts)
      
      # print(summary(dunnettT3Test(Percentage ~ NovEx,data = dataFrame)))
    }
    
  }
}

# Outdated see anovaSecondB()
anovaSecondA = function(expertList,noviceList){
  anovaResults <- c()
  for(slideCount in 1:nrow(expertList[[1]])){
    dataFrame <- data.frame(NovEx = character(),SurMean = character(), Percentage = double())
    for(expertCount in 1:length(expertList)){
      dataFrame <- rbind(dataFrame, list(NovEx = "Expert",SurMean = "Surface",Percentage = expertList[[expertCount]]$SurfacePercentage[[slideCount]]))
      dataFrame <- rbind(dataFrame, list(NovEx = "Expert",SurMean = "Meaningful",Percentage = expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]))
    } 
    for(noviceCount in 1:length(noviceList)){
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
    
    print(names(pages)[[slideCount]])
    
    # One way; Note that you have to change that dataframe to NoviceMeaningful, NoviceSurface, ExpertMeaningful, ExpertSurface
    # anovaResults <- c(anovaResults, aov(Percentage ~ NovEx, dataFrame))
    
    # Two way no interaction
    # print(summary(aov(Percentage ~ SurMean+NovEx, dataFrame)))
    # print(TukeyHSD(aov(Percentage ~ SurMean*NovEx, dataFrame)))
    
    # Create plots to see if there is an interaction
    # pdf(file = paste0("C:/Users/mchri/Desktop/R Stuff/S20_research/wd/csv/Analysis/",names(pages)[[slideCount]],".pdf"),
    #     width = 9,
    #     height = 6)
    # abc <- lineplot.CI( x.factor = dataFrame$NovEx,
    #              response = dataFrame$Percentage,
    #              group = dataFrame$SurMean,
    #              ci.fun = ciMean,
    # 
    #              xlab = "NovEx",
    #              ylab = "Percentage")
    # dev.off()
    
    # Two way with interaction
    # print(summary(aov(Percentage ~ SurMean*NovEx, dataFrame)))
    # print(TukeyHSD(aov(Percentage ~ SurMean*NovEx, dataFrame)))
    
    # I don't even know
    # dataFrame$NovEx <- as.factor(dataFrame$NovEx)
    # dataFrame$SurMean <- as.factor(dataFrame$SurMean)
    # model <- lm(Percentage ~ NovEx*SurMean, data = dataFrame)
    # tmp <- expand.grid(NovEx = unique(dataFrame$NovEx),
    #                     SurMean = unique(dataFrame$SurMean))
    # X <- model.matrix(~ NovEx*SurMean, data = tmp)
    # # glht(model, linfct = X)
    # Tukey <- contrMat(table(dataFrame$NovEx), "Tukey")
    # K1 <- cbind(Tukey, matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)))
    # rownames(K1) <- paste(levels(dataFrame$SuMean)[1], rownames(K1), sep = ":")
    # K2 <- cbind(matrix(0, nrow = nrow(Tukey), ncol = ncol(Tukey)), Tukey)
    # rownames(K2) <- paste(levels(dataFrame$SuMean)[2], rownames(K1), sep = ":")
    # K <- rbind(K1, K2)
    # colnames(K) <- c(colnames(Tukey), colnames(Tukey))
    # print(summary(glht(model, linfct = K %*% X)))
    
    
    
    
    # k1 <- glht(model, mcp(NovEx = "Tukey"))$linfct
    # k2 <- glht(model, mcp(SurMean = "Tukey"))$linfct
    # 
    # print(summary(glht(model, linfct = rbind(k1,k2))))
    
  }
  a <- 0
}

# Outdated, see anovaSecondB()
anovaSecond = function(expertList, noviceList){
  anovaResults <- c()
  for(slideCount in 1:nrow(expertList[[1]])){
    dataFrame <- data.frame(NovEx = character(), Percentage = double())
    for(expertCount in 1:length(expertList)){
      dataFrame <- rbind(dataFrame, list(NovEx = "ExpertSurface",Percentage = expertList[[expertCount]]$SurfacePercentage[[slideCount]]))
      dataFrame <- rbind(dataFrame, list(NovEx = "ExpertMeaningful",Percentage = expertList[[expertCount]]$MeaningfulPercentage[[slideCount]]))
    } 
    for(noviceCount in 1:length(noviceList)){
      if(noviceCount == 1 || noviceCount == 6){
        if(slideCount > 1){
          dataFrame <- rbind(dataFrame, list(NovEx = "NoviceSurface",Percentage = noviceList[[noviceCount]]$SurfacePercentage[[slideCount-1]]))
          dataFrame <- rbind(dataFrame, list(NovEx = "NoviceMeaningful",Percentage = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount-1]]))
        }
      }else{
        dataFrame <- rbind(dataFrame, list(NovEx = "NoviceSurface",Percentage = noviceList[[noviceCount]]$SurfacePercentage[[slideCount]]))
        dataFrame <- rbind(dataFrame, list(NovEx = "NoviceMeaningful",Percentage = noviceList[[noviceCount]]$MeaningfulPercentage[[slideCount]]))
      }
    }
    # anovaResults <- c(anovaResults, aov(Percentage ~ NovEx, dataFrame))
    print(names(pages)[[slideCount]])
    # print(summary(aov(Percentage ~ NovEx, dataFrame)))
    print(posthocPairwiseT(aov(Percentage ~ NovEx, dataFrame)))
    
  }
  a <- 0
}