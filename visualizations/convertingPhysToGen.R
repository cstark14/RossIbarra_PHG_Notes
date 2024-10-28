#### code to convert phys to gen positions using a gen map as a ref/frame
#### Jeff recommended approx.fun
# library(tidyverse)
# ogut<-read.table("~/Desktop/ogut_fifthcM_map_agpv4_INCLUDE.txt",header=F)
# colnames(ogut)=c("marker","marker_num","cm","chr","pos")
# 
# get_cM<-function(cr,p){
#   temp<-filter(ogut,chr==cr) 
#   return(approx(temp$pos,temp$cm,p)$y)
# }
# 
# get_cM(1,13587889)


#### Below will be code using loess
loessFunctions <- function(){
  library(stringr)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(tidyverse)
  library(plyr)
  library(fANCOVA)
  library(readxl)
  library(openxlsx)
  "%notin%" <-  Negate("%in%")
  options(stringsAsFactors = FALSE)
  options(scipen = 999)
  options(digits=10)
  
  genMapWorkbook <- createWorkbook("IntegratedGeneticMap")
  addWorksheet(genMapWorkbook,"integratedGenMap")
  
  
  uniquePhysPos <- function(physpos) {
    physpos$ChrPos <- paste0(physpos$Chromosome,"_",physpos$Position)
    physposDupes <- physpos %>% filter(duplicated(ChrPos)) %>% mutate(Position = Position+1) %>% mutate(ChrPos=paste0(Chromosome,"_",Position))
    if(nrow(physposDupes) >=1){
      for(i in 1:10){
        if(i==1){
          physposDupes <- physpos %>% filter(duplicated(ChrPos)) %>% mutate(Position = Position+1) %>% mutate(ChrPos=paste0(Chromosome,"_",Position))
          physposNotDupes <- physpos %>% filter(!duplicated(ChrPos))
        } else{
          physposNotDupes <- finalPhysPos %>% filter(!duplicated(ChrPos))
          physposDupes <- finalPhysPos %>% filter(duplicated(ChrPos)) %>% mutate(Position = Position+1) %>% mutate(ChrPos=paste0(Chromosome,"_",Position))
        }
        finalPhysPos <- rbind(physposNotDupes,physposDupes)
        if(nrow(finalPhysPos %>% filter(duplicated(ChrPos)))==0){break}
      }
    }
    return(finalPhysPos)
  }
  
  addBeginEndDummys <- function(startTable,chrFastaIndex,chrPrefix) {
    chrs <- unique(startTable$Chromosome)
    endTable <- startTable
    for(i in 1:length(chrs)){
      currentChr <- chrs[i]
      if(as.numeric(gsub(chrPrefix,"",currentChr)==0)){next}
      chrTable <- startTable %>% 
        filter(Chromosome==currentChr) %>% 
        arrange(Position)
      maxCM <- ifelse(max(chrTable$cM,na.rm=T)==0,max(endTable$chrTable,na.rm=T),max(chrTable$cM,na.rm=T))
      addedMarkers <- data.frame(MRN=c(paste0("Dummy0_",currentChr),paste0("DummyEnd_",currentChr)),
                                 Chromosome=currentChr,
                                 Position=c(1,chrFastaIndex[which(chrFastaIndex$Contig==currentChr),"Length"]),
                                 #Orientation=1,
                                 LG=c(as.numeric(gsub(chrPrefix,"",currentChr)),as.numeric(gsub(chrPrefix,"",currentChr))),
                                 cM=c(0.0,maxCM))
      endTable <- rbind.fill(endTable,addedMarkers) %>% distinct(.)
    }
    endTable <- endTable %>% arrange(Chromosome,Position)
    return(endTable)
  }
  
  createPlot <- function(dataToPlot) {
    tempPlot <- ggplot(data = dataToPlot, aes(x = Position, y = cM, color = LG)) + 
      #geom_point(aes(text=name,size = 0.5), name="MRN") + 
      geom_point(size = 0.8) + 
      scale_x_continuous(labels = label_number(suffix = "Mbp", scale = 1e-6), 
                         expand = c(0,0), 
                         limits = c(0, NA)) +  
      facet_wrap(~Chromosome, scales = "free" ) + 
      theme_bw()
    return(tempPlot)
  }
  
  createInteractivePlot <- function(dataToPlotBase,whichChr,chrPrefix){
    dataToPlot <- dataToPlotBase %>% 
      filter(Chromosome==paste0(chrPrefix,whichChr)) %>%
      mutate(LG=as.character(LG))
    p <- plot_ly() %>% 
      add_trace(data = dataToPlot, 
                x = ~Position, y = ~cM, size = 0.2, color = ~LG, 
                colors="Set1", type = "scatter", mode = "markers",
                key = ~dataToPlot$MRN, 
                hoverinfo = 'text', text = ~paste("MRN: ", 
                                                  dataToPlot$MRN, 
                                                  "<br>Physical position:",
                                                  dataToPlot$Position, 
                                                  "<br>Genetic position:", 
                                                  dataToPlot$cM, 
                                                  "<br>Strand:", 
                                                  dataToPlot$Orientation)) 
    return(p)
    #%>% add_trace(x = dataToPlot[dataToPlot$Chromosome == chr,]$Position[phys.order], 
    #            y = ~loFit$fitted[phys.order], type = "scatter", 
    #            mode = "lines",opacity = 0.5, line = list(color = "black") )
  }
  
  addGapDummys <- function(startTable,avgGenPos=TRUE) {
    chrs <- as.numeric(levels(unique(startTable$LG)[startTable$LG]))
    endTable <- startTable %>% mutate(bpToNextMarker=NA) %>% filter(Chromosome=="20")
    for(i in chrs){
      currentChr <- i
      if(currentChr==0) {next}
      chrTable <- startTable %>% 
        filter(LG==currentChr) %>% 
        arrange(Position,cM) %>%
        mutate(bpToNextMarker=Position-lag(Position,n=1L))
      chrTable$bpToNextMarker <- c(chrTable[-1,"bpToNextMarker"], NA)
      rowsWithMoreThan1Mbp <- chrTable[which(chrTable$bpToNextMarker > 1e6),]
      if(nrow(rowsWithMoreThan1Mbp) >=1){
        indicesOfRows <- row.names(rowsWithMoreThan1Mbp)
        for(j in 1:nrow(rowsWithMoreThan1Mbp)){
          currentMRN <- rowsWithMoreThan1Mbp[j,"MRN"]
          if(grepl("Dummy",currentMRN)){next}
          prevMarkerPos <- rowsWithMoreThan1Mbp[j,"Position"]
          nextMarkerPos <- chrTable[as.numeric(row.names(rowsWithMoreThan1Mbp[j,]))+1,"Position"]
          prevGenPos <- rowsWithMoreThan1Mbp[j,"cM"]
          nextGenPos <- chrTable[as.numeric(row.names(rowsWithMoreThan1Mbp[j,]))+1,"cM"]
          if (rowsWithMoreThan1Mbp[j,"bpToNextMarker"]>= 5*1e6){
            distanceDiv10 <- rowsWithMoreThan1Mbp[j,"bpToNextMarker"] / 10
            positionsToAdd <- c(round(prevMarkerPos + distanceDiv10),
                                round(prevMarkerPos + (distanceDiv10*2)),
                                round(prevMarkerPos + (distanceDiv10*3)),
                                round(prevMarkerPos + (distanceDiv10*4)),
                                round(prevMarkerPos + (distanceDiv10*5)),
                                round(prevMarkerPos + (distanceDiv10*6)),
                                round(prevMarkerPos + (distanceDiv10*7)),
                                round(prevMarkerPos + (distanceDiv10*8)),
                                round(prevMarkerPos + (distanceDiv10*9)))
            if(avgGenPos){
              #genDistanceDiv4 <- (nextGenPos-prevGenPos) / 4
              #genAddPositions <- c((prevGenPos+genDistanceDiv4),
              #                     (prevGenPos+genDistanceDiv4+genDistanceDiv4),
              #                     (prevGenPos+genDistanceDiv4+genDistanceDiv4+genDistanceDiv4))
              genDistanceDiv2 <- (nextGenPos-prevGenPos) / 2
              genAddPositions <- c(prevGenPos+genDistanceDiv2)
              addedMarkersTable <- data.frame(MRN=c(paste0("Dummy1_",currentMRN),
                                                    paste0("Dummy2_",currentMRN),
                                                    paste0("Dummy3_",currentMRN),
                                                    paste0("Dummy4_",currentMRN),
                                                    paste0("Dummy5_",currentMRN),
                                                    paste0("Dummy6_",currentMRN),
                                                    paste0("Dummy7_",currentMRN),
                                                    paste0("Dummy8_",currentMRN),
                                                    paste0("Dummy9_",currentMRN)
              ),
              Chromosome=rowsWithMoreThan1Mbp[j,"Chromosome"],
              Position=positionsToAdd,
              #Orientation=rowsWithMoreThan1Mbp[j,"Orientation"],
              LG=rowsWithMoreThan1Mbp[j,"LG"],
              cM=genAddPositions)
            } else {
              addedMarkersTable <- data.frame(MRN=c(paste0("Dummy1_",currentMRN),
                                                    paste0("Dummy2_",currentMRN),
                                                    paste0("Dummy3_",currentMRN),
                                                    paste0("Dummy4_",currentMRN),
                                                    paste0("Dummy5_",currentMRN),
                                                    paste0("Dummy6_",currentMRN),
                                                    paste0("Dummy7_",currentMRN),
                                                    paste0("Dummy8_",currentMRN),
                                                    paste0("Dummy9_",currentMRN),
              ),
              Chromosome=rowsWithMoreThan1Mbp[j,"Chromosome"],
              Position=positionsToAdd,
              #Orientation=rowsWithMoreThan1Mbp[j,"Orientation"],
              LG=rowsWithMoreThan1Mbp[j,"LG"],
              cM=prevGenPos)
            }
          } else if(rowsWithMoreThan1Mbp[j,"bpToNextMarker"] >= 1.5e6){
            distanceDiv4 <- rowsWithMoreThan1Mbp[j,"bpToNextMarker"] / 4
            positionsToAdd <- c(round(prevMarkerPos + distanceDiv4),
                                round(prevMarkerPos + distanceDiv4 + distanceDiv4),
                                round(prevMarkerPos + distanceDiv4 + distanceDiv4 + distanceDiv4))
            if(avgGenPos){
              #genDistanceDiv4 <- (nextGenPos-prevGenPos) / 4
              #genAddPositions <- c((prevGenPos+genDistanceDiv4),
              #                     (prevGenPos+genDistanceDiv4+genDistanceDiv4),
              #                     (prevGenPos+genDistanceDiv4+genDistanceDiv4+genDistanceDiv4))
              genDistanceDiv2 <- (nextGenPos-prevGenPos) / 2
              genAddPositions <- c(prevGenPos+genDistanceDiv2)
              addedMarkersTable <- data.frame(MRN=c(paste0("Dummy1_",currentMRN),
                                                    paste0("Dummy2_",currentMRN),
                                                    paste0("Dummy3_",currentMRN)),
                                              Chromosome=rowsWithMoreThan1Mbp[j,"Chromosome"],
                                              Position=positionsToAdd,
                                              #Orientation=rowsWithMoreThan1Mbp[j,"Orientation"],
                                              LG=rowsWithMoreThan1Mbp[j,"LG"],
                                              cM=genAddPositions)
            } else {
              addedMarkersTable <- data.frame(MRN=c(paste0("Dummy1_",currentMRN),
                                                    paste0("Dummy2_",currentMRN),
                                                    paste0("Dummy3_",currentMRN)),
                                              Chromosome=rowsWithMoreThan1Mbp[j,"Chromosome"],
                                              Position=positionsToAdd,
                                              #Orientation=rowsWithMoreThan1Mbp[j,"Orientation"],
                                              LG=rowsWithMoreThan1Mbp[j,"LG"],
                                              cM=prevGenPos)
            }
            
          } else{
            distanceDiv2 <- rowsWithMoreThan1Mbp[j,"bpToNextMarker"] / 2
            if(avgGenPos){
              positionsToAdd <- round(prevMarkerPos + distanceDiv2)
              genDistanceDiv2 <- (nextGenPos-prevGenPos) / 2
              addedMarkersTable <- data.frame(MRN=paste0("Dummy1_",currentMRN),
                                              Chromosome=rowsWithMoreThan1Mbp[j,"Chromosome"],
                                              Position=positionsToAdd,
                                              #Orientation=rowsWithMoreThan1Mbp[j,"Orientation"],
                                              LG=rowsWithMoreThan1Mbp[j,"LG"],
                                              cM=prevGenPos+genDistanceDiv2)
            } else{
              positionsToAdd <- round(prevMarkerPos + distanceDiv2)
              addedMarkersTable <- data.frame(MRN=paste0("Dummy1_",currentMRN),
                                              Chromosome=rowsWithMoreThan1Mbp[j,"Chromosome"],
                                              Position=positionsToAdd,
                                              #Orientation=rowsWithMoreThan1Mbp[j,"Orientation"],
                                              LG=rowsWithMoreThan1Mbp[j,"LG"],
                                              cM=prevGenPos)
            }
            
          }
          endTable <- rbind.fill(endTable,chrTable,addedMarkersTable) %>% distinct(.)
        }
      } else {
        endTable <- rbind.fill(endTable,chrTable) %>% distinct(.)
      }
    }
    endTable <- endTable %>% arrange(Chromosome,Position)
    return(endTable)
  }
  
  flagDecreasing <- function(){
    # Pre-Loess Flagging of "decreasing" markers ####
    # for(k in 1:nrow(spanTable)){
    #   currentChr <- spanTable[k,"Chromosome"]
    #   currentSubset <- filteredGenAndPhysPositions %>% filter(Chromosome==currentChr) %>% arrange(Position,cM)
    #   unname(which(sapply(currentSubset, function(x) any(diff(x) < 0))))
    #   
    #   
    #   physPosRanking <- rank(currentSubset$Position) + sort(runif(length(currentSubset$Position)), decreasing=TRUE)
    #   physPosFinal <- replace(currentSubset$Position, seq_along(currentSubset$Position) >= which(physPosRanking < cummax(physPosRanking))[1], NA)
    #   
    #   genPosRanking <- rank(currentSubset$Position) + sort(runif(length(currentSubset$Position)), decreasing=TRUE)
    #   genPosFinal <- 
    # }
  }
  
  getFancovaSpans <- function(span.table,min.span,frameMap) {
    for(i in 1:nrow(span.table)) {
      currentChr <- unique(frameMap$Chromosome)[[i]]
      frameMapChr <- frameMap %>% filter(Chromosome==currentChr)
      if(nrow(frameMapChr) < 10) {
      } else {
        loFit <- loess.as(x = frameMapChr$Position, 
                          y = frameMapChr$cM, degree = 1, criterion = c("aicc", "gcv")[1], user.span = NULL, plot = F)
        span.table[span.table$Chromosome == currentChr,"fANCOVASpan"] <- ifelse(loFit$pars$span>=min.span,loFit$pars$span,0.15)
      }
    }
    return(span.table)
  }
  
  runLoess <- function(span.table,frameMap,refMapPreLoess,chrPrefix,roundCM=FALSE,shiftTo0=FALSE) {
    for(j in 1:nrow(span.table)) {
      currentChr <- span.table[j,"Chromosome"]
      currentSpan <- span.table[j,"fANCOVASpan"]
      frameMapChr <- frameMap %>%
        filter(Chromosome==currentChr)
      if(is.na(currentSpan)){
      } else {
        loFit <- suppressWarnings(loess(frameMapChr$cM ~ frameMapChr$Position, 
                                        degree=1, span=currentSpan, control=loess.control(surface = "direct") ))
        loPos <- predict(loFit, refMapPreLoess[refMapPreLoess$Chromosome == currentChr,]$Position, se=T )
        
        ### below code will shift Chr if the min is negative
        minPos <- min(loPos$fit)
        if(minPos <0){
          loPos$fit <- loPos$fit + abs(minPos)
        } else{
          if (shiftTo0){
            loPos$fit <- loPos$fit - minPos
          }
        }
        
        loPosRound <- loPos
        
        if(roundCM > 0){
          loPosRound$fit <- round(loPosRound$fit, roundCM)
          refMapPreLoess[refMapPreLoess$Chromosome == currentChr,"InterpolatedGenPosRounded"] <- loPosRound$fit
          refMapPreLoess[refMapPreLoess$Chromosome == currentChr,"InterpolatedGenPos"] <- loPos$fit
        } else {
          refMapPreLoess[refMapPreLoess$Chromosome == currentChr,"InterpolatedGenPos"] <- loPos$fit
        }
        refMapPreLoess[refMapPreLoess$Chromosome == currentChr,"AssumedLG"] <- gsub(chrPrefix,"",currentChr)
        
      }
    }
    #refMapPreLoess[grepl("Dummy0",refMapPreLoess$MRN,perl=TRUE) & !is.na(refMapPreLoess$InterpolatedGenPos),"InterpolatedGenPos"] <- 0.00
    return(refMapPreLoess)
  }
  
  plotLoess <- function(interpolatedRefMap,rounded=FALSE) {
    # post loess plot ####
    if(rounded){
      loess.plot <- ggplot(data = interpolatedRefMap, aes(x = Position, y = InterpolatedGenPosRounded, color = AssumedLG)) + 
        geom_point(size = 0.5) + 
        scale_x_continuous(labels = label_number(suffix = "Mbp", scale = 1e-6), 
                           expand = c(0,0), 
                           limits = c(0, NA)) +  
        facet_wrap(~Chromosome, scales = "free" ) + 
        theme_bw()
      return(loess.plot)
    } else{
      loess.plot <- ggplot(data = interpolatedRefMap, aes(x = Position, y = InterpolatedGenPos, color = AssumedLG)) + 
        geom_point(size = 0.5) + 
        scale_x_continuous(labels = label_number(suffix = "Mbp", scale = 1e-6), 
                           expand = c(0,0), 
                           limits = c(0, NA)) +  
        facet_wrap(~Chromosome, scales = "free" ) + 
        theme_bw()
      return(loess.plot)
    }
    
  }
  
  createInteractivePlotPostLoess <- function(dataToPlotBase,whichChr,chrPrefix,rounded=FALSE){
    dataToPlot <- dataToPlotBase %>% 
      filter(Chromosome==paste0(chrPrefix,whichChr)) %>%
      mutate(AssumedLG=as.character(AssumedLG))
    if(rounded){
      p2 <- plot_ly() %>% 
        add_trace(data = dataToPlot, 
                  x = ~Position, y = ~InterpolatedGenPosRounded, size = 0.2, color = ~AssumedLG, 
                  colors="Set1", type = "scatter", mode = "markers",
                  key = ~dataToPlot$MRN, 
                  hoverinfo = 'text', text = ~paste("MRN: ", 
                                                    dataToPlot$MRN, 
                                                    "<br>Physical position:",
                                                    dataToPlot$Position, 
                                                    "<br>Interpolated Rounded Genetic position:", 
                                                    dataToPlot$InterpolatedGenPosRounded, 
                                                    "<br>Original Genetic position:", 
                                                    dataToPlot$cM, 
                                                    "<br>Strand:", 
                                                    dataToPlot$Orientation)) 
    } else {
      p2 <- plot_ly() %>% 
        add_trace(data = dataToPlot, 
                  x = ~Position, y = ~InterpolatedGenPos, size = 0.2, color = ~AssumedLG, 
                  colors="Set1", type = "scatter", mode = "markers",
                  key = ~dataToPlot$MRN, 
                  hoverinfo = 'text', text = ~paste("MRN: ", 
                                                    dataToPlot$MRN, 
                                                    "<br>Physical position:",
                                                    dataToPlot$Position, 
                                                    "<br>Interpolated Genetic position:", 
                                                    dataToPlot$InterpolatedGenPos, 
                                                    "<br>Original Genetic position:", 
                                                    dataToPlot$cM, 
                                                    "<br>Strand:", 
                                                    dataToPlot$Orientation)) 
    }
    
    return(p2)
    #%>% add_trace(x = dataToPlot[dataToPlot$Chromosome == chr,]$Position[phys.order], 
    #            y = ~loFit$fitted[phys.order], type = "scatter", 
    #            mode = "lines",opacity = 0.5, line = list(color = "black") )
  }
  
  makeUniqueGenPos_breakTies <- function(intGenMap){
    e <- intGenMap
    LGs <- unique(e[,"AssumedLG"])
    #LGs <- unique(intGenMap[,"AssumedLG"])
    #LG_i <- 5
    #now, identify markers with tied genetic positions, sort and bump their cM to avoid ties. for now, just do it with spacing them by 10 bp, centered on the cM
    #spacing in cM space
    spacing <- 0.00001
    for(LG_i in LGs){
      is_this_LG <- e[,"AssumedLG"]==LG_i
      unique_cM_counts <- sort(table(e[is_this_LG,"InterpolatedGenPos"]),decreasing=TRUE)
      unique_cM_counts_gt1 <- as.numeric(names(unique_cM_counts[unique_cM_counts > 1]))
      if(length(unique_cM_counts_gt1) > 0){
        #j <- 1
        for(j in 1:length(unique_cM_counts_gt1)){
          the_cM <- unique_cM_counts_gt1[j]
          rows_in_question <- (1:nrow(e))[is_this_LG & (e[,"InterpolatedGenPos"]==the_cM)]
          if(length(rows_in_question) == 0){next()}
          rows_in_question <- rows_in_question[!is.na(rows_in_question)]
          rows_sorted_by_bp <- rows_in_question[order(e[rows_in_question,"Position"])]
          e[rows_in_question,] <- e[rows_sorted_by_bp,]
          row_that_isnt_changed <- floor(median(1:length(rows_in_question)))
          #so... the row_that_isnt_changed doesn't work right with odd numbers, its off by one, but frankly I don't care
          offset_cM <- seq(from=the_cM-(spacing*(row_that_isnt_changed-1)),to=the_cM+(spacing*row_that_isnt_changed),length.out=length(rows_in_question))
          if(offset_cM[1] < 0){
            offset_cM <- offset_cM + (-1*offset_cM[1])
          }
          e[rows_in_question,"InterpolatedGenPos"] <- offset_cM
        }
      }
    }
    return(e)
    
  }
  
  breakGenPosTies <- function(intGenMap){
    LGs <- unique(intGenMap[,"AssumedLG"])
    for(l in 1:length(LGs)){
      currentLG <- LGs[l]
      is_this_LG <- intGenMap[,1]==currentLG
      lgMap <- intGenMap %>% filter(AssumedLG==currentLG)
      unique_cM_counts <- sort(table(lgMap[,"InterpolatedGenPos"]),decreasing=TRUE)
      unique_cM_counts_gt1 <- as.numeric(names(unique_cM_counts[unique_cM_counts > 1]))
      if(length(unique_cM_counts_gt1) > 0){
        j <- 1
        for(j in 1:length(unique_cM_counts_gt1)){
          the_cM <- unique_cM_counts_gt1[j]
          rows_in_question <- which(lgMap[,"InterpolatedGenPos"]==the_cM)
          rows_sorted_by_bp <- rows_in_question[order(intGenMap[rows_in_question,"BP"])]
          intGenMap[rows_in_question,] <- intGenMap[rows_sorted_by_bp,]
          row_that_isnt_changed <- floor(median(1:length(rows_in_question)))
          #so... the row_that_isnt_changed doesn't work right with odd numbers, its off by one, but frankly I don't care
          offset_cM <- seq(from=the_cM-(spacing*(row_that_isnt_changed-1)),to=the_cM+(spacing*row_that_isnt_changed),length.out=length(rows_in_question))
          if(offset_cM[1] < 0){
            offset_cM <- offset_cM + (-1*offset_cM[1])
          }
          intGenMap[rows_in_question,"cM"] <- offset_cM
        }
      }
    }
  }
}

loessFunctionsTotal <- function(){
  ### Must install packages ONCE at the start of the workspace
  install.packages("fANCOVA")
  install.packages("openxlsx")
  
  source("/repos/RefMap_Scripts/geneticMapping_Functions.R")
  
  ### Establish variables ####
  cropName <- "Pepper"
  outdir <- "/domino/datasets/local/GenomicResources/Pepper/Pepper_Map_20240924/"
  
  physPosFile <- paste0(outdir,"CapAnn_SBR991318_BayerV1_MarkerPhysPos_2024-09-24.csv")
  consensusMapFile <- "/domino/datasets/local/GenomicResources/Pepper/Pepper_Map_20240924/Pepper_Consensus.xlsx"
  rosettaFromVertimus <- "/domino/datasets/local/GenomicResources/Pepper/Pepper_Map_20240924/rosetta_stone_view.csv"
  
  filenameToSaveInitialPlots <- paste0(outdir,"genPhysCorrelationPlotsAsIs.pdf")
  filenameToSavePreLoess <- paste0(outdir,"genPhysCorrelationPlotsPreLoess.pdf")
  filenameToSavePostLoessPlots <- paste0(outdir,"genPhysCorrelationPlotsPostLoess.pdf")
  excelFileToSaveGeneticMap <- paste0(outdir,"Pepper_IntegratedGeneticMap","_",Sys.Date(),".xlsx")
  genomeFileFromGmapDB <- "/domino/datasets/gmapDatabase/Capsicum_annuum/SBR-99-1318_BayerV1.0.0/CAPAN_SBR-99-1318_v1_0_0.fa.gz"
  
  workingDirectory <- getwd()
  # Importing data ####
  
  # importing the physical positions out of GMAP
  physPosTable <- read.csv(physPosFile, check.names = F, sep = ",", strip.white = T, comment.char = "")
  
  # Importing Consensus Map ###
  consensusMap <- read_excel(consensusMapFile)
  colnames(consensusMap)[3] <- "cM"
  colnames(consensusMap)[2] <- "LG"
  colnames(consensusMap) <- gsub("Marker","MRN",colnames(consensusMap))
  consensusMap <- consensusMap %>%
    filter(LG != "-") %>%
    filter(!is.na(LG)) %>%
    mutate(LG=as.numeric(LG)) %>%
    mutate(cM=as.numeric(cM)) %>%
    arrange(LG,cM) %>%
    #filter(which(is.numeric(LG))) %>%
    mutate(prevMapMarkerIndex = 1:nrow(.))
  
  
  # Using fasta from GMAP db, creating index, importing index table 
  system(paste0("cp ",genomeFileFromGmapDB," ",workingDirectory, "&& gunzip ",basename(genomeFileFromGmapDB)))
  baseGenomeName <- gsub(".gz","",basename(genomeFileFromGmapDB))
  system(paste0("samtools faidx ",workingDirectory,"/",baseGenomeName))
  genomeIndex <- read.table(paste0(baseGenomeName,".fai"),sep="\t",header = F)
  colnames(genomeIndex) <- c("Contig","Length","ByteStartIndex","BPperLine","BytesPerLine")
  system(paste0("rm ",workingDirectory,"/",baseGenomeName,"*"))
  #### NOTE: This next step must be written in manually after pulling in the index file. 
  #### Hoping the standarization of contig naming will help automate this step
  chrNamePrefix <- "CAPAN_SBR-99-1318_v1_0_0_chr"
  
  # Initial Plotting ####
  
  ## remove markers with no position and in intervals
  ### keep MRN,Chromosome,Position,Orientation columns
  ### change Position to numeric
  physPosTable <- physPosTable %>% 
    filter(!is.na(Chromosome)) %>%
    filter(!str_detect(Position,"-")) %>%
    filter(Chromosome != paste0(chrNamePrefix,"0")) %>%
    filter(Chromosome != paste0(chrNamePrefix,"10000001")) %>%
    #select(MRN,Chromosome,Position,Orientation) %>%
    select(MRN,Chromosome,Position) %>%
    mutate(Position=as.numeric(Position))
  #### merging genetic and physical positions, keeping all
  genPhysPositionsMerged <- merge(physPosTable,consensusMap,by="MRN",all=T) %>% 
    filter(!is.na(Chromosome)) %>% 
    select(- prevMapMarkerIndex) %>%
    distinct()
  ### add 0bp,0cm first marker, and last bp position marker
  genPhysPositions <- addBeginEndDummys(startTable=genPhysPositionsMerged,
                                        chrFastaIndex=genomeIndex,
                                        chrPrefix=chrNamePrefix)
  
  #### filtering all positions to those that have both gen and phys
  ##### change LG to factored character
  genAndPhysPositions <- genPhysPositions %>% filter(complete.cases(.)) %>% 
    mutate(LG=factor(as.character(LG)))
  
  ## generate plot for current state of correlation
  currentCorPlot <- createPlot(genAndPhysPositions)
  currentCorPlot
  
  ggsave(filenameToSaveInitialPlots,plot=currentCorPlot,height=5,width=10,units="in")
  
  ##OPTIONAL: export of current state correlation  #must manually change this file name
  write.csv(genAndPhysPositions, "/domino/datasets/local/GenomicResources/Pepper/Pepper_Map_20240305/GenAndPhysPositions_V1.csv", row.names = FALSE)
  
  #### Interactively graph chr by chr to identify bad markers, will need to run one by one
  createInteractivePlot(dataToPlotBase = genAndPhysPositions,whichChr = "2",
                        chrPrefix = chrNamePrefix)
  
  # Remove MRNs to better fit loess ####
  mrnsToExclude <- c()
  filteredGenAndPhysPositions <- genAndPhysPositions %>%
    filter(MRN %notin% mrnsToExclude)
  
  #### Run function to insert dummy markers if there's a gap more than 1Mbp
  filteredGenAndPhysWithDummyMRNs <- addGapDummys(startTable=filteredGenAndPhysPositions,
                                                  avgGenPos=TRUE)
  
  #### Rerun plot with removed markers 
  filteredPlot <- createPlot(filteredGenAndPhysWithDummyMRNs)
  filteredPlot
  
  ggsave(filenameToSavePreLoess,plot=filteredPlot,height=5,width=10,units="in")
  
  #### Interactively graph chr by chr to identify ANY ADDITIONAL markers, will need to run one by one
  createInteractivePlot(dataToPlotBase = filteredGenAndPhysWithDummyMRNs,whichChr = "7",
                        chrPrefix = chrNamePrefix)
  
  ### span table to be used for fANCOVA but creating early as an indexing point ####
  spanTable1 <- data.frame(Chromosome = unique(filteredGenAndPhysWithDummyMRNs$Chromosome),fANCOVASpan=NA)
  
  # Pre-Loess Flagging of "decreasing" markers ####
  ### TO-DO
  
  #  Remove flagged markers from genPhysPositions table ####
  filteredGenPhysPositions <- genPhysPositions %>%
    filter(MRN %notin% mrnsToExclude)
  
  filteredGenAndPhysWithDummyMRNs$LG <- as.numeric(as.character(filteredGenAndPhysWithDummyMRNs$LG))
  
  # Use Applying Loess using fANCOVA for span values, then apply back to regular loess ####
  ## fANCOVA loess.as for span values
  ### NOTE: there is an ifelse which excludes any chromosomes with less than 10 datapoints (loess.as has a minimum, not sure what it is)
  ### IDEA: getting reminded of the output for Tyler's marker picking with a slider for spans. useful?
  spanTable <- getFancovaSpans(span.table=spanTable1,
                               min.span=0.15,
                               frameMap=filteredGenAndPhysWithDummyMRNs)
  
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.19
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.18
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.17
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.19
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.19
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.17
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.17
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.15
  spanTable[which(spanTable$Chromosome == ""),"fANCOVASpan"] <- 0.19
  
  ## regular loess to interpolate gen positions for all phys positions
  #### AssumedLG is based on the Physical Chr
  integratedGenMap <- runLoess(span.table=spanTable,
                               frameMap=filteredGenAndPhysWithDummyMRNs,
                               refMapPreLoess=filteredGenPhysPositions,
                               chrPrefix=chrNamePrefix,
                               roundCM=2,
                               shiftTo0=TRUE) %>%
    mutate(AssumedLG=as.factor(as.character(AssumedLG)))
  
  loessPlot <- plotLoess(integratedGenMap)
  loessPlot
  
  
  #### Interactively graph chr by chr to identify ANY ISSUES WITH POST LOESS marker positions, will need to run one by one
  createInteractivePlotPostLoess(dataToPlotBase = integratedGenMap,whichChr = "9",
                                 chrPrefix = chrNamePrefix)
  
  #### Below line will overlay trendline 
  # loessPlot + stat_smooth()
  
  #### Save post loess plots
  ggsave(filenameToSavePostLoessPlots,plot=loessPlot,height=5,width=10,units="in")
  
  ### write out file of post loess positions
  write.csv(integratedGenMap, file = paste0(outdir,'Pepper_map_integratedGenMap_20240924_v2.csv',row.names = FALSE))
  
  #### Mutate LG column to no longer be a factor/character for ease of excel import
  integratedGenMap$AssumedLG <- as.numeric(as.character(integratedGenMap$AssumedLG))
  
  ### Make unique positions 
  intGenMapOut <- makeUniqueGenPos_breakTies(intGenMap = integratedGenMap) %>%
    filter(!grepl("Dummy",MRN)) %>%
    filter(!is.na(InterpolatedGenPos))
  
  # Write outputs to multi-sheet Excel ####
  genMapWithDummies <- filteredGenAndPhysWithDummyMRNs %>%
    mutate(LG=as.character(LG))
  
  # Comparing previous Maps
  
  writeData(genMapWorkbook,sheet="integratedGenMap",intGenMapOut,rowNames=F)
  writeData(genMapWorkbook,sheet="spanTable",spanTable, rowNames = F)
  
  saveWorkbook(genMapWorkbook,excelFileToSaveGeneticMap,overwrite=T)
}