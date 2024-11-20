library(tidyverse)
library(dplyr)
library(fANCOVA)
library(readr)
library(reshape2)
library(ggplot2)
library(scales)
# library(RColorBrewer)
library(colorBlindness)
#library(plyr)
options(scipen=999)
options(stringsAsFactors=FALSE)
"%notin%" <- Negate("%in%")

#### shifting negative gen pos to 0, adding to the rest of the cm for that chr
shiftGenMapTo0 <- function(gen.map){
  chromosomes <- unique(gen.map$chr)
  gen.map$cmOG <- gen.map$cm
  for(chr in 1:length(chromosomes)){
    currentChr <- chr
    genMapForChr <- gen.map %>% filter(chr==currentChr)
    minGenPos <- min(genMapForChr$cm)
    if(minGenPos < 0){
      gen.map[which(gen.map$chr==currentChr),"cm"] <- gen.map[which(gen.map$chr==currentChr),"cm"] + -(minGenPos)
    }
  }
  return(gen.map)
}

### Loess ####
getFancovaSpans <- function(span.table,min.span,frameMap) {
  for(i in 1:nrow(span.table)) {
    currentChr <- unique(frameMap$chr)[[i]]
    frameMapChr <- frameMap %>% filter(chr==currentChr)
    if(nrow(frameMapChr) < 10) {
    } else {
      loFit <- loess.as(x = frameMapChr$pos, 
                        y = frameMapChr$cm, degree = 1, criterion = c("aicc", "gcv")[1], user.span = NULL, plot = F)
      span.table[span.table$Chromosome == currentChr,"fANCOVASpan"] <- ifelse(loFit$pars$span>=min.span,loFit$pars$span,min.span)
    }
  }
  return(span.table)
}

runLoess <- function(span.table,frameMap,physPosToConvert,chrPrefix,roundCM=FALSE,shiftTo0=FALSE) {
  for(j in 1:nrow(span.table)) {
    currentChr <- span.table[j,"Chromosome"]
    currentSpan <- span.table[j,"fANCOVASpan"]
    frameMapChr <- frameMap %>%
      filter(chr==currentChr)
    if(is.na(currentSpan)){
    } else {
      loFit <- suppressWarnings(loess(frameMapChr$cm ~ frameMapChr$pos, 
                                      degree=1, span=currentSpan, control=loess.control(surface = "direct") ))
      loPos <- predict(loFit, physPosToConvert[physPosToConvert$chr == currentChr,"value"], se=T )
      
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
        physPosToConvert[physPosToConvert$chr == currentChr,"InterpolatedGenPosRounded"] <- loPosRound$fit
        physPosToConvert[physPosToConvert$chr == currentChr,"InterpolatedGenPos"] <- loPos$fit
      } else {
        physPosToConvert[physPosToConvert$chr == currentChr,"InterpolatedGenPos"] <- loPos$fit
      }
      physPosToConvert[physPosToConvert$chr == currentChr,"AssumedLG"] <- ifelse(is.numeric(currentChr),currentChr,gsub(chrPrefix,"",currentChr))
      
    }
  }
  return(physPosToConvert)
}



combineRegionsInCMWindow_mode <- function(precombinedRegions, window.size, min.size) {
  combined <- data.frame(start = numeric(0), end = numeric(0), sample1 = character(0), stringsAsFactors = FALSE)
  i <- 1
  #while (i <= 354){
  while (i <= nrow(precombinedRegions)){
    current_start <- precombinedRegions$start[i]
    first_end <- precombinedRegions$end[i]
    current_end <- first_end
    current_label <- precombinedRegions$sample1[i]
    currentChrom <- precombinedRegions$chrom[i]
    currentLength <- precombinedRegions$length[i]
    # print(paste0("first start: ",current_start))
    # print(paste0("first end: ",first_end))
    # print(current_label)
    
    if(currentLength < min.size){
      #break
      if (is.numeric(window.size)){
        j <- i + 1
        windowEnd <- NA
        while (j <= nrow(precombinedRegions) && precombinedRegions$start[j] <= first_end + window.size) {
          windowEnd <- precombinedRegions$end[j]
          j <- j +1
        }
      } else if(window.size == "dynamic"){
        j <- i + 1
        windowEnd <- NA
        while (j <= nrow(precombinedRegions) && precombinedRegions$length[j] < min.size) {
            windowEnd <- precombinedRegions$end[j]
            j <- j +1
        }
        #### add in logic so if the current window is less than the threshold, 
        #### and the regions on either side are the same parent, call it that parent
      } else{
        print("Unrecognized value for windowSize. Please use either 'dynamic' or a number")
        break
      }
      if(!is.na(windowEnd)){
        if(current_end+windowEnd < min.size){
            parentBefore <- precombinedRegions$sample1[i-1]
            parentAfter <- precombinedRegions$sample1[j+1]
            if(parentBefore==parentAfter){
              current_label <- parentAfter
            } else {
              print(paste0("Unable to combine window at ",current_start," for ",current_label,". Parent region before did not match parent region after"))
            }
        } else{
          windowTable <- precombinedRegions %>% filter(between(end,current_end,windowEnd)) 
          windowTableSummary <- windowTable %>%
            group_by(sample1) %>%
            summarise(sumLength = sum(length))
          mostLikelyParent <- windowTableSummary[which(windowTableSummary$sumLength == max(windowTableSummary$sumLength)),"sample1"][1]
          current_end <- max(windowTable$end)
          current_label <- mostLikelyParent
        }
      } else {
        if(i==1) {
          ### first region
          if(precombinedRegions$length[i+1]>min.size){
            current_label <- precombinedRegions$sample1[i+1]
          } else {
            print(paste0("Unable to combine window at ",current_start," for ",current_label,". The second parent region was not larger than minThreshold"))
          }
        } else if (i==nrow(precombinedRegions)) {
          ### last region
          if(precombinedRegions$length[i-1]>min.size){
            current_label <- precombinedRegions$sample1[i-1]
          } else{
            print(paste0("Unable to combine window at ",current_start," for ",current_label,". The second to last parent region was not larger than minThreshold"))
          }
        } else {
          parentBefore <- precombinedRegions$sample1[i-1]
          parentAfter <- precombinedRegions$sample1[i+1]
          if(parentBefore==parentAfter){
            current_label <- parentAfter
          } else {
            print(paste0("Unable to combine window at ",current_start," for ",current_label,". Parent region before did not match parent region after"))
          }
        }
      }
      
    }

    # print(paste0("updated end for ",current_label," is ",current_end," instead of ",first_end))
    combined <- rbind(combined, data.frame(start = current_start, end = current_end, 
                                           sample1 = current_label,chrom=currentChrom,length=current_end-current_start))
    if(current_end != first_end){
      #i <- j - 1
      i <- which(precombinedRegions$start == current_end)
    } else {
      i <- i + 1
    }
    
  }
  combinedUniqueEnds <- combined %>% distinct(end,sample1,.keep_all = T) %>%
    mutate(new_group = (sample1 != lag(sample1, default = first(sample1))) | (chrom != lag(chrom, default = first(chrom)))) %>%
    mutate(group = cumsum(new_group)) %>%
    group_by(group, sample1,chrom) %>%
    summarize(start = round(min(start),4), end = round(max(end),4), .groups = 'drop') %>%
    mutate(length=end-start) 
  return(combinedUniqueEnds)
}

combineRegionsInCMWindow_assumeLeft <- function(precombinedRegions, window.size) {
  combined <- data.frame(start = numeric(0), end = numeric(0), sample1 = character(0), stringsAsFactors = FALSE)
  i <- 1
  #while (i <= 219){
  while (i <= nrow(precombinedRegions)){
    current_start <- precombinedRegions$start[i]
    first_end <- precombinedRegions$end[i]
    current_end <- first_end
    current_label <- precombinedRegions$sample1[i]
    currentChrom <- precombinedRegions$chrom[i]
    # print(paste0("first start: ",current_start))
    # print(paste0("first end: ",first_end))
    # print(current_label)
    
    j <- i + 1
    while (j <= nrow(precombinedRegions) && precombinedRegions$start[j] <= first_end + window.size) {
      # print(paste0("next label: ",precombinedRegions$sample1[j]))
      # print(paste0("next start: ",precombinedRegions$start[j]))
      if(precombinedRegions$sample1[j] == current_label){
        # print("found matching label")
        current_end <- max(current_end,precombinedRegions$end[j])
      }
      j <- j + 1
    }
    # print(paste0("updated end for ",current_label," is ",current_end," instead of ",first_end))
    combined <- rbind(combined, data.frame(start = current_start, end = current_end, 
                                           sample1 = current_label,chrom=currentChrom,length=current_end-current_start))
    if(current_end != first_end){
      #i <- j - 1
      i <- which(precombinedRegions$start == current_end)
    } else {
      i <- i + 1
    }
    
  }
  combinedUniqueEnds <- combined %>% distinct(end,sample1,.keep_all = T)
  return(combined)
}
