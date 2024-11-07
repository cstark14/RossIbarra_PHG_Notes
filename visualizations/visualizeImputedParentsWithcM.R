library(tidyverse)
library(dplyr)
library(fANCOVA)
library(readr)
library(reshape2)
library(ggplot2)
library(scales)
options(scipen=999)
options(stringsAsFactors=FALSE)

genMapFile <- "ogut_v5_from_paulo.map.txt"
trialNameForPlot <- "ZeaSynDH_Trial1100_mapAccuracy0.9"
imputeParentsFile <- "~/Documents/GitHub/RossIbarra_PHG_Notes/testMicahPHGwithsynDH/0.9mapAccuracy/ZeaSynDH_Trial1100_0.9accuracy_imputed_parents.txt"
minThreshold <- 0.5 ### in cM, for filtering out regions smaller than this threshold
windowSize <- 0.5 ### in cM, looks this number away from each feature and combines of it finds the same feature within that window
selectedChrom <- "chr1"
#minThreshold <- NA


### if it comes back to same lineage within x centimorgans, they'll be the same lineage
#### whenever a change, look those x centimorgans ahead
##### PHG different recombination probability between reference ranges as you move across genome, not just average
######## Hacky: New PHG, insert fake n number of fake reference ranges between known to account for recomb freq differences between ranges
#### graphing sizes of ref ranges shows that 3.5 orders of magnitude differences in length 
###### focus on intergenic (most likely longer, so recomb), subset to those intergenic ones for the recreation of hist
#### reach to to Jeff after with results so he can request that feature in the slack channel

genMap <- read_delim(genMapFile, 
                     delim = "\t", escape_double = FALSE, 
                     col_types = cols(chr = col_integer(), 
                                      pos = col_integer(), cm = col_number()), 
                     trim_ws = TRUE) %>% mutate(cm=round(cm,digits=3)) 
imputeParents <- read_delim(imputeParentsFile, 
                            delim = "\t", escape_double = FALSE, 
                            col_types = cols(start = col_integer(), 
                                             end = col_integer()), trim_ws = TRUE) %>%
  mutate(chr=gsub("chr","",chrom)) %>%
  filter(!startsWith(chrom,"scaf")) %>%
  mutate(chrStart = paste0(chr,"_",start)) %>%
  mutate(chrEnd = paste0(chr,"_",end))
meltedImputeParents <- imputeParents %>%
  select(-c(sample1,sample2,chrStart,chrEnd)) %>%
  melt(.)

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

genMap0Start <- shiftGenMapTo0(genMap)

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


spanTable <- data.frame(Chromosome = unique(genMap$chr),fANCOVASpan=NA)
loessSpanTable <- getFancovaSpans(span.table=spanTable,frameMap=genMap0Start,min.span=0.05)


regressedGenMap <- runLoess(span.table=loessSpanTable,
                            frameMap=genMap0Start,
                            physPosToConvert=meltedImputeParents,
                            roundCM=4,
                            chrPrefix="chr",
                            shiftTo0=TRUE) %>%
  mutate(AssumedLG=as.factor(as.character(AssumedLG))) %>%
  mutate(chrPos=paste0(chr,"_",value))

imputeParentsWithGen <- merge(imputeParents,regressedGenMap[,c("chrPos","InterpolatedGenPos")],by.x="chrStart",by.y="chrPos")
imputeParentsWithGen <- merge(imputeParentsWithGen,regressedGenMap[,c("chrPos","InterpolatedGenPos")],by.x="chrEnd",by.y="chrPos") %>%
  rename(.,startGen="InterpolatedGenPos.x") %>%
  rename(.,endGen="InterpolatedGenPos.y") %>%
  arrange(chrom,startGen) %>%
  select(chrom,startGen,endGen,sample1) %>%
  mutate(length=endGen-startGen) 

combinedRegionParents <- imputeParentsWithGen %>%
  mutate(new_group = (sample1 != lag(sample1, default = first(sample1))) | (chrom != lag(chrom, default = first(chrom)))) %>%
  mutate(group = cumsum(new_group)) %>%
  group_by(group, sample1,chrom) %>%
  summarize(start = min(startGen), end = max(endGen), .groups = 'drop') %>%
  mutate(length=end-start) 

combinedRegionParentsChr <- combinedRegionParents %>% filter(chrom==selectedChrom)
#### used chatGPT to see if there was a simpler (non-write-your-own-function) way to do the below:
combineRegionsInCMWindow <- function(precombinedRegions, window.size) {
  combined <- data.frame(start = numeric(0), end = numeric(0), sample1 = character(0), stringsAsFactors = FALSE)
  i <- 1
  while (i <= nrow(precombinedRegions)){
    current_start <- precombinedRegions$start[i]
    first_end <- precombinedRegions$end[i]
    current_end <- first_end
    current_label <- precombinedRegions$sample1[i]
    currentChrom <- precombinedRegions$chrom[i]
    
    j <- i + 1
    while (j <= nrow(precombinedRegions) && precombinedRegions$start[j] <= current_end + window.size) {
      if(precombinedRegions$sample1[j] == current_label){
        current_end <- max(current_end,precombinedRegions$end[j])
      }
      j <- j + 1
    }
    
    combined <- rbind(combined, data.frame(start = current_start, end = current_end, 
                                           sample1 = current_label,chrom=currentChrom,length=current_end-current_start))
    i <- j
  }
  return(combined)
}

parentsCombinedWindows <- combineRegionsInCMWindow(precombinedRegions=combinedRegionParentsChr,window.size = windowSize)

if(is.na(windowSize)){
  dataToPlot <- combinedRegionParentsChr
  dataToPlotChr <- dataToPlot %>% filter(chrom=="chr1")
  perChr1PlotSegments <- ggplot(dataToPlotChr) + 
    geom_segment(aes(x=start,xend=end,y=sample1,yend=sample1,color=sample1),linewidth=5) +
    #geom_segment(aes(x=start,xend=end,y=1,yend=1,color=sample1),linewidth=5) +
    ggtitle(paste0("Imputed Parent per Genetic Segments of Chr 1 for ",trialNameForPlot)) +
    xlab("cM")+
    ylab("NAM Parent")
  perChr1PlotSegments
} else{
  dataToPlotChr <- parentsCombinedWindows 
  perChr1PlotSegmentsFiltered <- ggplot(dataToPlotChr) + 
    geom_segment(aes(x=start,xend=end,y=sample1,yend=sample1,color=sample1),linewidth=5) +
    #geom_segment(aes(x=start,xend=end,y=1,yend=1,color=sample1),linewidth=5) +
    ggtitle(paste0("Imputed Parent per Genetic Segments Greater than ",minThreshold," of Chr 1 for ",trialNameForPlot)) +
    xlab("cM")+
    ylab("NAM Parent")
  perChr1PlotSegmentsFiltered
}
  


