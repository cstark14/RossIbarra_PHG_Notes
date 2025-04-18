#### code to convert phys to gen positions using a gen map as a ref/frame
#### Tips from Beibei/Paulo
## Here are two tips I got from Paulo, which is very helpful for me: 
#Some of the cM are negative so you have to add a scalar for every position for each chromosome. 
#For example, for chr1 you would have to add 4.8 cM for every position and for chr2 you would add 3.6 cM
## I found an error with the map at the end of chr2 where the position decreases while mapping units increase. 
# Jeff and I aren't super sure what's happening there but it will cause recombination rates to be negative 
# (which doesn't make sense). Every other chromosome is fine though

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
imputeParentsFile <- "~/Documents/GitHub/RossIbarra_PHG_Notes/testMicahPHGwithsynDH/0.9mapAccuracy/ZeaSynDH_Trial10_0.9accuracy_imputed_parents.txt"

genMap <- read_delim(genMapFile, 
                     delim = "\t", escape_double = FALSE, 
                     col_types = cols(chr = col_integer(), 
                                      pos = col_integer(), cm = col_number()), 
                     trim_ws = TRUE) %>% mutate(cm=round(cm,digits=3)) %>%
  filter("scaf" %notin% chr)
imputeParents <- read_delim(imputeParentsFile, 
                      delim = "\t", escape_double = FALSE, 
                      col_types = cols(start = col_integer(), 
                                       end = col_integer()), trim_ws = TRUE) %>%
  mutate(chr=gsub("chr","",chrom)) %>%
  filter(!startsWith(chrom,"scaf"))
meltedImputeParents <- imputeParents %>%
  select(-c(sample1,sample2)) %>%
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
  #physPosToConvert[grepl("Dummy0",physPosToConvert$MRN,perl=TRUE) & !is.na(physPosToConvert$InterpolatedGenPos),"InterpolatedGenPos"] <- 0.00
  return(physPosToConvert)
}
plotLoess <- function(physToGenMap,rounded=FALSE) {
  # post loess plot ####
  if(rounded){
    loess.plot <- ggplot(data = physToGenMap, aes(x = value, y = InterpolatedGenPosRounded, color = AssumedLG)) + 
      geom_point(size = 0.5) + 
      scale_x_continuous(labels = label_number(suffix = "Mbp", scale = 1e-6), 
                         expand = c(0,0), 
                         limits = c(0, NA)) +  
      facet_wrap(~chr, scales = "free" ) + 
      theme_bw()
    return(loess.plot)
  } else{
    loess.plot <- ggplot(data = physToGenMap, aes(x = value, y = InterpolatedGenPos, color = AssumedLG)) + 
      geom_point(size = 0.5) + 
      scale_x_continuous(labels = label_number(suffix = "Mbp", scale = 1e-6), 
                         expand = c(0,0), 
                         limits = c(0, NA)) +  
      facet_wrap(~chr, scales = "free" ) + 
      theme_bw()
    return(loess.plot)
  }
}

spanTable <- data.frame(Chromosome = unique(genMap$chr),fANCOVASpan=NA)
loessSpanTable <- getFancovaSpans(span.table=spanTable,frameMap=genMap0Start,min.span=0.05)


regressedGenMap <- runLoess(span.table=loessSpanTable,
                            frameMap=genMap0Start,
                            physPosToConvert=meltedImputeParents,
                            roundCM=4,
                            chrPrefix="chr",
                            shiftTo0=TRUE) %>%
  mutate(AssumedLG=as.factor(as.character(AssumedLG)))

loessPlot <- plotLoess(physToGenMap=regressedGenMap)
loessPlot

#### Jeff recommended approx.fun ####
# ogut<-read.table("~/Desktop/ogut_fifthcM_map_agpv4_INCLUDE.txt",header=F)
# colnames(ogut)=c("marker","marker_num","cm","chr","pos")
# 
get_cM <- function(gen.map,cr,p){
  temp <- filter(gen.map,chr==cr)
  return(approx(temp$pos,temp$cm,p)$y)
}
# 
phys2GenWithApprox <- meltedImputeParents %>% mutate(cm=NA)

for(i in 1:nrow(phys2GenWithApprox)){
  currentChr <- phys2GenWithApprox[i,"chr"] 
  currentPhysPos <- phys2GenWithApprox[i,"value"]
  approxGen <- get_cM(gen.map=genMap0Start,cr=currentChr,p=currentPhysPos)
  phys2GenWithApprox[i,"cm"] <- ifelse(is.na(approxGen),0,approxGen)
}

