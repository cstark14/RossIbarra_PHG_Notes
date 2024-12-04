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


singleFileOrFolder <- "folder" ### folder or file
minThreshold <- 2 ### in cM, size of parent region which will flag to look for combination
### in cM, looks this number away from each feature and 
### combines of it finds the same feature within that window
#windowSize <- "dynamic" ### can be a number or "dynamic" or NA
windowSize <- NA
selectedChrom <- "chr1"

##### below 3 lines only necessary for single files
trialNameForPlot <- "SRR391113_Acc0.90_recomb0.000001_maxAncestor2"
#imputeParentsFile <- "~//Downloads/parents_NAM_RILs_subset/SRR391118_imputed_parents.txt"
#imputeParentsFile <- "C:/Users/Cristian/Documents/GitHub/RossIbarra_PHG_Notes/testMicahPHGwithNAMrils/subset/SRR391113_imputed_parents.txt"

#### below lines for folder
filenameExtraInfo <- "_Acc0.90_recomb0.000001_maxAncestor2" ### add any additional naming to the output file names..... assumes your filename is <identifier>_imputed_parents.txt
#folderPath <- "C:/Users/Cristian/Documents/GitHub/RossIbarra_PHG_Notes/testMicahPHGwithNAMrils/subset/"
folderPath <- "~/Documents/GitHub/RossIbarra_PHG_Notes/testMicahPHGwithNAMrils/subset_Acc0.90_recomb0.000001_maxAncestor2/"


#source("C://Users/Cristian/Documents/GitHub/RossIbarra_PHG_Notes/visualizations/visualizeImputedParentsWithcM_Functions.R")
source("~/Documents/GitHub/RossIbarra_PHG_Notes/visualizations/visualizeImputedParentsWithcM_Functions.R")
genMapFile <- "~/Documents/GitHub/RossIbarra_PHG_Notes/visualizations//ogut_v5_from_paulo.map.txt"
#genMapFile <- "C://Users/Cristian/Documents/GitHub/RossIbarra_PHG_Notes/visualizations/ogut_v5_from_paulo.map.txt"


#################### don't edit below this line ##########################

### if it comes back to same lineage within x centimorgans, they'll be the same lineage
#### whenever a change, look those x centimorgans ahead
##### PHG different recombination probability between reference ranges as you move across genome, not just average
######## Hacky: New PHG, insert fake n number of fake reference ranges between known to account for recomb freq differences between ranges
#### graphing sizes of ref ranges shows that 3.5 orders of magnitude differences in length 
###### focus on intergenic (most likely longer, so recomb), subset to those intergenic ones for the recreation of hist
#### reach to to Jeff after with results so he can request that feature in the slack channel

if(singleFileOrFolder=="file"){
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
  
  ggplotParentColorsTable <- data.frame(parent=unique(imputeParents$sample1),color=NA) %>% 
    # mutate(color=colorRampPalette((brewer.pal(12, "Paired")))(nrow(ggplotParentColors)))
    mutate(color=colorRampPalette(colorBlindness::paletteMartin)(nrow(.)))
  ggPlotParentVector <- setNames(as.character(ggplotParentColorsTable$color),as.character(ggplotParentColorsTable$parent))
  
  #### shifting negative gen pos to 0, adding to the rest of the cm for that chr
  genMap0Start <- shiftGenMapTo0(genMap)
  
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
    #summarize(start = min(startGen), end = max(endGen), .groups = 'drop') %>%
    summarize(start = round(min(startGen),4), end = round(max(endGen),4), .groups = 'drop') %>%
    mutate(length=end-start) 
  
  combinedRegionParentsChr <- combinedRegionParents %>% filter(chrom==selectedChrom)
  
  if(is.na(windowSize)){
    parentsNotInData <- ggplotParentColorsTable %>% filter(parent %notin% combinedRegionParentsChr$sample1) %>% dplyr::rename(.,sample1=parent) %>% select(- color)
    dataToPlotChr <- plyr::rbind.fill(combinedRegionParentsChr,parentsNotInData)
    
    perChr1PlotSegments <- ggplot(dataToPlotChr) + 
      geom_segment(aes(x=start,xend=end,y=sample1,yend=sample1,color=sample1),linewidth=5) +
      geom_segment(aes(x=start,xend=end,y=0,yend=0,color=sample1),linewidth=5) +
      scale_color_manual(values=ggPlotParentVector) + 
      scale_y_discrete(drop=FALSE) +
      ggtitle(paste0("Imputed Parent per Genetic Segments of ",selectedChrom," for ",trialNameForPlot)) +
      xlab("cM")+
      ylab("NAM Parent")
    perChr1PlotSegments
  } else{
    parentsCombinedWindows <- combineRegionsInCMWindow_mode(precombinedRegions=combinedRegionParentsChr,
                                                            window.size = windowSize,
                                                            min.size=minThreshold)
    if(windowSize == "dynamic"){
      title <- paste0("Imputed Parent per cM (merged ranges smaller than ",
                      minThreshold," cM, using dynamic windows) of ",selectedChrom," for ",trialNameForPlot)
    } else {
      title <- paste0("Imputed Parent per cM (merged ranges smaller than ",
                      minThreshold," cM using ",windowSize," cM windows) of ",selectedChrom," for ",trialNameForPlot)
    }
    
    parentsNotInData <- ggplotParentColorsTable %>% filter(parent %notin% parentsCombinedWindows$sample1) %>% dplyr::rename(.,sample1=parent) %>% select(- color)
    dataToPlotChr <- plyr::rbind.fill(parentsCombinedWindows,parentsNotInData)
    
    perChr1PlotSegments <- ggplot(dataToPlotChr) + 
      geom_segment(aes(x=start,xend=end,y=sample1,yend=sample1,color=sample1),linewidth=5) +
      geom_segment(aes(x=start,xend=end,y=0,yend=0,color=sample1),linewidth=5) +
      scale_color_manual(values=ggPlotParentVector) + 
      scale_y_discrete(drop=FALSE) +
      ggtitle(title) +
      xlab("cM")+
      ylab("NAM Parent")
    perChr1PlotSegments
  }
  perChr1PlotSegments
} else {
  #listOfFiles <- dir(path=folderPath,full.names = TRUE)
  listOfFiles <- grep(list.files(path=folderPath,full.names = TRUE),pattern="*.txt",value=TRUE)
  for(i in 1:length(listOfFiles)){
    imputeParentsFile <- listOfFiles[i]
    trialNameForPlot <- gsub("_imputed_parents.txt","",basename(imputeParentsFile))
    dir_name <- dirname(imputeParentsFile)
    
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
    
    ggplotParentColorsTable <- data.frame(parent=unique(imputeParents$sample1),color=NA) %>% 
      # mutate(color=colorRampPalette((brewer.pal(12, "Paired")))(nrow(ggplotParentColors)))
      mutate(color=colorRampPalette(colorBlindness::paletteMartin)(nrow(.)))
    ggPlotParentVector <- setNames(as.character(ggplotParentColorsTable$color),as.character(ggplotParentColorsTable$parent))
    
    #### shifting negative gen pos to 0, adding to the rest of the cm for that chr
    genMap0Start <- shiftGenMapTo0(genMap)
    
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
      #summarize(start = min(startGen), end = max(endGen), .groups = 'drop') %>%
      summarize(start = round(min(startGen),4), end = round(max(endGen),4), .groups = 'drop') %>%
      mutate(length=end-start) 
    
    combinedRegionParentsChr <- combinedRegionParents %>% filter(chrom==selectedChrom)
    
    if(is.na(windowSize)){
      outputFileName <- paste0(trialNameForPlot,filenameExtraInfo,"_",selectedChrom,"_plot.pdf")
      parentsNotInData <- ggplotParentColorsTable %>% filter(parent %notin% combinedRegionParentsChr$sample1) %>% dplyr::rename(.,sample1=parent) %>% select(- color)
      dataToPlotChr <- plyr::rbind.fill(combinedRegionParentsChr,parentsNotInData)
      
      perChr1PlotSegments <- ggplot(dataToPlotChr) + 
        geom_segment(aes(x=start,xend=end,y=sample1,yend=sample1,color=sample1),linewidth=5) +
        geom_segment(aes(x=start,xend=end,y=0,yend=0,color=sample1),linewidth=5) +
        scale_color_manual(values=ggPlotParentVector) + 
        scale_y_discrete(drop=FALSE) +
        ggtitle(paste0("Imputed Parent per Genetic Segments of ",selectedChrom," for ",trialNameForPlot)) +
        xlab("cM")+
        ylab("NAM Parent")
    } else{
      
      parentsCombinedWindows <- combineRegionsInCMWindow_mode(precombinedRegions=combinedRegionParentsChr,
                                                              window.size = windowSize,
                                                              min.size=minThreshold)
      if(windowSize == "dynamic"){
        outputFileName <- paste0(trialNameForPlot,filenameExtraInfo,"_dynamicConcat_",selectedChrom,"_plot.pdf")
        
        title <- paste0("Imputed Parent per cM (merged ranges smaller than ",
                        minThreshold," cM, using dynamic windows) of ",selectedChrom," for ",trialNameForPlot)
      } else {
        title <- paste0("Imputed Parent per cM (merged ranges smaller than ",
                        minThreshold," cM using ",windowSize," cM windows) of ",selectedChrom," for ",trialNameForPlot)
      }
      
      parentsNotInData <- ggplotParentColorsTable %>% filter(parent %notin% parentsCombinedWindows$sample1) %>% dplyr::rename(.,sample1=parent) %>% select(- color)
      dataToPlotChr <- plyr::rbind.fill(parentsCombinedWindows,parentsNotInData)
      
      perChr1PlotSegments <- ggplot(dataToPlotChr) + 
        geom_segment(aes(x=start,xend=end,y=sample1,yend=sample1,color=sample1),linewidth=5) +
        geom_segment(aes(x=start,xend=end,y=0,yend=0,color=sample1),linewidth=5) +
        scale_color_manual(values=ggPlotParentVector) + 
        scale_y_discrete(drop=FALSE) +
        ggtitle(title) +
        xlab("cM")+
        ylab("NAM Parent")
    }
    ggsave(filename=outputFileName,plot=perChr1PlotSegments,device="pdf",path = dir_name,width=11,height = 8.5,units="in")
  }
}
