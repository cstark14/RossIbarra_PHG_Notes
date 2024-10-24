library(tidyverse)
library(dplyr)

fromChatGPT <- function(){
  data <- data.frame(
    location = c(1, 2, 3, 5, 6, 7, 10),
    label = c("A", "A", "B", "B", "B", "A", "A")
  )
  
  combined_data <- data %>%
    mutate(group = cumsum(c(1, diff(location) != 1 | lag(label) != label))) %>%
    group_by(group, label) %>%
    summarize(start = min(location), end = max(location)) %>%
    ungroup() %>%
    select(-group)
  
  combined_data <- data %>%
    mutate(group = cumsum(c(TRUE, diff(location) != 1 | lag(label, default = first(label)) != label))) %>%
    group_by(group, label) %>%
    summarize(start = min(location), end = max(location), .groups = 'drop') %>%
    select(-group)
  
  combined_data <- data %>%
    mutate(new_group = (location != lag(location, default = first(location)) + 1) | (label != lag(label, default = first(label)))) %>%
    mutate(group = cumsum(new_group)) %>%
    group_by(group, label) %>%
    summarize(start = min(location), end = max(location), .groups = 'drop') %>%
    select(-group)##, -new_group)
}

currentSampleMappingImputedParents <- read_delim("ZeaSynDH_Trial1100_0.9accuracy_imputed_parents.txt",
                                                 delim = "\t", escape_double = FALSE, col_types = cols(sample2 = col_skip()), trim_ws = TRUE)
combinedRegionParents <- currentSampleMappingImputedParents %>%
  filter(!startsWith(chrom,"scaf")) %>%
  mutate(new_group = (sample1 != lag(sample1, default = first(sample1))) | (chrom != lag(chrom, default = first(chrom)))) %>%
  mutate(group = cumsum(new_group)) %>%
  group_by(group, sample1,chrom) %>%
  summarize(start = min(start), end = max(end), .groups = 'drop') %>%
  select(-group)

#perChrPlot <- ggplot(combinedRegionParents,aes(x=start,y=sample1,color=sample1)) + geom_point() + facet_wrap(~chrom)
#perChrPlot

#perChrPlotSegments <- ggplot(combinedRegionParents) + geom_segment(aes(x=start,xend=end,y=sample1,yend=sample1,colour=sample1)) + facet_wrap(~chrom)
#perChrPlotSegments

combinedRegionParentsChr1 <- combinedRegionParents %>% filter(chrom=="chr1")
perChr1PlotSegments <- ggplot(combinedRegionParentsChr1) + geom_segment(aes(x=start,xend=end,y=sample1,yend=sample1,color=sample1),linewidth=5)
perChr1PlotSegments
