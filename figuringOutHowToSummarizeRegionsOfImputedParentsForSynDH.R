library(tidyverse)
library(dplyr)

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
  select(-group, -new_group)

ZeaSynDH_Trial10_250458543_H5NWTBGXX_1_TTCGGAC_readMapping_txt_imputed_parents <- read_delim("testMicahPHGwithsynDH/examplesFromImputeParents/ZeaSynDH_Trial10:250458543_H5NWTBGXX_1_TTCGGAC_readMapping.txt_imputed_parents.txt", 
                                                                                             +     delim = "\t", escape_double = FALSE, 
                                                                                             +     col_types = cols(sample2 = col_skip()), 
                                                                                             +     trim_ws = TRUE)