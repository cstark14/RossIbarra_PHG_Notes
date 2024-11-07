# Install and load the necessary package
library(dplyr)

firstShot <- function(){
  # Create a sample data frame
  data <- data.frame(
    start = c(1,2, 5, 10, 15, 20, 25),
    end = c(4,3, 9, 14, 19, 24, 29),
    label = c("A", "B","A", "A", "B", "A", "A")
  )
  
  # Define the function to combine features
  combine_features <- function(data, x) {
    combined <- data.frame(start = numeric(0), end = numeric(0), label = character(0), stringsAsFactors = FALSE)
    i <- 1
    while (i <= nrow(data)) {
      current_start <- data$start[i]
      current_end <- data$end[i]
      current_label <- data$label[i]
      
      j <- i + 1
      while (j <= nrow(data) && data$start[j] <= current_end + x && data$label[j] == current_label) {
        current_end <- data$end[j]
        j <- j + 1
      }
      
      combined <- rbind(combined, data.frame(start = current_start, end = current_end, label = current_label))
      i <- j
    }
    return(combined)
  }
  
  # Use the function with your data and specified distance x
  x <- 5  # Example distance in centimorgans
  combined_data <- combine_features(data, x)
  print(combined_data)
}

# Install and load the necessary package

# Create the sample data frame
data <- data.frame(
  start = c(1, 2, 5, 10, 15, 20, 25),
  end = c(4, 3, 9, 14, 19, 24, 29),
  label = c("A", "B", "A", "A", "B", "A", "A")
)

# Define the function to combine features within a specified window
combine_features_within_window <- function(data, x) {
  combined <- data.frame(start = numeric(0), end = numeric(0), label = character(0), stringsAsFactors = FALSE)
  i <- 1
  while (i <= nrow(data)) {
    current_start <- data$start[i]
    current_end <- data$end[i]
    current_label <- data$label[i]
    
    j <- i + 1
    while (j <= nrow(data) && data$start[j] <= current_start + x) {
      if (data$label[j] == current_label) {
        current_end <- max(current_end, data$end[j])
      }
      j <- j + 1
    }
    
    combined <- rbind(combined, data.frame(start = current_start, end = current_end, label = current_label))
    i <- j
  }
  
  # Combine overlapping ranges with the same label
  combined <- combined %>%
    group_by(label) %>%
    summarize(start = min(start), end = max(end), .groups = 'drop')
  
  return(combined)
}

# Use the function with your data and specified distance x
x <- 5  # Example distance in centimorgans
combined_data <- combine_features_within_window(data, x)
print(combined_data)
