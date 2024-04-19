############ Heatmap   #############

library(readr)
library(ggplot2)
library(tidyverse)
library(pheatmap)

###load file
#load catch file
catch <- read_csv("landings_data.csv")
View(catch)

#remove the column containing nr and total catches
catch <- catch[,c(-1,-3)]
View(catch)
               
### transform the data from long to wide
data <- spread(catch, key = "Species", value = "catches")
               

###asign first column as rowname
data <- data %>%
  column_to_rownames("year")


#### remove outliers##
# save the object as dublicate in order not to modify the original df
dublicate <- data

## Save the row names
rownames <- rownames(dublicate)  

# Convert matrix to data frame
df <- as.data.frame(dublicate)  

# Replace the values for the year 1982 for "Merluccius merluccius" and "Mustelus spp" with NA
df["1982", "Merluccius merluccius"] <- NA
df["1982", "Mustelus spp"] <- NA



# Function to replace NA with mean of values before and after
replace_na_with_mean <- function(x) {
  na_indices <- which(is.na(x))
  for (i in na_indices) {
    values <- x[(i-1):(i+1)]
    x[i] <- mean(values[!is.na(values)], na.rm = TRUE)
  }
  return(x)
}

# Apply the function to the data frame
df <- lapply(df, replace_na_with_mean)

# Convert list back to data frame
df <- as.data.frame(df)

# Replace dots with spaces in column names
colnames(df) <- gsub("\\.", " ", colnames(df))
# Rename the column
colnames(df)[colnames(df) == "Loliginidae  Ommastrephidae"] <- "Loliginidae - Ommastrephidae"


# Convert data frame back to matrix
dublicate <- as.matrix(df)

# Reassign the row names
rownames(dublicate) <- rownames  


# Scale the data:
scaled_data <- scale(dublicate)


# Transpose the matrix because we want the species as rows
transposed_data <- t(scaled_data)


# Create the heatmap with the clusters

clustered_heatmap <- pheatmap(
  transposed_data,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "ward.D2",
  cluster_cols = FALSE,
  show_rownames = TRUE,
  show_colnames = TRUE,
  treeheight_row = 0,
  treeheight_col = 0,
  cutree_rows = 2,
  gaps_col = 22,
  fontsize_row = 7,  # Adjust the font size of row names
  fontsize_col = 7,   # Adjust the font size of column names
  legend_breaks = c(-1.5, 1, 3.7),
  legend_labels = c("Low", "Medium", "High")
)
