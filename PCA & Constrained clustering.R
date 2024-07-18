# PCA and Hiererchical constrained clustering

# load packages
library(vegan)
library(rioja)
library(tidyverse)
library(ade4)
library(readr)
library(factoextra)
library(ggplot2)
library(gridExtra)

# load file
catch <- read.csv("landings_data.csv")

#remove the column containing nr and total catches
catch <- catch[,c(-1,-3)]

# format the dataframe from long to wide
Data.transf <- spread(catch, key = "Species", value = "catches")

# Create a separate time vector for labels
Time <- Data.transf$year

# asign first column as rowname
new_df <- Data.transf %>%       
  column_to_rownames("year")      


# Standardize the data the data :
Data.stand <- decostand(new_df, method="standardize", MARGIN = 2, na.rm = T)


# Save the dimensions (number of rows[1] and columns[2] ) in a vector
n <- dim(new_df)

# Calculate the Euclidean distance matrix from the standardised data 
Data.dist <- dist(Data.stand, "euclidean",  diag = TRUE, upper = TRUE)


### Perform constrained hierarchical Clustering: 

# Coniss = constrained incremental sum of squares clustering
CC <- chclust(Data.dist, method="coniss")

# plot outcome: Show both plots next to each other
par(mfrow = c(1, 2))
# cluster-plot with labels
plot(CC,labels= Time, main = "Coniss cluster plot", cex=0.8)
# broken stick plot
bstick(CC) 
title("Coniss -broken stick plot")


# plot Coniss cluster
par(mfrow = c(1, 1))
# cluster-plot with labels
plot(CC,labels= Time, main = "Cluster plot", cex=0.8)



#PCA starts
pca_community <- dudi.pca(new_df, scannf=F, nf=3, scale = TRUE)
summary(pca_community)

###to have an idea how the variables are linked with PCs
var <- get_pca_var(pca_community)
var$coord###correlation between variable and PCS
var$cos2## how well the PCs explain the variable 
var$contrib##contirbution of a variable to the PCs


### increase max.overlaps
options(ggrepel.max.overlaps = Inf)

##plot the contribution of the variable to PCs '####### SAVE THEM IN PPT
dim1 <- fviz_contrib(pca_community, choice = "var", axes = 1, top = 15) 
dim1
dim2 <- fviz_contrib(pca_community, choice = "var", axes = 2, top = 15) 
dim2
dim3 <- fviz_contrib(pca_community, choice = "var", axes = 3, top = 10) 
dim3

###### basic biplot###
variables_pca <- fviz_pca_var(pca_community,
                  alpha.var = "contrib",#transparency of var
                  col.var = "contrib", # Color by contributions to the PC
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE 
)# Avoid text overlapping
variables_pca


# Plot the years ###
ind <- get_pca_ind(pca_community)
##plot individuals (years) 
fviz_pca_ind(pca_community)
##plot individuals and variables 
biplot <- fviz_pca_biplot(pca_community, repel = TRUE,
                     col.var = "contrib", # Color by contributions to the PC
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # Variables color
                     alpha.var = "contrib",
                     col.ind = "gray"# Individuals color
,
ggtheme = theme(
  axis.text = element_text(colour = "black", size = 10), # Adjust axis text size
  axis.title = element_text(colour = "black", size = 10),
  plot.background = element_rect(fill = "white", colour = "white"),
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
)) +
  labs(x = "PC1 (37%)", y = "PC2 (14.1%)") +
  ggtitle(NULL) +
  theme(legend.position = "none")

biplot


# create clusters of years 

# Define the number of clusters
nclust <- 2

# Create the clusters
clusters <- as.factor(cutree(CC, k=nclust))


##plot individuals based on clusters 
individuals <- fviz_pca_ind(pca_community,
                  #geom.ind = "point", # show points only (but not "text")
                  col.ind = clusters, # color by groups
                  palette = c("#1b7837", "#313695", "#d73027", "orange", "black"),
                  addEllipses = TRUE, # Concentration ellipses
                  legend.title = "Groups"
)
individuals

# plot variables and individuals

final_biplot <- fviz_pca_biplot(pca_community, repel = TRUE,
                                col.var = "black", # Color variables in gray
                                alpha.var = "contrib",
                                col.ind = clusters,
                                palette = c("#313695", "#d73027"),
                                labelsize = 3,  # Adjust text size for variables and individuals
                                pointsize = 1,
                                ggtheme = theme(
                                  axis.text = element_text(colour = "black", size = 12), # Adjust axis text size
                                  axis.title = element_text(colour = "black", size = 12),
                                  plot.background = element_rect(fill = "white", colour = "white"),
                                  panel.background = element_rect(fill = "white"),
                                  panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
                                )) + 
  labs(x = "PC1 (37%)", y = "PC2 (14.1%)") +  # Rename the axes
  theme(legend.position = "none") +  # Remove the legend 
  ggtitle(NULL)

final_biplot


# plot the PC trend 
pca_community$li
pca_pc <- as.data.frame(pca_community$li[,c(1,2)]) ##dataset to use to check Tipping
##add the year
pca_pc$year <- c(1970:2020)


pc1 <- ggplot() + 
  geom_line(data = pca_pc, aes(x = year, y = Axis1), color = "lightgray", linewidth = 2) + 
  geom_point(data = pca_pc, aes(x = year, y = Axis1, colour = factor(clusters)), size = 2) + 
  scale_color_manual(breaks = c("1", "2", "3"), values = c("#313695", "#d73027")) +
  xlab("Year") +
  ylab("PC1 Loadings") + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text = element_text(colour = "black", size=8),
    plot.title = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.title = element_text(colour = "black", size = 9, face = "bold"),
    legend.position = "none"
  )

pc1

pc2 <- ggplot() + 
  geom_line(data = pca_pc, aes(x = year, y = Axis2), color = "lightgray", linewidth = 2) + 
  geom_point(data = pca_pc, aes(x = year, y = Axis2, colour = factor(clusters)), size = 2) + 
  scale_color_manual(breaks = c("1", "2", "3"), values = c("#313695", "#d73027")) +
  xlab("Year") +
  ylab("PC2 Loadings") + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA),
    axis.text = element_text(colour = "black", size=8),
    plot.title = element_text(colour = "black", size = 10, hjust = 0.5),
    axis.title = element_text(colour = "black", size = 9, face = "bold"),
    legend.position = "none"
  )
pc2


grid.arrange(pc1, pc2)

