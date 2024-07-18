

library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)

#load catch file
catch <- read_csv("landings_data.csv")
View(catch)

#remove the column containing nr and total catches
catch <- catch[,c(-1,-3)]
View(catch)



## plot of ordered species based on the PC score
# Define the order of species
species_order <- c("Boops boops", "Atherinidae", "Spicara spp", "Rajiformes", "Trachurus spp", 
                   "Scophthalmidae", "Sardinella aurita", "Micromesistius poutassou",  "Scorpaenidae", 
                   "Loliginidae, Ommastrephidae", "Squalus spp", "Sepiidae, Sepiolidae", "Mugilidae", 
                   "Mustelus spp", "Sardina pilchardus", "Chamelea gallina", "Nephrops norvegicus", 
                   "Merluccius merluccius", "Octopodidea", "Scomber spp", 
                   "Lophius spp", "Parapenaeus longirostris", "Mullus spp", "Triglidae", "Solea solea", 
                   "Squilla mantis", "Engraulis encrasicolus")

# Change the order of levels
catch$Species <- factor(catch$Species, levels = species_order)

# Plot
ggplot(catch, aes(year, catches)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~Species, scales = "free_y") +
  labs(x = "", y = "Landings (t)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7))




## plots of all landings #

### to calculate the landings for all the species in one year
#convert the dataframe from long to wide
wide <- catch %>% 
  spread(year, catches)
wide


# Calculate total catches for each year
totals <- colSums(wide[, 2:ncol(wide)])

# Add a new row to the data frame
wide <- rbind(wide, c("TOTAL", totals))


# Create data frame with year and total catch
df_totals <- data.frame(year = names(totals), total = totals)


ggplot(df_totals, aes(x = year, y = total, group = 1)) +
  geom_line(color = "#313695", linewidth = 1.5) +
  labs(x = "Year", y = "Landings (t)") +
  scale_x_discrete(breaks = seq(1970, 2020, by = 10)) +
  theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    )

