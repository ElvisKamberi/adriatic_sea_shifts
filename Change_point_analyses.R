################## change point l#################################### 
setwd("C:/Users/Elvis/Desktop/UHH/PhD project/Change point")

library(readr)
library(ggplot2)
library(ade4)
library(factoextra)
library(gridExtra)
library(rioja)
library(vegan)
library(rshift)
#install packages
library(changepoint)
library(bcp)


library(standardize)
library(dplyr)
library(reshape2)
library(tidyr)
library(corrplot)
library(lattice)
library(tidyverse)



# Continue this analysis after the PCA and constrained clustering

##### Change point ####


pca_community$li
pca_pc <- as.data.frame(pca_community$li[,c(1,2,3)]) ##dataset to use to check changepoints

# convert rowname as column named "year
pca_pc <- pca_pc %>% 
  tibble::rownames_to_column('year')

class(pca_pc$year) # as character
#convert year to numeric
pca_pc$year <- as.numeric(pca_pc$year)



#detection of change point in the first pc ###

# bcp
m1 <- bcp(pca_pc$Axis1)
plot(m1)

#change point
c1 <- cpt.mean(pca_pc$Axis1, method = "PELT", test.stat = "Normal", penalty = "AIC", minseglen = 10)
c1
plot(c1, ylab = "PC1 loadings")
#The method identifies two change points in the position 19 (1988) and 29 (1998)



# Regime shift STARS ####

# PC 1
RSI_data <- Rodionov(pca_pc, "Axis1", "year", 10, prob = 0.05, startrow = 1, merge = TRUE)
RSI_graph(RSI_data, "Axis1", "year", "RSI")



# Convert the RSI to a data frame for the final plot
df <- as.data.frame(RSI_data)



# Plot together the results from bcp, changepoint and STARS
class(pca_pc$year)

# if pca_pc_year is not numeric
# pca_pc$year <- as.numeric(pca_pc$year)


# extract the mean values from the changepoint analyses by calculating the means manually
# Calculate mean of first segment
mean_first <- mean(head(pca_pc$Axis1, 19))
mean_first

# Calculate mean of middle segment between changepoints
mean_middle <- mean(pca_pc$Axis1[20:29])
mean_middle


# Calculate mean of last segment
mean_last <- mean(tail(pca_pc$Axis1, 22))
mean_last


# save as dataframe the data from the bcp
bcp_data <- data.frame(
  posterior_prob = head(m1$posterior.prob, 51),
  posterior_mean = head(m1$posterior.mean, 51)
)

# Create a list of years from 1970 to 2020
year <- 1970:2020

# Create a new column called 'year' and assign the list of years to it
bcp_data$year <- year

# Rename the columns in break_point_data
colnames(bcp_data) <- c("posterior_prob", "posterior_mean", "year")
bcp_data


### Merge bcp_data with the df to have all the data in one dataframe

joint_df <- merge(bcp_data, df, by = "year")

joint_df 



# Plot together
class(joint_df$year)
class(joint_df$RSI)

joint_df$year <- as.numeric(joint_df$year)


ylim.prim <- c(0, 3)   # primary axis (in this example, RSI)
ylim.sec <- c(-5, 5)    # sec axis (in this example, Axis1)


b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] # there was a bug here

# Transform the mean values back to the primary y-axis (RSI)
mean_first_rsi <- mean_first * b + a
mean_last_rsi <- mean_last * b + a
mean_middle_rsi <- mean_middle * b + a

joint_df <- joint_df %>%
  mutate(changepoint = case_when(
    year >= 1970 & year <= 1988 ~ mean_first_rsi,
    year >= 1989 & year <= 1998 ~ mean_middle_rsi,
    year >= 1999 & year <= 2020 ~ mean_last_rsi
  ))

joint_df # contains info for PC (1,2,3), RSI, bcp and changepoint (changepoint data are transformed to be suitable for ploting)


a <- ylim.prim[1] - b*ylim.sec[1]

#plot everything
# 'segment' contains the segment information (e.g., 1, 2, 3)
# We'll use mutate to create a new column for segments based on the years
joint_df <- joint_df %>%
  mutate(segment = case_when(
    year >= 1970 & year <= 1988 ~ 1,
    year >= 1989 & year <= 1998 ~ 2,
    year >= 1999 & year <= 2020 ~ 3,
    TRUE ~ NA_real_
  ))

# Plotting

com_st <- ggplot(joint_df, aes(x = year)) +
  geom_col(aes(y = RSI), fill = "steelblue", alpha = 0.7) +
  geom_line(aes(y = a + Axis1 * b), color = "black", linewidth = 2) +
  geom_line(aes(y = posterior_prob), color = "red3", linetype = "solid", linewidth = 1)  +
  geom_line(aes(y = changepoint, group = segment), linewidth = 1, color = "#313695") +
  geom_segment(aes(x = -Inf, xend = -Inf, y = 0, yend = 1), color = "red3", linewidth = 2) +
  geom_segment(aes(x = -Inf, xend = -Inf, y = 1, yend = ylim.prim[2]), color = "steelblue", linewidth = 2) +
  scale_y_continuous("Regime Shift Index", sec.axis = sec_axis(~ (. - a) / b, name = "PC1 Loadings"), expand = c(0, 0)) +
  scale_x_continuous("Year", breaks = seq(min(joint_df$year), max(joint_df$year), by = 5), expand = c(0, 0)) +
  theme_classic() +
  labs(title = "",y = "RSI / PC1 Loadings", x = "Year") +
  geom_vline(xintercept = c(1989, 1999, 2014, 2020.3), linetype = "dashed", color = "black") +
  theme(
    axis.line.y.left = element_line(color = "transparent", linewidth = 1),
    axis.text.y.left = element_text(size = 12, color = "steelblue"),
    axis.title.y.left = element_text(color = "steelblue"),
    axis.line.y.right = element_line(color = "black", linewidth = 1),
    axis.ticks.y.right = element_line(color = "black"),
    axis.text.y.right = element_text(color = "black", size = 12),
    axis.title.y.right = element_text(color = "black"),
    axis.text.x = element_text(angle = 0, size = 10, color = "black"),
    panel.grid.major.y = element_line()
  )  +
  geom_line(aes(y = a + Axis1 * b), color = ifelse(joint_df$year >= 1989 & joint_df$year < 1999 | (joint_df$year >= 2014 & joint_df$year <= 2020) , "black", "lightgrey"), linewidth = 1.3, alpha = 0.7) +
  geom_line(aes(y = posterior_prob), color = ifelse(joint_df$year >= 1989 & joint_df$year < 1999 | (joint_df$year >= 2014 & joint_df$year <= 2020), "red3", "lightgrey"), linetype = "solid", linewidth = 1.5, alpha = 0.7) +
  geom_line(aes(y = changepoint, group = segment), color = ifelse(joint_df$year >= 1989 & joint_df$year < 1999 | (joint_df$year >= 2014 & joint_df$year <= 2020), "#313695", "lightgrey"), linetype = "solid", linewidth = 1.5, alpha = ifelse(joint_df$year >= 1989 & joint_df$year < 1999 | (joint_df$year >= 2014 & joint_df$year <= 2020),1, 0.5))


com_st

# version with legend
com_st1 <- ggplot(joint_df, aes(x = year)) +
  geom_col(aes(y = RSI, fill = "RSI"), position = position_nudge(x = -0.2)) +
  geom_line(aes(y = a + Axis1 * b, color = "PC1 Loadings"), linewidth = 2, alpha = 1) +
  geom_line(aes(y = posterior_prob, color = "BCP"), size = 1, alpha = 0.7) +
  geom_line(aes(y = changepoint, group = segment, color = "Changepoint"), size = 1, alpha = 0.7) +
  geom_segment(aes(x = -Inf, xend = -Inf, y = 0, yend = 1), color = "red3", linewidth = 2) +
  geom_segment(aes(x = -Inf, xend = -Inf, y = 1, yend = ylim.prim[2]), color = "steelblue", linewidth = 2) +
  scale_y_continuous("RSI", sec.axis = sec_axis(~ (. - a) / b, name = "PC1 Loadings"), expand = c(0, 0)) +
  scale_x_continuous("Year", breaks = seq(min(joint_df$year), max(joint_df$year), by = 5), expand = c(0.005, 0.005)) +
  theme_classic() +
  labs(title = "Community Stability",y = "RSI / PC1 Loadings", x = "Year") +
  geom_vline(xintercept = c(1989, 1999, 2014, 2020), linetype = "dashed", color = "black", linewidth = 1) +
  theme(
    axis.line.y.left = element_line(color = "transparent", linewidth = 2),
    axis.text.y.left = element_text(size = 12),
    axis.line.y.right = element_line(color = "black", linewidth = 1.2),
    axis.ticks.y.right = element_line(color = "black"),
    axis.text.y.right = element_text(color = "black", size = 12),
    axis.title.y.right = element_text(color = "black"),
    axis.text.x = element_text(angle = 0, size = 10),
    panel.grid.major.y = element_line()
  ) +
  scale_fill_manual(name = "", values = c("RSI" = "steelblue")) +
  scale_color_manual(name = "", values = c("PC1 Loadings" = "black", "BCP" = "red3", "Changepoint" = "#313695")) +
  geom_line(aes(y = a + Axis1 * b), color = ifelse(joint_df$year >= 1989 & joint_df$year < 1999 | (joint_df$year >= 2014 & joint_df$year <= 2020) , "black", "lightgrey"), size = 2, alpha = 0.7) +
  geom_line(aes(y = posterior_prob), color = ifelse(joint_df$year >= 1989 & joint_df$year < 1999 | (joint_df$year >= 2014 & joint_df$year <= 2020), "red3", "lightgrey"), size = 2, alpha = 0.7) +
  geom_line(aes(y = changepoint, group = segment), color = ifelse(joint_df$year >= 1989 & joint_df$year < 1999 | (joint_df$year >= 2014 & joint_df$year <= 2020), "#313695", "lightgrey"), size = 2, alpha = 0.6)

com_st1

