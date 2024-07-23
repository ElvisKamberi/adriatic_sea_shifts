# Run after the Principal component analysis
# set working directory
#setwd("C:/Users/Elvis/Desktop/UHH/PhD project/test correlation/analyses")
setwd("C:/Users/Elvis/Desktop/UHH/PhD project/Manuscripts/1st Manuscript/Data & Codes")
library(readr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)



# SST analysis ####

sst <- read.csv("sst_df.csv")
sst <- subset(sst, year >= 1970 & year <= 2020)
View(sst)
str(sst)

# save as a new df to calculate the anomalies

Anom_sst <- sst
# Save the dimensions (number of rows[1] and columns[2] ) in a vector
n <- dim(Anom_sst)
# Loop to convert actual values into anomaly values (incl. missing 
# values in the average- and anomaly-calculation); the loop starts with the 
# second column to exclude "Year" (no variable) and ends with the last column 
# (defined by the second element of the vector n (n[2]) )
for (i in 2:n[2]) {
  Anom_sst[ ,i] <- Anom_sst[ ,i] - mean(Anom_sst[ ,i], na.rm = TRUE)}



# First, the limits of the y-axes (ylim) are defined by calculating the minimum
# and maximum anomaly of each variable
y.low <- apply(Anom_sst[,-1], MARGIN = 2, FUN = min, na.rm = TRUE)
y.low <- y.low+1/10*y.low
y.upp <- apply(Anom_sst[,-1], MARGIN = 2, FUN = max, na.rm = TRUE)
y.upp <- y.upp+1/10*y.upp



anomalies <- ggplot(Anom_sst, aes(x = year, y = mean_sst)) +
  geom_bar(stat = "identity", fill = "#CC6600") +
  scale_y_continuous(limits = c(y.low["mean_sst"], y.upp["mean_sst"]),
                     name = "SST anomaly (°C)") +
  labs(x = "") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 11, color = "black", face = "bold"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  scale_x_continuous(breaks = seq(1970, 2020, by=10)) +
  xlab(NULL)

anomalies


# convert year column from as character to as.Date
Anom_sst$year <- format(as.Date(paste0(Anom_sst$year, "-01-01")), "%Y")
Anom_sst


# Remove min_sst and max_sst columns
Anom_sst <- Anom_sst %>%
  select(-min_sst, -max_sst)
Anom_sst

# save sst anomalies data as sst for easier manipulation
sst <- Anom_sst

# load landing data

catch <- read.csv("landings_data.csv")
catch


# remove first (number) and third (total catches) columns from catch
catch <- catch[,c(-1,-3)]


# spread long to wide
catch_wide <- spread(catch, key = Species, value = catches)
View(catch_wide)


#### log transform catch_wide data

transformed_data <- catch_wide %>%
  mutate(across(-1, ~ log(. + 1)))



# Merge the two dataframes sst and transformed_data based on the year column

total <- merge(sst, transformed_data, by = "year")
total


## Correlation #

## remove outliers in the data

# save the object as object in order not to modify the original dataframe
dublicate <- total


## remove the outlier in the year 1982 #hake
outliers_hake <- boxplot.stats(dublicate$`Merluccius merluccius`)$out
dublicate[dublicate$`Merluccius merluccius` %in% outliers_hake & dublicate$year == 1982, "Merluccius merluccius"] <- NA


### remove the outlier in year 1982 Mustelus spp 

outlier_mus <- boxplot.stats(dublicate$`Mustelus spp`)$out
dublicate[dublicate$`Mustelus spp` %in% outlier_mus, "Mustelus spp"] <- NA



### LAG analysis

#change column names to lower case
names(dublicate) <- tolower(names(dublicate))

# replace the " " with "_"
names(dublicate) <- gsub(" ", "_", names(dublicate), fixed = TRUE)


### perform correlation analyses

#Atherinidae
ccf_ather_2 <- ccf(dublicate$mean_sst, dublicate$atherinidae, lag.max = 2, na.action = na.pass)
ccf_ather_2

#Boops boops
ccf_boopboo_2 <- ccf(dublicate$mean_sst, dublicate$boops_boops, lag.max = 2, na.action = na.pass)
ccf_boopboo_2

#Chamelea gallina
ccf_chamgal_2 <- ccf(dublicate$mean_sst, dublicate$chamelea_gallina, lag.max = 2, na.action = na.pass)


#anchovy
ccf_engrenc_2 <- ccf(dublicate$mean_sst, dublicate$engraulis_encrasicolus, lag.max = 2, na.action = na.pass)


#squids
ccf_squids_2 <- ccf(dublicate$mean_sst, dublicate$`loliginidae,_ommastrephidae`, lag.max = 2, na.action = na.pass)

#lophius
ccf_lophius_2 <- ccf(dublicate$mean_sst, dublicate$lophius_spp, lag.max = 2, na.action = na.pass)


# Merlucius merlucius
ccf_mermer_2 <- ccf(dublicate$mean_sst, dublicate$merluccius_merluccius, lag.max = 2, na.action = na.pass)


#micromesistius poutasou
ccf_micmpou_2 <- ccf(dublicate$mean_sst, dublicate$micromesistius_poutassou, lag.max = 2, na.action = na.pass)


#mugilidae
ccf_mugil_2 <- ccf(dublicate$mean_sst, dublicate$mugilidae, lag.max = 2, na.action = na.pass)


#mullus
ccf_mullus_2 <- ccf(dublicate$mean_sst, dublicate$mullus_spp, lag.max = 2, na.action = na.pass)


#mustelus
ccf_mustelus_2 <- ccf(dublicate$mean_sst, dublicate$mustelus_spp, lag.max = 2, na.action = na.pass)


#neprnor
ccf_neprnor_2 <- ccf(dublicate$mean_sst, dublicate$nephrops_norvegicus, lag.max = 2, na.action = na.pass)

#octopus
ccf_octo_2 <- ccf(dublicate$mean_sst, dublicate$octopodidea, lag.max = 2, na.action = na.pass)

#papelon
ccf_papelon_2 <- ccf(dublicate$mean_sst, dublicate$parapenaeus_longirostris, lag.max = 2, na.action = na.pass)

#rajiformes
ccf_rays_2 <- ccf(dublicate$mean_sst, dublicate$rajiformes, lag.max = 2, na.action = na.pass)

#sardina
ccf_sardina_2 <- ccf(dublicate$mean_sst, dublicate$sardina_pilchardus, lag.max = 2, na.action = na.pass)

#sardinella
ccf_sardinella_2 <- ccf(dublicate$mean_sst, dublicate$sardinella_aurita, lag.max = 2, na.action = na.pass)


#scomber
ccf_scomber_2 <- ccf(dublicate$mean_sst, dublicate$scomber_spp, lag.max = 2, na.action = na.pass)

#scophthalmidae

ccf_scophthalmidae_2 <- ccf(dublicate$mean_sst, dublicate$scophthalmidae, lag.max = 2, na.action = na.pass)

#scorpaenidae
ccf_scorpaenidae_2 <- ccf(dublicate$mean_sst, dublicate$scorpaenidae, lag.max = 2, na.action = na.pass)


#sepiidae
ccf_sepia_2 <- ccf(dublicate$mean_sst, dublicate$`sepiidae,_sepiolidae`, lag.max = 2, na.action = na.pass)

#sole
ccf_solesol_2 <- ccf(dublicate$mean_sst, dublicate$solea_solea, lag.max = 2, na.action = na.pass)



#spicara
ccf_spicara_2 <- ccf(dublicate$mean_sst, dublicate$spicara_spp, lag.max = 2, na.action = na.pass)


#squalus
ccf_squalus_2 <- ccf(dublicate$mean_sst, dublicate$squalus_spp, lag.max = 2, na.action = na.pass)


#squilla mantis
ccf_squiman_2 <- ccf(dublicate$mean_sst, dublicate$squilla_mantis, lag.max = 2, na.action = na.pass)


#trachurus
ccf_trachurus_2 <- ccf(dublicate$mean_sst, dublicate$trachurus_spp, lag.max = 2, na.action = na.pass)

#triglidae
ccf_triglidae_2 <- ccf(dublicate$mean_sst, dublicate$triglidae, lag.max = 2, na.action = na.pass)


## PCA correlation

# Merge dublicate df used for correlation analyses with pca_pc containing principal component data

final_df <- merge(dublicate, pca_pc, by = "year")
final_df

# PC 1
ccf_pc1_2 <- ccf(final_df$mean_sst, final_df$Axis1, lag.max = 2, na.action = na.pass)
print(ccf_pc1_2)


# PC 2
ccf_pc2_2 <- ccf(final_df$mean_sst, final_df$Axis2, lag.max = 2, na.action = na.pass)
print(ccf_pc2_2)


### Chelton validation method ####
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
final_df <- lapply(final_df, replace_na_with_mean)

# Convert list back to data frame
final_df <- as.data.frame(final_df)

## Atherinidae
# Calculate the correlation coefficient between mean_sst and atherinidae
cor_ather <- cor.test(final_df$mean_sst, final_df$atherinidae, use = "complete.obs")
cor_obs <- cor_ather$estimate # Observed correlation coefficient
p_value <- cor_ather$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_ath <- acf(final_df$atherinidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$atherinidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_ath$acf[2:(n_lag + 1)] # Autocorrelation values for atherinidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


## Boops boops
# Calculate the correlation coefficient between mean_sst and boops boops
cor_boopboo <- cor.test(final_df$mean_sst, final_df$boops_boops, use = "complete.obs")
cor_obs <- cor_boopboo$estimate # Observed correlation coefficient
p_value <- cor_boopboo$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_bb <- acf(final_df$boops_boops, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$boops_boops))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_bb$acf[2:(n_lag + 1)] # Autocorrelation values for boops boops
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Loliginidae, Ommastrephidae

# Calculate the correlation coefficient between mean_sst and squids
cor_squids <- cor.test(final_df$mean_sst, final_df$`loliginidae,_ommastrephidae`, use = "complete.obs")
cor_obs <- cor_squids$estimate # Observed correlation coefficient
p_value <- cor_squids$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_squid <- acf(final_df$`loliginidae,_ommastrephidae`, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$`loliginidae,_ommastrephidae`))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_squid$acf[2:(n_lag + 1)] # Autocorrelation values for squids
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Lophius spp
# Calculate the correlation coefficient between mean_sst and squids
cor_lophius <- cor.test(final_df$mean_sst, final_df$lophius_spp, use = "complete.obs")
cor_obs <- cor_lophius$estimate # Observed correlation coefficient
p_value <- cor_lophius$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_lophius <- acf(final_df$lophius_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$lophius_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_lophius$acf[2:(n_lag + 1)] # Autocorrelation values for lophius
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}

# Micromesistius poutassou
# Calculate the correlation coefficient between mean_sst and MICMPOU
cor_micmpou <- cor.test(final_df$mean_sst, final_df$micromesistius_poutassou, use = "complete.obs")
cor_obs <- cor_micmpou$estimate # Observed correlation coefficient
p_value <- cor_micmpou$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_micmpou <- acf(final_df$micromesistius_poutassou, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$micromesistius_poutassou))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_micmpou$acf[2:(n_lag + 1)] # Autocorrelation values for MICMPOU
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Mullus sp
# Calculate the correlation coefficient between mean_sst and Mullus spp
cor_mullus <- cor.test(final_df$mean_sst, final_df$mullus_spp, use = "complete.obs")
cor_obs <- cor_mullus$estimate # Observed correlation coefficient
p_value <- cor_mullus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_mullus <- acf(final_df$mullus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$mullus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_mullus$acf[2:(n_lag + 1)] # Autocorrelation values for Mullus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}

# Mustelus
# Calculate the correlation coefficient between mean_sst and Mustelus
cor_mustelus <- cor.test(final_df$mean_sst, final_df$mustelus_spp, use = "complete.obs")
cor_obs <- cor_mustelus$estimate # Observed correlation coefficient
p_value <- cor_mustelus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_mustelus <- acf(final_df$mustelus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$mustelus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_mustelus$acf[2:(n_lag + 1)] # Autocorrelation values for Mustelus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Parapenaeus longirostris
# Calculate the correlation coefficient between mean_sst and papelon
cor_papelon <- cor.test(final_df$mean_sst, final_df$parapenaeus_longirostris, use = "complete.obs")
cor_obs <- cor_papelon$estimate # Observed correlation coefficient
p_value <- cor_papelon$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_papelon <- acf(final_df$parapenaeus_longirostris, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$parapenaeus_longirostris))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_papelon$acf[2:(n_lag + 1)] # Autocorrelation values for papelon
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Rajiformes
# Calculate the correlation coefficient between mean_sst and rajiformes
cor_rays <- cor.test(final_df$mean_sst, final_df$rajiformes, use = "complete.obs")
cor_obs <- cor_rays$estimate # Observed correlation coefficient
p_value <- cor_rays$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_rays <- acf(final_df$rajiformes, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$rajiformes))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_rays$acf[2:(n_lag + 1)] # Autocorrelation values for rajiformes
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Sardinella aurita
# Calculate the correlation coefficient between mean_sst and sardinella
cor_sardinella <- cor.test(final_df$mean_sst, final_df$sardinella_aurita, use = "complete.obs")
cor_obs <- cor_sardinella$estimate # Observed correlation coefficient
p_value <- cor_sardinella$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_sardinella <- acf(final_df$sardinella_aurita, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$sardinella_aurita))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_sardinella$acf[2:(n_lag + 1)] # Autocorrelation values for S.aurita
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Scomber spp
# Calculate the correlation coefficient between mean_sst and scomber
cor_scomber <- cor.test(final_df$mean_sst, final_df$scomber_spp, use = "complete.obs")
cor_obs <- cor_scomber$estimate # Observed correlation coefficient
p_value <- cor_scomber$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_scomber <- acf(final_df$scomber_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$scomber_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_scomber$acf[2:(n_lag + 1)] # Autocorrelation values for Scomber
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Scophthalmidae
# Calculate the correlation coefficient between mean_sst and scophthalmidae
cor_scoph <- cor.test(final_df$mean_sst, final_df$scophthalmidae, use = "complete.obs")
cor_obs <- cor_scoph$estimate # Observed correlation coefficient
p_value <- cor_scoph$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_scoph <- acf(final_df$scophthalmidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$scophthalmidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_scoph$acf[2:(n_lag + 1)] # Autocorrelation values for scophthalmidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Scorpaenidae
# Calculate the correlation coefficient between mean_sst and scorpaenidae
cor_scorpaenidae <- cor.test(final_df$mean_sst, final_df$scorpaenidae, use = "complete.obs")
cor_obs <- cor_scorpaenidae$estimate # Observed correlation coefficient
p_value <- cor_scorpaenidae$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_scorpaenidae <- acf(final_df$scorpaenidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$scorpaenidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_scorpaenidae$acf[2:(n_lag + 1)] # Autocorrelation values for scorpaenidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Sepiidae, Sepiolidae
# Calculate the correlation coefficient between mean_sst and sepiidae
cor_sepia <- cor.test(final_df$mean_sst, final_df$sepiidae._sepiolidae, use = "complete.obs")
cor_obs <- cor_sepia$estimate # Observed correlation coefficient
p_value <- cor_sepia$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_sepia <- acf(final_df$sepiidae._sepiolidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$sepiidae._sepiolidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_sepia$acf[2:(n_lag + 1)] # Autocorrelation values for sepiidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Solea solea
# Calculate the correlation coefficient between mean_sst and sepiidae
cor_solea <- cor.test(final_df$mean_sst, final_df$solea_solea, use = "complete.obs")
cor_obs <- cor_solea$estimate # Observed correlation coefficient
p_value <- cor_solea$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_solea <- acf(final_df$solea_solea, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$solea_solea))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_solea$acf[2:(n_lag + 1)] # Autocorrelation values for solea
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Spicara
# Calculate the correlation coefficient between mean_sst and spicara
cor_spicara <- cor.test(final_df$mean_sst, final_df$spicara_spp, use = "complete.obs")
cor_obs <- cor_spicara$estimate # Observed correlation coefficient
p_value <- cor_spicara$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_spicara <- acf(final_df$spicara_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$spicara_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_spicara$acf[2:(n_lag + 1)] # Autocorrelation values for spicara
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Squalus
# Calculate the correlation coefficient between mean_sst and squalus
cor_squalus <- cor.test(final_df$mean_sst, final_df$squalus_spp, use = "complete.obs")
cor_obs <- cor_squalus$estimate # Observed correlation coefficient
p_value <- cor_squalus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_squalus <- acf(final_df$squalus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$squalus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_squalus$acf[2:(n_lag + 1)] # Autocorrelation values for squalus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Trachurus spp
# Calculate the correlation coefficient between mean_sst and trachurus
cor_trachurus <- cor.test(final_df$mean_sst, final_df$trachurus_spp, use = "complete.obs")
cor_obs <- cor_trachurus$estimate # Observed correlation coefficient
p_value <- cor_trachurus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_trachurus <- acf(final_df$trachurus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$trachurus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_trachurus$acf[2:(n_lag + 1)] # Autocorrelation values for trachurus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Triglidae
# Calculate the correlation coefficient between mean_sst and triglidae
cor_triglidae <- cor.test(final_df$mean_sst, final_df$triglidae, use = "complete.obs")
cor_obs <- cor_triglidae$estimate # Observed correlation coefficient
p_value <- cor_triglidae$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_triglidae <- acf(final_df$triglidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$triglidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_triglidae$acf[2:(n_lag + 1)] # Autocorrelation values for triglidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# PC1
# Calculate the correlation coefficient between mean_sst and PC1
cor_pc1 <- cor.test(final_df$mean_sst, final_df$Axis1, use = "complete.obs")
cor_obs <- cor_pc1$estimate # Observed correlation coefficient
p_value <- cor_pc1$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_pc1 <- acf(final_df$Axis1, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$pc1))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_pc1$acf[2:(n_lag + 1)] # Autocorrelation values for pc1
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# PC2
# Calculate the correlation coefficient between mean_sst and PC1
cor_pc2 <- cor.test(final_df$mean_sst, final_df$Axis2, use = "complete.obs")
cor_obs <- cor_pc2$estimate # Observed correlation coefficient
p_value <- cor_pc2$p.value # P-value

# Calculate the autocorrelation values for each series
acf_sst <- acf(final_df$mean_sst, plot = FALSE)
acf_pc2 <- acf(final_df$Axis2, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$mean_sst, final_df$pc2))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_sst$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_sst$acf[2:(n_lag + 1)] # Autocorrelation values for mean_sst
rho_yy <- acf_pc2$acf[2:(n_lag + 1)] # Autocorrelation values for pc1
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# barplot
# Create a data frame with the results of the correlation analysis
df_sst <- data.frame(
  species = c("Atherinidae", "Boops boops", "Loliginidae, Ommastrephidae", "Lophius spp", "Micromesistius poutassou", "Mullus spp", "Mustelus spp", "Parapenaeus longirostris", "Sardinella aurita", "Scomber spp", "Scophthalmidae", "Sepiidae, Sepiolidae", "Solea solea", "Spicara spp", "Squalus spp", "Trachurus spp", "Triglidae","PC1"),
  value = c(-0.8,-0.81, -0.54, 0.47, -0.75, 0.66, -0.53, 0.65, -0.69, 0.46, -0.74, -0.52, 0.39, -0.76, -0.58, -0.57, 0.69, 0.77)
)
df_sst


sst_ccf_plot <- ggplot(df_sst, aes(x = value, y = species, fill = value)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  scale_x_continuous(limits = c(-1, 1), expand = c(0,0)) +
  scale_fill_gradient2(low = "#313695", high = "#d73027", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2), x = ifelse(value > 0, value + 0.1, value - 0.1)), 
            hjust = ifelse(df_sst$value > 0, 0.2, 0.7), 
            color = "black") +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.position = "none") +
  labs(x = "Correlation Coefficient", y = "")

sst_ccf_plot

# Arrange the data frame by value
df_sst <- df_sst[order(df_sst$value),]

# Change the order of factor levels
df_sst$species <- factor(df_sst$species, levels = df_sst$species)


# plot
sst_ccf_plot <- ggplot(df_sst, aes(x = value, y = species, fill = value)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  scale_x_continuous(limits = c(-1, 1), expand = c(0,0)) +
  scale_fill_gradient2(low = "#313695", high = "#d73027", mid = "white", midpoint = 0) +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.position = "none") +
  labs(x = "Correlation Coefficient", y = "")

sst_ccf_plot

sst_anomalies <- ggplot(final_df, aes(x = year, y = mean_sst, fill = mean_sst >= 0)) +
  geom_col(col = "black", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_manual(values = c("#313695", "#d73027"), guide = "none") +
  labs(x = "", y = "SST anomaly (°C)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(0.66,0.3,0.2,0.2), "cm"),
        aspect.ratio = 1/2) +
  scale_x_continuous(expand = c(0, 0))
sst_anomalies

# NAO analysis ####

nao_data <- read_csv("winter_nao_data.csv")
View(nao_data)

nao_plot <- ggplot(nao_data, aes(x = year, y = winter_nao_index, fill = winter_nao_index >= 0)) +
  geom_col(col = "black", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_manual(values = c("#313695", "#d73027"), guide = "none") +
  labs(title = "",
       x = " ",
       y = "NAO") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(0,0.3,0.2,0.2), "cm"),
        aspect.ratio = 1/2) +
  scale_x_continuous(expand = c(0, 0))

nao_plot

# merge nao_data with final_df

final_df <- merge(nao_data, final_df, by = "year")
View(final_df)


# Correlation and significance test (Chelton Method)

## Atherinidae
ccf_ather_2 <- ccf(final_df$nao_index, final_df$atherinidae, lag.max = 2, na.action = na.pass)
ccf_ather_2

# Calculate the correlation coefficient between nao_index and atherinidae
cor_ather <- cor.test(final_df$nao_index, final_df$atherinidae, use = "complete.obs")
cor_obs <- cor_ather$estimate # Observed correlation coefficient
p_value <- cor_ather$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_ath <- acf(final_df$atherinidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$atherinidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_ath$acf[2:(n_lag + 1)] # Autocorrelation values for atherinidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


## Boops boops

ccf_bb_2 <- ccf(final_df$nao_index, final_df$boops_boops, lag.max = 2, na.action = na.pass)
ccf_bb_2


# Calculate the correlation coefficient between nao_index and boops boops
cor_boopboo <- cor.test(final_df$nao_index, final_df$boops_boops, use = "complete.obs")
cor_obs <- cor_boopboo$estimate # Observed correlation coefficient
p_value <- cor_boopboo$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_bb <- acf(final_df$boops_boops, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$boops_boops))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_bb$acf[2:(n_lag + 1)] # Autocorrelation values for boops boops
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# chamelea gallina ##

ccf_chamgal <- ccf(final_df$nao_index, final_df$chamelea_gallina, lag.max = 2, na.action = na.pass)
ccf_chamgal

# Calculate the correlation coefficient between nao_index and chamelea gallina
cor_chamgal <- cor.test(final_df$nao_index, final_df$chamelea_gallina, use = "complete.obs")
cor_obs <- cor_chamgal$estimate # Observed correlation coefficient
p_value <- cor_chamgal$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_chamgal <- acf(final_df$chamelea_gallina, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$chamelea_gallina))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_chamgal$acf[2:(n_lag + 1)] # Autocorrelation values for chamelea gallina
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Anchovy ##

ccf_anchovy <- ccf(final_df$nao_index, final_df$engraulis_encrasicolus, lag.max = 2, na.action = na.pass)
ccf_anchovy

# Calculate the correlation coefficient between nao_index and anchovy
cor_anchovy <- cor.test(final_df$nao_index, final_df$engraulis_encrasicolus, use = "complete.obs")
cor_obs <- cor_anchovy$estimate # Observed correlation coefficient
p_value <- cor_anchovy$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_anchovy <- acf(final_df$engraulis_encrasicolus, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$engraulis_encrasicolus))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_anchovy$acf[2:(n_lag + 1)] # Autocorrelation values for anchovy
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Loliginidae, Ommastrephidae

ccf_squids_2 <- ccf(final_df$nao_index, final_df$loliginidae._ommastrephidae, lag.max = 2, na.action = na.pass)
ccf_squids_2

# Calculate the correlation coefficient between nao_index and squids
cor_squids <- cor.test(final_df$nao_index, final_df$loliginidae._ommastrephidae, use = "complete.obs")
cor_obs <- cor_squids$estimate # Observed correlation coefficient
p_value <- cor_squids$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_squid <- acf(final_df$loliginidae._ommastrephidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$`loliginidae,_ommastrephidae`))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_squid$acf[2:(n_lag + 1)] # Autocorrelation values for squids
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Lophius spp


ccf_lophius_2 <- ccf(final_df$nao_index, final_df$lophius_spp, lag.max = 2, na.action = na.pass)
ccf_lophius_2

# Calculate the correlation coefficient between nao_index and squids
cor_lophius <- cor.test(final_df$nao_index, final_df$lophius_spp, use = "complete.obs")
cor_obs <- cor_lophius$estimate # Observed correlation coefficient
p_value <- cor_lophius$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_lophius <- acf(final_df$lophius_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$lophius_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_lophius$acf[2:(n_lag + 1)] # Autocorrelation values for lophius
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Hake ##


ccf_hake <- ccf(final_df$nao_index, final_df$merluccius_merluccius, lag.max = 2, na.action = na.pass)
ccf_hake

# Calculate the correlation coefficient between nao and hake
cor_hake <- cor.test(final_df$nao_index, final_df$merluccius_merluccius, use = "complete.obs")
cor_obs <- cor_hake$estimate # Observed correlation coefficient
p_value <- cor_hake$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_hake <- acf(final_df$merluccius_merluccius, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$merluccius_merluccius))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao
rho_yy <- acf_hake$acf[2:(n_lag + 1)] # Autocorrelation values for hake
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Micromesistius poutassou

ccf_micmpou_2 <- ccf(final_df$nao_index, final_df$micromesistius_poutassou, lag.max = 2, na.action = na.pass)
ccf_micmpou_2

# Calculate the correlation coefficient between nao_index and MICMPOU
cor_micmpou <- cor.test(final_df$nao_index, final_df$micromesistius_poutassou, use = "complete.obs")
cor_obs <- cor_micmpou$estimate # Observed correlation coefficient
p_value <- cor_micmpou$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_micmpou <- acf(final_df$micromesistius_poutassou, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$micromesistius_poutassou))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_micmpou$acf[2:(n_lag + 1)] # Autocorrelation values for MICMPOU
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}




#Mugilidae ##

ccf_mugilidae <- ccf(final_df$nao_index, final_df$mugilidae, lag.max = 2, na.action = na.pass)
ccf_mugilidae

# Calculate the correlation coefficient between nao and mugilidae
cor_mugilidae <- cor.test(final_df$nao_index, final_df$mugilidae, use = "complete.obs")
cor_obs <- cor_mugilidae$estimate # Observed correlation coefficient
p_value <- cor_mugilidae$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_mugilidae <- acf(final_df$mugilidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$mugilidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for NAO
rho_yy <- acf_mugilidae$acf[2:(n_lag + 1)] # Autocorrelation values for mugilidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Mullus sp

ccf_mullus_2 <- ccf(final_df$nao_index, final_df$mullus_spp, lag.max = 2, na.action = na.pass)
ccf_mullus_2

# Calculate the correlation coefficient between nao_index and Mullus spp
cor_mullus <- cor.test(final_df$nao_index, final_df$mullus_spp, use = "complete.obs")
cor_obs <- cor_mullus$estimate # Observed correlation coefficient
p_value <- cor_mullus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_mullus <- acf(final_df$mullus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$mullus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_mullus$acf[2:(n_lag + 1)] # Autocorrelation values for Mullus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}

# Mustelus

ccf_mustelus_2 <- ccf(final_df$nao_index, final_df$mustelus_spp, lag.max = 2, na.action = na.pass)
ccf_mustelus_2

# Calculate the correlation coefficient between nao_index and Mustelus
cor_mustelus <- cor.test(final_df$nao_index, final_df$mustelus_spp, use = "complete.obs")
cor_obs <- cor_mustelus$estimate # Observed correlation coefficient
p_value <- cor_mustelus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_mustelus <- acf(final_df$mustelus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$mustelus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_mustelus$acf[2:(n_lag + 1)] # Autocorrelation values for Mustelus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# nephrops norvegicus ##

ccf_nephnor_2 <- ccf(final_df$nao_index, final_df$nephrops_norvegicus, lag.max = 2, na.action = na.pass)
ccf_nephnor_2


# Calculate the correlation coefficient between mean_sst and nephnor
cor_nephnor <- cor.test(final_df$nao_index, final_df$nephrops_norvegicus, use = "complete.obs")
cor_obs <- cor_nephnor$estimate # Observed correlation coefficient
p_value <- cor_nephnor$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_nephnor <- acf(final_df$nephrops_norvegicus, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$nephrops_norvegicus))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao
rho_yy <- acf_nephnor$acf[2:(n_lag + 1)] # Autocorrelation values for nephnor
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Octopodidea ##

ccf_octopus_2 <- ccf(final_df$nao_index, final_df$octopodidea, lag.max = 2, na.action = na.pass)
ccf_octopus_2

# Calculate the correlation coefficient between nao and octopus
cor_octopus <- cor.test(final_df$nao_index, final_df$octopodidea, use = "complete.obs")
cor_obs <- cor_octopus$estimate # Observed correlation coefficient
p_value <- cor_octopus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_octopus <- acf(final_df$octopodidea, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$octopodidea))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao
rho_yy <- acf_octopus$acf[2:(n_lag + 1)] # Autocorrelation values for octopus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Parapenaeus longirostris

ccf_papelon_2 <- ccf(final_df$nao_index, final_df$parapenaeus_longirostris, lag.max = 2, na.action = na.pass)
ccf_papelon_2

# Calculate the correlation coefficient between nao_index and papelon
cor_papelon <- cor.test(final_df$nao_index, final_df$parapenaeus_longirostris, use = "complete.obs")
cor_obs <- cor_papelon$estimate # Observed correlation coefficient
p_value <- cor_papelon$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_papelon <- acf(final_df$parapenaeus_longirostris, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$parapenaeus_longirostris))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_papelon$acf[2:(n_lag + 1)] # Autocorrelation values for papelon
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Rajiformes

ccf_rays_2 <- ccf(final_df$nao_index, final_df$rajiformes, lag.max = 2, na.action = na.pass)
ccf_rays_2

# Calculate the correlation coefficient between nao_index and rajiformes
cor_rays <- cor.test(final_df$nao_index, final_df$rajiformes, use = "complete.obs")
cor_obs <- cor_rays$estimate # Observed correlation coefficient
p_value <- cor_rays$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_rays <- acf(final_df$rajiformes, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$rajiformes))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_rays$acf[2:(n_lag + 1)] # Autocorrelation values for rajiformes
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Sardina pilchardus

ccf_sardine <- ccf(final_df$nao_index, final_df$sardina_pilchardus, lag.max = 2, na.action = na.pass)
ccf_sardine

# Calculate the correlation coefficient between nao_index and sardine
cor_sardine <- cor.test(final_df$nao_index, final_df$sardina_pilchardus, use = "complete.obs")
cor_obs <- cor_sardine$estimate # Observed correlation coefficient
p_value <- cor_sardine$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_sardine <- acf(final_df$sardina_pilchardus, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$sardina_pilchardus))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_sardine$acf[2:(n_lag + 1)] # Autocorrelation values for sardine
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Sardinella aurita

ccf_sardinella_2 <- ccf(final_df$nao_index, final_df$sardinella_aurita, lag.max = 2, na.action = na.pass)
ccf_sardinella_2

# Calculate the correlation coefficient between nao_index and sardinella
cor_sardinella <- cor.test(final_df$nao_index, final_df$sardinella_aurita, use = "complete.obs")
cor_obs <- cor_sardinella$estimate # Observed correlation coefficient
p_value <- cor_sardinella$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_sardinella <- acf(final_df$sardinella_aurita, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$sardinella_aurita))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_sardinella$acf[2:(n_lag + 1)] # Autocorrelation values for S.aurita
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Scomber spp

ccf_scomber_2 <- ccf(final_df$nao_index, final_df$scomber_spp, lag.max = 2, na.action = na.pass)
ccf_scomber_2

# Calculate the correlation coefficient between nao_index and scomber
cor_scomber <- cor.test(final_df$nao_index, final_df$scomber_spp, use = "complete.obs")
cor_obs <- cor_scomber$estimate # Observed correlation coefficient
p_value <- cor_scomber$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_scomber <- acf(final_df$scomber_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$scomber_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_scomber$acf[2:(n_lag + 1)] # Autocorrelation values for Scomber
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Scophthalmidae


ccf_scophthalmidae_2 <- ccf(final_df$nao_index, final_df$scophthalmidae, lag.max = 2, na.action = na.pass)
ccf_scophthalmidae_2

# Calculate the correlation coefficient between nao_index and scophthalmidae
cor_scoph <- cor.test(final_df$nao_index, final_df$scophthalmidae, use = "complete.obs")
cor_obs <- cor_scoph$estimate # Observed correlation coefficient
p_value <- cor_scoph$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_scoph <- acf(final_df$scophthalmidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$scophthalmidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_scoph$acf[2:(n_lag + 1)] # Autocorrelation values for scophthalmidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Scorpaenidae

ccf_scorpaenidae_2 <- ccf(final_df$nao_index, final_df$scorpaenidae, lag.max = 2, na.action = na.pass)
ccf_scorpaenidae_2

# Calculate the correlation coefficient between nao_index and scorpaenidae
cor_scorpaenidae <- cor.test(final_df$nao_index, final_df$scorpaenidae, use = "complete.obs")
cor_obs <- cor_scorpaenidae$estimate # Observed correlation coefficient
p_value <- cor_scorpaenidae$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_scorpaenidae <- acf(final_df$scorpaenidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$scorpaenidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_scorpaenidae$acf[2:(n_lag + 1)] # Autocorrelation values for scorpaenidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Sepiidae, Sepiolidae

ccf_sepia_2 <- ccf(final_df$nao_index, final_df$sepiidae._sepiolidae, lag.max = 2, na.action = na.pass)
ccf_sepia_2

# Calculate the correlation coefficient between nao_index and sepiidae
cor_sepia <- cor.test(final_df$nao_index, final_df$sepiidae._sepiolidae, use = "complete.obs")
cor_obs <- cor_sepia$estimate # Observed correlation coefficient
p_value <- cor_sepia$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_sepia <- acf(final_df$sepiidae._sepiolidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$sepiidae._sepiolidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_sepia$acf[2:(n_lag + 1)] # Autocorrelation values for sepiidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Solea solea

ccf_sole_2 <- ccf(final_df$nao_index, final_df$solea_solea, lag.max = 2, na.action = na.pass)
ccf_sole_2

# Calculate the correlation coefficient between nao_index and sepiidae
cor_solea <- cor.test(final_df$nao_index, final_df$solea_solea, use = "complete.obs")
cor_obs <- cor_solea$estimate # Observed correlation coefficient
p_value <- cor_solea$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_solea <- acf(final_df$solea_solea, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$solea_solea))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_solea$acf[2:(n_lag + 1)] # Autocorrelation values for solea
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Spicara

ccf_spicara_2 <- ccf(final_df$nao_index, final_df$spicara_spp, lag.max = 2, na.action = na.pass)
ccf_spicara_2

# Calculate the correlation coefficient between nao_index and spicara
cor_spicara <- cor.test(final_df$nao_index, final_df$spicara_spp, use = "complete.obs")
cor_obs <- cor_spicara$estimate # Observed correlation coefficient
p_value <- cor_spicara$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_spicara <- acf(final_df$spicara_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$spicara_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_spicara$acf[2:(n_lag + 1)] # Autocorrelation values for spicara
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Squalus

ccf_squalus_2 <- ccf(final_df$nao_index, final_df$squalus_spp, lag.max = 2, na.action = na.pass)
ccf_squalus_2

# Calculate the correlation coefficient between nao_index and squalus
cor_squalus <- cor.test(final_df$nao_index, final_df$squalus_spp, use = "complete.obs")
cor_obs <- cor_squalus$estimate # Observed correlation coefficient
p_value <- cor_squalus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_squalus <- acf(final_df$squalus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$squalus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_squalus$acf[2:(n_lag + 1)] # Autocorrelation values for squalus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}

# Squilla mantis

ccf_squilman <- ccf(final_df$nao_index, final_df$squilla_mantis, lag.max = 2, na.action = na.pass)
ccf_squilman

# Calculate the correlation coefficient between nao_index and squila mantis
cor_squilman <- cor.test(final_df$nao_index, final_df$squilla_mantis, use = "complete.obs")
cor_obs <- cor_squilman$estimate # Observed correlation coefficient
p_value <- cor_squilman$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_squilman <- acf(final_df$squilla_mantis, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$squilla_mantis))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_squilman$acf[2:(n_lag + 1)] # Autocorrelation values for squila mantis
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Trachurus spp

ccf_trachurus_2 <- ccf(final_df$nao_index, final_df$trachurus_spp, lag.max = 2, na.action = na.pass)
ccf_trachurus_2

# Calculate the correlation coefficient between nao_index and trachurus
cor_trachurus <- cor.test(final_df$nao_index, final_df$trachurus_spp, use = "complete.obs")
cor_obs <- cor_trachurus$estimate # Observed correlation coefficient
p_value <- cor_trachurus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_trachurus <- acf(final_df$trachurus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$trachurus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_trachurus$acf[2:(n_lag + 1)] # Autocorrelation values for trachurus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Triglidae

ccf_triglidae_2 <- ccf(final_df$nao_index, final_df$triglidae, lag.max = 2, na.action = na.pass)
ccf_triglidae_2

# Calculate the correlation coefficient between nao_index and triglidae
cor_triglidae <- cor.test(final_df$nao_index, final_df$triglidae, use = "complete.obs")
cor_obs <- cor_triglidae$estimate # Observed correlation coefficient
p_value <- cor_triglidae$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_triglidae <- acf(final_df$triglidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$triglidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_triglidae$acf[2:(n_lag + 1)] # Autocorrelation values for triglidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# PC1
ccf_pc1 <- ccf(final_df$nao_index, final_df$Axis1, lag.max = 2, na.action = na.pass)
ccf_pc1

# Calculate the correlation coefficient between nao_index and PC1
cor_pc1 <- cor.test(final_df$nao_index, final_df$Axis1, use = "complete.obs")
cor_obs <- cor_pc1$estimate # Observed correlation coefficient
p_value <- cor_pc1$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_pc1 <- acf(final_df$Axis1, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$pc1))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_pc1$acf[2:(n_lag + 1)] # Autocorrelation values for pc1
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# PC2
ccf_pc2 <- ccf(final_df$nao_index, final_df$Axis2, lag.max = 2, na.action = na.pass)
ccf_pc2

# Calculate the correlation coefficient between nao_index and PC1
cor_pc2 <- cor.test(final_df$nao_index, final_df$Axis2, use = "complete.obs")
cor_obs <- cor_pc2$estimate # Observed correlation coefficient
p_value <- cor_pc2$p.value # P-value

# Calculate the autocorrelation values for each series
acf_nao <- acf(final_df$nao_index, plot = FALSE)
acf_pc2 <- acf(final_df$Axis2, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$nao_index, final_df$pc2))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_nao$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_nao$acf[2:(n_lag + 1)] # Autocorrelation values for nao_index
rho_yy <- acf_pc2$acf[2:(n_lag + 1)] # Autocorrelation values for pc1
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# barplot 

# Create a data frame with the correlation analysis
df_nao <- data.frame(
  species = c("Atherinidae", "Merluccius merluccius", "Mugilidae", "Mullus spp", "Parapenaeus longirostris", "Scomber spp", "Scophthalmidae", "PC 2"),
  value = c(-0.28, 0.4,-0.4, 0.37, 0.39, 0.39, -0.33, 0.28)
)
df_nao

# Arrange the data frame by value
df_nao <- df_nao[order(df_nao$value),]

# Change the order of factor levels
df_nao$species <- factor(df_nao$species, levels = df_nao$species)

# Create the horizontal histogram chart with modified x-axis and bar colors
nao_ccf_plot <- ggplot(df_nao, aes(x = value, y = species, fill = value)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  scale_x_continuous(limits = c(-1, 1), expand = c(0,0)) +
  scale_fill_gradient2(low = "#313695", high = "#d73027", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2), x = ifelse(value > 0, value + 0.1, value - 0.1)), 
            hjust = ifelse(df_nao$value > 0, 0.5, 0.5), 
            color = "black") +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.position = "none") +
  labs(x = "Correlation Coefficient", y = "")

nao_ccf_plot
nao_plot

# AMO analysis ####

amo_data <- read_csv("amo_data_final.csv")


amo_plot <- ggplot(amo_data, aes(x = year, y = amo_index, fill = amo_index >= 0)) +
  geom_col(col = "black", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid") +
  scale_fill_manual(values = c("#313695", "#d73027"), guide = "none") +
  labs(title = "", x = " ", y = "AMO") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        plot.margin = unit(c(0,0.3,0.2,0.2), "cm"),
        aspect.ratio = 1/2) +
  scale_x_continuous(expand = c(0, 0))

amo_plot




# combine all 6 plots together
grid.arrange(sst_anomalies, amo_plot, nao_plot, 
             sst_ccf_plot, amo_ccf_plot, nao_ccf_plot, 
             nrow = 2)


## ADITIONAL SCRIPT CCF ## DON'T UPLOAD

# merge amo_data with the existing final_df

final_df <- merge(amo_data, final_df, by = "year")
View(final_df)


# Correlation

## Atherinidae

ggscatter(final_df, x = "amo_index", y = "atherinidae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Atherinidae landings")


ccf_ather_2 <- ccf(final_df$amo_index, final_df$atherinidae, lag.max = 2, na.action = na.pass)
ccf_ather_2

# Calculate the correlation coefficient between amo_index and atherinidae
cor_ather <- cor.test(final_df$amo_index, final_df$atherinidae, use = "complete.obs")
cor_obs <- cor_ather$estimate # Observed correlation coefficient
p_value <- cor_ather$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_ath <- acf(final_df$atherinidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$atherinidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_ath$acf[2:(n_lag + 1)] # Autocorrelation values for atherinidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


## Boops boops

ggscatter(final_df, x = "amo_index", y = "boops_boops", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")



ccf_bb_2 <- ccf(final_df$amo_index, final_df$boops_boops, lag.max = 2, na.action = na.pass)
ccf_bb_2


# Calculate the correlation coefficient between amo_index and boops boops
cor_boopboo <- cor.test(final_df$amo_index, final_df$boops_boops, use = "complete.obs")
cor_obs <- cor_boopboo$estimate # Observed correlation coefficient
p_value <- cor_boopboo$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_bb <- acf(final_df$boops_boops, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$boops_boops))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_bb$acf[2:(n_lag + 1)] # Autocorrelation values for boops boops
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



########################### chamelea gallina ##


ggscatter(final_df, x = "amo_index", y = "chamelea_gallina", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_chamgal <- ccf(final_df$amo_index, final_df$chamelea_gallina, lag.max = 2, na.action = na.pass)
ccf_chamgal

# Calculate the correlation coefficient between amo_index and chamelea gallina
cor_chamgal <- cor.test(final_df$amo_index, final_df$chamelea_gallina, use = "complete.obs")
cor_obs <- cor_chamgal$estimate # Observed correlation coefficient
p_value <- cor_chamgal$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_chamgal <- acf(final_df$chamelea_gallina, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$chamelea_gallina))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_chamgal$acf[2:(n_lag + 1)] # Autocorrelation values for chamelea gallina
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


########################### Anchovy ##


ggscatter(final_df, x = "amo_index", y = "engraulis_encrasicolus", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_anchovy <- ccf(final_df$amo_index, final_df$engraulis_encrasicolus, lag.max = 2, na.action = na.pass)
ccf_anchovy

# Calculate the correlation coefficient between amo_index and anchovy
cor_anchovy <- cor.test(final_df$amo_index, final_df$engraulis_encrasicolus, use = "complete.obs")
cor_obs <- cor_anchovy$estimate # Observed correlation coefficient
p_value <- cor_anchovy$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_anchovy <- acf(final_df$engraulis_encrasicolus, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$engraulis_encrasicolus))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_anchovy$acf[2:(n_lag + 1)] # Autocorrelation values for anchovy
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Loliginidae, Ommastrephidae

ggscatter(final_df, x = "amo_index", y = "loliginidae._ommastrephidae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_squids_2 <- ccf(final_df$amo_index, final_df$loliginidae._ommastrephidae, lag.max = 2, na.action = na.pass)
ccf_squids_2

# Calculate the correlation coefficient between amo_index and squids
cor_squids <- cor.test(final_df$amo_index, final_df$loliginidae._ommastrephidae, use = "complete.obs")
cor_obs <- cor_squids$estimate # Observed correlation coefficient
p_value <- cor_squids$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_squid <- acf(final_df$loliginidae._ommastrephidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$`loliginidae,_ommastrephidae`))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_squid$acf[2:(n_lag + 1)] # Autocorrelation values for squids
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Lophius spp

ggscatter(final_df, x = "amo_index", y = "lophius_spp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_lophius_2 <- ccf(final_df$amo_index, final_df$lophius_spp, lag.max = 2, na.action = na.pass)
ccf_lophius_2

# Calculate the correlation coefficient between amo_index and squids
cor_lophius <- cor.test(final_df$amo_index, final_df$lophius_spp, use = "complete.obs")
cor_obs <- cor_lophius$estimate # Observed correlation coefficient
p_value <- cor_lophius$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_lophius <- acf(final_df$lophius_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$lophius_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_lophius$acf[2:(n_lag + 1)] # Autocorrelation values for lophius
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



########################### Hake ##


ggscatter(final_df, x = "amo_index", y = "merluccius_merluccius", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_hake <- ccf(final_df$amo_index, final_df$merluccius_merluccius, lag.max = 2, na.action = na.pass)
ccf_hake

# Calculate the correlation coefficient between amo and hake
cor_hake <- cor.test(final_df$amo_index, final_df$merluccius_merluccius, use = "complete.obs")
cor_obs <- cor_hake$estimate # Observed correlation coefficient
p_value <- cor_hake$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_hake <- acf(final_df$merluccius_merluccius, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$merluccius_merluccius))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo
rho_yy <- acf_hake$acf[2:(n_lag + 1)] # Autocorrelation values for hake
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Micromesistius poutassou

ggscatter(final_df, x = "amo_index", y = "micromesistius_poutassou", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_micmpou_2 <- ccf(final_df$amo_index, final_df$micromesistius_poutassou, lag.max = 2, na.action = na.pass)
ccf_micmpou_2

# Calculate the correlation coefficient between amo_index and MICMPOU
cor_micmpou <- cor.test(final_df$amo_index, final_df$micromesistius_poutassou, use = "complete.obs")
cor_obs <- cor_micmpou$estimate # Observed correlation coefficient
p_value <- cor_micmpou$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_micmpou <- acf(final_df$micromesistius_poutassou, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$micromesistius_poutassou))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_micmpou$acf[2:(n_lag + 1)] # Autocorrelation values for MICMPOU
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}




########################### Mugilidae ##


ggscatter(final_df, x = "amo_index", y = "mugilidae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_mugilidae <- ccf(final_df$amo_index, final_df$mugilidae, lag.max = 2, na.action = na.pass)
ccf_mugilidae

# Calculate the correlation coefficient between amo and mugilidae
cor_mugilidae <- cor.test(final_df$amo_index, final_df$mugilidae, use = "complete.obs")
cor_obs <- cor_mugilidae$estimate # Observed correlation coefficient
p_value <- cor_mugilidae$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_mugilidae <- acf(final_df$mugilidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$mugilidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo
rho_yy <- acf_mugilidae$acf[2:(n_lag + 1)] # Autocorrelation values for mugilidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Mullus sp

ggscatter(final_df, x = "amo_index", y = "mullus_spp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_mullus_2 <- ccf(final_df$amo_index, final_df$mullus_spp, lag.max = 2, na.action = na.pass)
ccf_mullus_2

# Calculate the correlation coefficient between amo_index and Mullus spp
cor_mullus <- cor.test(final_df$amo_index, final_df$mullus_spp, use = "complete.obs")
cor_obs <- cor_mullus$estimate # Observed correlation coefficient
p_value <- cor_mullus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_mullus <- acf(final_df$mullus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$mullus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_mullus$acf[2:(n_lag + 1)] # Autocorrelation values for Mullus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}

# Mustelus

ggscatter(final_df, x = "amo_index", y = "mustelus_spp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_mustelus_2 <- ccf(final_df$amo_index, final_df$mustelus_spp, lag.max = 2, na.action = na.pass)
ccf_mustelus_2

# Calculate the correlation coefficient between amo_index and Mustelus
cor_mustelus <- cor.test(final_df$amo_index, final_df$mustelus_spp, use = "complete.obs")
cor_obs <- cor_mustelus$estimate # Observed correlation coefficient
p_value <- cor_mustelus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_mustelus <- acf(final_df$mustelus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$mustelus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_mustelus$acf[2:(n_lag + 1)] # Autocorrelation values for Mustelus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


########################### nephrops norvegicus ##


ggscatter(final_df, x = "amo_index", y = "nephrops_norvegicus", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_nephrops <- ccf(final_df$amo_index, final_df$nephrops_norvegicus, lag.max = 2, na.action = na.pass)
ccf_nephrops


# Calculate the correlation coefficient between mean_sst and nephnor
cor_nephnor <- cor.test(final_df$amo_index, final_df$nephrops_norvegicus, use = "complete.obs")
cor_obs <- cor_nephnor$estimate # Observed correlation coefficient
p_value <- cor_nephnor$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_nephnor <- acf(final_df$nephrops_norvegicus, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$nephrops_norvegicus))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo
rho_yy <- acf_nephnor$acf[2:(n_lag + 1)] # Autocorrelation values for nephnor
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


########################### Octopodidea ##


ggscatter(final_df, x = "amo_index", y = "octopodidea", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_octopus <- ccf(final_df$amo_index, final_df$octopodidea, lag.max = 2, na.action = na.pass)
ccf_octopus

# Calculate the correlation coefficient between amo and octopus
cor_octopus <- cor.test(final_df$amo_index, final_df$octopodidea, use = "complete.obs")
cor_obs <- cor_octopus$estimate # Observed correlation coefficient
p_value <- cor_octopus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_octopus <- acf(final_df$octopodidea, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$octopodidea))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo
rho_yy <- acf_octopus$acf[2:(n_lag + 1)] # Autocorrelation values for octopus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Parapenaeus longirostris

ggscatter(final_df, x = "amo_index", y = "parapenaeus_longirostris", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_papelon_2 <- ccf(final_df$amo_index, final_df$parapenaeus_longirostris, lag.max = 2, na.action = na.pass)
ccf_papelon_2

# Calculate the correlation coefficient between amo_index and papelon
cor_papelon <- cor.test(final_df$amo_index, final_df$parapenaeus_longirostris, use = "complete.obs")
cor_obs <- cor_papelon$estimate # Observed correlation coefficient
p_value <- cor_papelon$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_papelon <- acf(final_df$parapenaeus_longirostris, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$parapenaeus_longirostris))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_papelon$acf[2:(n_lag + 1)] # Autocorrelation values for papelon
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Rajiformes

ggscatter(final_df, x = "amo_index", y = "rajiformes", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_rays_2 <- ccf(final_df$amo_index, final_df$rajiformes, lag.max = 2, na.action = na.pass)
ccf_rays_2

# Calculate the correlation coefficient between amo_index and rajiformes
cor_rays <- cor.test(final_df$amo_index, final_df$rajiformes, use = "complete.obs")
cor_obs <- cor_rays$estimate # Observed correlation coefficient
p_value <- cor_rays$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_rays <- acf(final_df$rajiformes, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$rajiformes))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_rays$acf[2:(n_lag + 1)] # Autocorrelation values for rajiformes
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Sardina pilchardus

ggscatter(final_df, x = "amo_index", y = "sardina_pilchardus", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_sardine <- ccf(final_df$amo_index, final_df$sardina_pilchardus, lag.max = 2, na.action = na.pass)
ccf_sardine

# Calculate the correlation coefficient between amo_index and sardine
cor_sardine <- cor.test(final_df$amo_index, final_df$sardina_pilchardus, use = "complete.obs")
cor_obs <- cor_sardine$estimate # Observed correlation coefficient
p_value <- cor_sardine$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_sardine <- acf(final_df$sardina_pilchardus, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$sardina_pilchardus))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_sardine$acf[2:(n_lag + 1)] # Autocorrelation values for sardine
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Sardinella aurita

ggscatter(final_df, x = "amo_index", y = "sardinella_aurita", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_sardinella_2 <- ccf(final_df$amo_index, final_df$sardinella_aurita, lag.max = 2, na.action = na.pass)
ccf_sardinella_2

# Calculate the correlation coefficient between amo_index and sardinella
cor_sardinella <- cor.test(final_df$amo_index, final_df$sardinella_aurita, use = "complete.obs")
cor_obs <- cor_sardinella$estimate # Observed correlation coefficient
p_value <- cor_sardinella$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_sardinella <- acf(final_df$sardinella_aurita, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$sardinella_aurita))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_sardinella$acf[2:(n_lag + 1)] # Autocorrelation values for S.aurita
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}



# Scomber spp

ggscatter(final_df, x = "amo_index", y = "scomber_spp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_scomber_2 <- ccf(final_df$amo_index, final_df$scomber_spp, lag.max = 2, na.action = na.pass)
ccf_scomber_2

# Calculate the correlation coefficient between amo_index and scomber
cor_scomber <- cor.test(final_df$amo_index, final_df$scomber_spp, use = "complete.obs")
cor_obs <- cor_scomber$estimate # Observed correlation coefficient
p_value <- cor_scomber$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_scomber <- acf(final_df$scomber_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$scomber_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_scomber$acf[2:(n_lag + 1)] # Autocorrelation values for Scomber
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Scophthalmidae

ggscatter(final_df, x = "amo_index", y = "scophthalmidae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_scophthalmidae_2 <- ccf(final_df$amo_index, final_df$scophthalmidae, lag.max = 2, na.action = na.pass)
ccf_scophthalmidae_2

# Calculate the correlation coefficient between amo_index and scophthalmidae
cor_scoph <- cor.test(final_df$amo_index, final_df$scophthalmidae, use = "complete.obs")
cor_obs <- cor_scoph$estimate # Observed correlation coefficient
p_value <- cor_scoph$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_scoph <- acf(final_df$scophthalmidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$scophthalmidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_scoph$acf[2:(n_lag + 1)] # Autocorrelation values for scophthalmidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Scorpaenidae

ggscatter(final_df, x = "amo_index", y = "scorpaenidae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_scorpaenidae_2 <- ccf(final_df$amo_index, final_df$scorpaenidae, lag.max = 2, na.action = na.pass)
ccf_scorpaenidae_2

# Calculate the correlation coefficient between amo_index and scorpaenidae
cor_scorpaenidae <- cor.test(final_df$amo_index, final_df$scorpaenidae, use = "complete.obs")
cor_obs <- cor_scorpaenidae$estimate # Observed correlation coefficient
p_value <- cor_scorpaenidae$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_scorpaenidae <- acf(final_df$scorpaenidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$scorpaenidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_scorpaenidae$acf[2:(n_lag + 1)] # Autocorrelation values for scorpaenidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Sepiidae, Sepiolidae

ggscatter(final_df, x = "amo_index", y = "sepiidae._sepiolidae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_sepia_2 <- ccf(final_df$amo_index, final_df$sepiidae._sepiolidae, lag.max = 2, na.action = na.pass)
ccf_sepia_2

# Calculate the correlation coefficient between amo_index and sepiidae
cor_sepia <- cor.test(final_df$amo_index, final_df$sepiidae._sepiolidae, use = "complete.obs")
cor_obs <- cor_sepia$estimate # Observed correlation coefficient
p_value <- cor_sepia$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_sepia <- acf(final_df$sepiidae._sepiolidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$sepiidae._sepiolidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_sepia$acf[2:(n_lag + 1)] # Autocorrelation values for sepiidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Solea solea

ggscatter(final_df, x = "amo_index", y = "solea_solea", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_sole_2 <- ccf(final_df$amo_index, final_df$solea_solea, lag.max = 2, na.action = na.pass)
ccf_sole_2

# Calculate the correlation coefficient between amo_index and sepiidae
cor_solea <- cor.test(final_df$amo_index, final_df$solea_solea, use = "complete.obs")
cor_obs <- cor_solea$estimate # Observed correlation coefficient
p_value <- cor_solea$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_solea <- acf(final_df$solea_solea, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$solea_solea))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_solea$acf[2:(n_lag + 1)] # Autocorrelation values for solea
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Spicara

ggscatter(final_df, x = "amo_index", y = "spicara_spp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_spicara_2 <- ccf(final_df$amo_index, final_df$spicara_spp, lag.max = 2, na.action = na.pass)
ccf_spicara_2

# Calculate the correlation coefficient between amo_index and spicara
cor_spicara <- cor.test(final_df$amo_index, final_df$spicara_spp, use = "complete.obs")
cor_obs <- cor_spicara$estimate # Observed correlation coefficient
p_value <- cor_spicara$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_spicara <- acf(final_df$spicara_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$spicara_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_spicara$acf[2:(n_lag + 1)] # Autocorrelation values for spicara
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Squalus

ggscatter(final_df, x = "amo_index", y = "squalus_spp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_squalus_2 <- ccf(final_df$amo_index, final_df$squalus_spp, lag.max = 2, na.action = na.pass)
ccf_squalus_2

# Calculate the correlation coefficient between amo_index and squalus
cor_squalus <- cor.test(final_df$amo_index, final_df$squalus_spp, use = "complete.obs")
cor_obs <- cor_squalus$estimate # Observed correlation coefficient
p_value <- cor_squalus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_squalus <- acf(final_df$squalus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$squalus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_squalus$acf[2:(n_lag + 1)] # Autocorrelation values for squalus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}

# Squilla mantis

ggscatter(final_df, x = "amo_index", y = "squilla_mantis", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_squilman <- ccf(final_df$amo_index, final_df$squilla_mantis, lag.max = 2, na.action = na.pass)
ccf_squilman

# Calculate the correlation coefficient between amo_index and squila mantis
cor_squilman <- cor.test(final_df$amo_index, final_df$squilla_mantis, use = "complete.obs")
cor_obs <- cor_squilman$estimate # Observed correlation coefficient
p_value <- cor_squilman$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_squilman <- acf(final_df$squilla_mantis, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$squilla_mantis))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_squilman$acf[2:(n_lag + 1)] # Autocorrelation values for squila mantis
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Trachurus spp

ggscatter(final_df, x = "amo_index", y = "trachurus_spp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_trachurus_2 <- ccf(final_df$amo_index, final_df$trachurus_spp, lag.max = 2, na.action = na.pass)
ccf_trachurus_2

# Calculate the correlation coefficient between amo_index and trachurus
cor_trachurus <- cor.test(final_df$amo_index, final_df$trachurus_spp, use = "complete.obs")
cor_obs <- cor_trachurus$estimate # Observed correlation coefficient
p_value <- cor_trachurus$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_trachurus <- acf(final_df$trachurus_spp, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$trachurus_spp))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_trachurus$acf[2:(n_lag + 1)] # Autocorrelation values for trachurus
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# Triglidae

ggscatter(final_df, x = "amo_index", y = "triglidae", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")


ccf_triglidae_2 <- ccf(final_df$amo_index, final_df$triglidae, lag.max = 2, na.action = na.pass)
ccf_triglidae_2

# Calculate the correlation coefficient between amo_index and triglidae
cor_triglidae <- cor.test(final_df$amo_index, final_df$triglidae, use = "complete.obs")
cor_obs <- cor_triglidae$estimate # Observed correlation coefficient
p_value <- cor_triglidae$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_triglidae <- acf(final_df$triglidae, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$triglidae))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_triglidae$acf[2:(n_lag + 1)] # Autocorrelation values for triglidae
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# PC1
ggscatter(final_df, x = "amo_index", y = "Axis1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_pc1 <- ccf(final_df$amo_index, final_df$Axis1, lag.max = 2, na.action = na.pass)
ccf_pc1

# Calculate the correlation coefficient between amo_index and PC1
cor_pc1 <- cor.test(final_df$amo_index, final_df$Axis1, use = "complete.obs")
cor_obs <- cor_pc1$estimate # Observed correlation coefficient
p_value <- cor_pc1$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_pc1 <- acf(final_df$Axis1, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$pc1))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_pc1$acf[2:(n_lag + 1)] # Autocorrelation values for pc1
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}


# PC2
ggscatter(final_df, x = "amo_index", y = "Axis2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "amo", ylab = "Landings")

ccf_pc2 <- ccf(final_df$amo_index, final_df$Axis2, lag.max = 2, na.action = na.pass)
ccf_pc2

# Calculate the correlation coefficient between amo_index and PC1
cor_pc2 <- cor.test(final_df$amo_index, final_df$Axis2, use = "complete.obs")
cor_obs <- cor_pc2$estimate # Observed correlation coefficient
p_value <- cor_pc2$p.value # P-value

# Calculate the autocorrelation values for each series
acf_amo <- acf(final_df$amo_index, plot = FALSE)
acf_pc2 <- acf(final_df$Axis2, plot = FALSE)

# Estimate the effective degrees of freedom
n <- length(na.omit(cbind(final_df$amo_index, final_df$pc2))) # Number of complete data pairs
n_lag <- min(n/5, length(acf_amo$acf) - 1) # Maximum number of lags as the minimum of n/5 and the length of the acf vector

# Extract the autocorrelation values from lag 1 to lag n_lag
rho_xx <- acf_amo$acf[2:(n_lag + 1)] # Autocorrelation values for amo_index
rho_yy <- acf_pc2$acf[2:(n_lag + 1)] # Autocorrelation values for pc1
n_star <- n/(1 + 2*sum(rho_xx*rho_yy)) # Effective degrees of freedom

# Calculate the critical correlation value
alpha <- 0.05 # Significance level
t_alpha <- qt(alpha/2, n_star - 2, lower.tail = FALSE) # Student's t-distribution
r_crit <- sqrt(t_alpha^2/(n_star - 2 + t_alpha^2)) # Critical correlation value

# Compare the observed and critical values
if (abs(cor_obs) > r_crit) {
  cat("The correlation coefficient is significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
} else {
  cat("The correlation coefficient is not significant at the", alpha, "level.\n")
  cat("The observed value is", cor_obs, "and the critical value is", r_crit, ".\n")
}





# Create a data frame with all the correlation results for AMO
df_amo <- data.frame(
  species = c("Atherinidae", "Boops boops","Lophius spp", "Micromesistius poutassou", "Mugilidae", "Mullus spp", "Mustelus spp", "Parapenaeus longirostris", "Rajiformes", "Sardinella aurita", "Scomber spp", "Scophthalmidae", "Scorpaenidae", "Sepiidae, Sepiolidae", "Spicara spp", "Squalus spp", "Squilla mantis", "Trachurus spp", "Triglidae", "PC1"),
  value = c(-0.73, -0.8, 0.43, -0.69, -0.38, 0.68, -0.51, 0.41, -0.69, -0.7, 0.4, -0.6, -0.58, -0.54,-0.76, -0.56, 0.46, -0.67, 0.67, 0.8)
)
df_amo

# Arrange the data frame by value
df_amo <- df_amo[order(df_amo$value),]

# Change the order of factor levels
df_amo$species <- factor(df_amo$species, levels = df_amo$species)

# Create the horizontal histogram chart with modified x-axis and bar colors
amo_ccf_plot <- ggplot(df_amo, aes(x = value, y = species, fill = value)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  scale_x_continuous(limits = c(-1, 1), expand = c(0,0)) +
  scale_fill_gradient2(low = "#313695", high = "#d73027", mid = "white", midpoint = 0) +
  geom_text(aes(label = round(value, 2), x = ifelse(value > 0, value + 0.1, value - 0.1)), 
            hjust = ifelse(df_amo$value > 0, 0.5, 0.5), 
            color = "black") +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.position = "none") +
  labs(x = "Correlation Coefficient", y = "")

amo_ccf_plot


