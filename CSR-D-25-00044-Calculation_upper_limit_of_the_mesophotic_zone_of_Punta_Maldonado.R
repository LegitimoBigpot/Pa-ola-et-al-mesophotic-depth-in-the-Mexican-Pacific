# Title:Calculation of the upper limit of the mesophotic zone at Punta Maldonado
# Author: Abigail Pañola-Madrigal
# Date: 09/02/2026

# Instructions for loading packages
# This command allows you to load multiple packages with a single instruction.
if (!require("pacman")) install.packages("pacman")
packages <- c ("STAT", "openxlsx", "tidyverse", "lubridate")
pacman::p_load(char = packages)

# Instructions for setting up the working directory
# This directory must change according to the configuration of the home directory
setwd(dir) # set work directory where the files are included 

# Instructions for uploading the data
data <- read.xlsx(
  "CSR-D-25-00044-SM Tables.xlsx",
  sheet = "SM3",
  startRow = 15,
  detectDates = TRUE)

# Instructions for preparing the data
data <- data %>%
  mutate(
    hour  = convertToDateTime(hour),    # This instruction sets the time from GMT to Pacific Time.
    light = as.numeric(RUI),            # This instruction ensures that the light values are numerical.
    depth = abs(as.numeric(depth)))     # This instruction ensures that the depth values are numeric and positive.

# __________________________________________________________________________________________ #
##### CALCULATIONS OF THE UPPER LIMIT ESTIMATE OF THE MESOPHOTIC ZONE AT PUNTA MALDONADO #####
# __________________________________________________________________________________________ #

#Instructions for creating a subset with the data from Punta Maldonado
data <- subset (data, region == "PMal")

##### RELATIVE IRRADIANCE #####
# Instructions for calculating surface irradiance (I0).
# This calculation was performed by obtaining the maximum surface irradiance one minute before and one minute after each dive.
I0 <- data %>%
  group_by(site) %>%
  summarise(
    I0 = max(light, na.rm = TRUE),
    .groups = "drop")

# Instructions for calculating relative irradiance (Irel)
# Formula: Irel = IZ/IO
# Irel: Relative irradiance
# IZ: Irradiance at depth Z
# I0: Surface irradiance (calculated in the previous step)
data <- data %>%
  left_join(I0, by = c("site")) %>%
  mutate(
    Irel = light / I0)              # This command calculates relative irradiance
data$Irel.porc <- (data$Irel*100)   # This command converts relative irradiance to a percentage.

# This instruction retrieves the maximum depth observed at each location.
max_depth <- data %>%
  group_by(site) %>%
  summarise(
    max_depth = max(depth, na.rm = TRUE),
    .groups = "drop")

###### APPARENT VERTICAL ATTENUATION #####
# Assuming exponential decay, the vertical attenuation is calculated.
# Formula: Kd = -ln(Irel)/Z
# Kd: Apparent vertical attenuation coefficient
# Irel: Relative irradiance (calculated in the previous step)
# Z: Depth

#This instruction obtains the irradiance relative to the maximum observed depth.
# In summary, it returns the region, the maximum depth, the irradiance relative to the maximum depth,
# the standard deviation of the calculated relative irradiance,
# the percentage of the calculated relative irradiance,
# and the standard deviation of the percentage of the calculated relative irradiance.
Irel_max_depth <- data %>%
  group_by(site) %>%
  slice_max(depth, n = 1, with_ties = TRUE) %>%
  mutate(Irel_pct = Irel * 100) %>%
  summarise(
    max_depth_obs = max(depth, na.rm = TRUE),
    Irel_depth = mean(Irel, na.rm = TRUE),            # mean value
    Irel_sd = sd(Irel, na.rm = TRUE),                 # standard deviation
    Irel_depth_pct = mean(Irel_pct, na.rm = TRUE),    # mean value
    Irel_sd_pct = sd(Irel_pct, na.rm = TRUE),         # standard deviation
    .groups = "drop")

# Instructions for calculating the apparent vertical attenuation coefficient (Kd)
Kd <- data %>%
  filter(depth > 0, Irel > 0) %>%
  group_by(site) %>%
  summarise(
    Kd = -mean(log(Irel) / depth, na.rm = TRUE),
    .groups = "drop")

# Calculation of Z10%
# This instruction estimates Z10% (i.e., 10% of the surface irradiance) from the TDR-MK9 Tag
z10_tag <- Kd %>%
  mutate(z10_tag = log(10)/Kd)

##### KdPAR CALCULATION #####
# Instructions for uploading the data
Kd490 <- read.xlsx(
  "CSR-D-25-00044-SM Tables.xlsx",
  sheet = "SM4",
  startRow = 13,
  na.strings = TRUE,
  colNames = TRUE,
  rowNames = FALSE,
  detectDates = TRUE)

#Instructions for creating a subset with the data from Punta Maldonado
Kd490 <- subset (Kd490, region == "PMal")

# KdPAR calculation
# Instructions for calculating KdPAR based on the model proposed by Morel et al 2007
# Formula: KdPAR = 0.0864 + 0.884Kd490 - 0.00137Kd490^(-1)
Kd490$KdPAR <- (0.0864 + (0.884 * Kd490$kd490) - (0.00137 * (Kd490$kd490^-1)))

# Instruction to obtain specific KdPAR values
# This instruction summarizes the KdPAR calculation and returns the maximum (max),
# minimum (min), average (mean), and standard deviation (sd) of the KdPAR data.
Kd_PAR <- Kd490 %>%
  group_by(site) %>%
  summarise(
    KdPAR_max = max(KdPAR, na.rm = TRUE),      # maximum value
    KdPAR_min = min(KdPAR, na.rm = TRUE),      # minimum value
    KdPAR_mean = mean(KdPAR, na.rm = TRUE),    # mean value
    KdPAR_sd = sd(KdPAR, na.rm = TRUE),        # standard deviation
    .groups = "drop")

# Instructions for calculating Z10% (i.e., 10% of the surface irradiance) from the KdPAR values obtained
z10_KdPAR <- Kd490 %>%
  mutate(z10 = 2.3 / KdPAR) %>%
  group_by(site) %>%
  summarise(
    max_z10  = max(z10, na.rm = TRUE),    # maximum value
    min_z10  = min(z10, na.rm = TRUE),    # minimum value
    prom_z10 = mean(z10, na.rm = TRUE),   # mean value
    sd_z10   = sd(z10, na.rm = TRUE),     # standard deviation
    .groups = "drop")

# Instructions for obtaining 10% of the surface irradiance from kdpar data (maximum, minimum, and mean)
satellite_Irel <- Kd_PAR %>%
  left_join(Irel_max_depth %>% select(site, max_depth_obs), by = "site") %>%
  mutate(
    Irel_sat_mean = 100 * exp(-KdPAR_mean * max_depth_obs),    # mean value
    Irel_sat_min  = 100 * exp(-KdPAR_max  * max_depth_obs),    # maximum value
    Irel_sat_max  = 100 * exp(-KdPAR_min  * max_depth_obs)     # minimum value
  ) %>%
  select(site, Irel_sat_mean, Irel_sat_min, Irel_sat_max)

# Results
# This instruction summarizes the results into a single data frame and saves it to an Excel sheet.
results_kd <- Kd_PAR %>%                          # ata frame Kd_PAR
  left_join(I0, by = c("site" )) %>%              # Data frame I0
  left_join(z10_KdPAR, by = "site") %>%           # Data frame z10_KdPAR
  left_join(z10_tag, by = "site") %>%             # Data frame z10_tag
  left_join(Irel_max_depth, by = "site") %>%      # Data frame Irel_max_depth
  left_join(satellite_Irel, by = "site") %>%      # Data frame satellite_Irel
  select(
    Site = site,
    Surface_Irradiance = I0,
    Maximum_observed_depth = max_depth_obs,
    Irradiance_relative_to_maximum_depth = Irel_depth,
    Percentage_of_relative_irradiance_at_maximum_depth = Irel_depth_pct,
    Kd_value_obtained_from_Tag = Kd,
    Z10_from_Kd_from_Tag = z10_tag,
    Maximum_KdPAR_from_Kd490 = KdPAR_max,
    Mean_KdPAR_from_Kd490 = KdPAR_mean,
    Minimum_KdPAR_from_Kd490 = KdPAR_min,
    Maximum_Z10_from_KdPAR = max_z10,
    Mean_Z10_from_KdPAR = prom_z10,
    Minimum_Z10_from_KdPAR = min_z10,
    Standard_Deviation_from_KdPAR = sd_z10,
    Maximun_Z10_Percentage = Irel_sat_max,
    Mean_Z10_Percentage = Irel_sat_mean,
    Minimum_Z10_Percentage = Irel_sat_min)

# Instructions for saving the new data frame to an Excel file
write.xlsx(results_kd, "Results_upper_limit_of_the_mesophotic_zone_of_Punta_Maldonado.xlsx", overwrite = TRUE)

#################################### E N D ###########################