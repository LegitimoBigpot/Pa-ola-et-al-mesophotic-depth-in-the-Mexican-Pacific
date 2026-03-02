# Title:  Calculation of berta diversity
# Author: Abigail Pañola-Madrigal
# Date:   09/02/2026

# Instructions for loading packages
# This command allows you to load multiple packages with a single instruction.
if (!require("pacman")) install.packages("pacman")
packages <- c("openxlsx", "betapart")  #"tidyverse", "tools", "magick")
pacman::p_load(char = packages)

# Instructions for setting up the working directory
# This directory must change according to the configuration of the home directory
setwd(dir) # set work directory where the files are included 

# Instructions for uploading the data
data <- read.xlsx(
  "CSR-D-25-00044-SM Tables.xlsx",
  sheet = "SM10",
  startRow = 8,
  cols = 3:8,
  colNames = TRUE,
  rowNames = TRUE) 

# Instructions to remove summary rows
data <- data[1:(nrow(data)-2), ]

# Instruction ensures that the data is numeric.
data <- data %>%
  mutate(across(everything(), as.numeric))

# Instruction to replace NA with 0 (only in numeric abundance columns)
data[is.na(data)] <- 0

#### Calculation of beta diversity by site #### 

# Instructions for converting the database to presence/absence data
beta_sites <- data
beta_sites[beta_sites > 0] <- 1

#Instructions for transposing data
beta_sites <- t(beta_sites)

#BETA CORE
b.core <- betapart.core(beta_sites)

#BETA.MULTI(x, index.family)
# Total dissimilarity between sites and their replacement (Bsim) and nesting (Bsne) components
(b.multi <- beta.multi(b.core))

# BETA.PAIR(x, index.family)
# Calculate the total dissimilarity between the sites and their turnover and nesting components
(b.pair <- beta.pair(beta_sites, "sorensen"))

#### Calculation of beta diversity by zone ####
# Instructions for obtaining shallow and mesophotic data by adding the corresponding sites
shallow <- data$Altura.Baja + data$La.Ahogada
mesophotic <- data$Jade + data$Las.24 + data$Salema

# Instructions for building the matrix by zone
beta_zones <- data.frame(
  Shallow = shallow,
  Mesophotic = mesophotic)

#Instructions for transposing data
beta_zones <- t(beta_zones)

# Instructions for converting the database to presence/absence data
beta_zones[beta_zones > 0] <- 1

#BETA CORE
b.core <- betapart.core(beta_zones)

#BETA.MULTI(x, index.family)
# Total dissimilarity between sites and their replacement (Bsim) and nesting (Bsne) components
(b.multi <- beta.multi(b.core))

# BETA.PAIR(x, index.family)
# Calculate the total dissimilarity between the sites and their turnover and nesting components
(b.pair <- beta.pair(beta_zones, "sorensen"))

#################################### E N D ###########################