# Title:  Calculation of iNext by Chao et. al 2020
# Author: Abigail Pañola-Madrigal
# Date:   09/02/2026

# Instructions for loading packages
# This command allows you to load multiple packages with a single instruction.
if (!require("pacman")) install.packages("pacman")
packages <- c("openxlsx", "iNEXT.4steps", "ggplot2", "tidyverse", "tools", "magick")
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

#### Chao 4 steps for  sites ####
# Instructions for performing 4 steps
site <- iNEXT4steps(data = data,
                    datatype = "abundance",
                    nboot = 150)

# Instructions for saving the file as RData
save(site, file = "Chao_4steps_sites.RData")

# Instructions for loading RData (if it is necessary to rerun the analyses)
load("Chao_4steps_sites.RData")

# Instructions for...
# creating a new folder 
dir.create("Chao_site", showWarnings = FALSE)

# Saving and compressing graphics
for (i in seq_along(site$figure)) {
  file_path <- file.path("Chao_site", paste0("grafico_", i, ".tiff"))
  ggsave(
    filename = file_path,
    plot = site$figure[[i]],
    device = "tiff",
    width = 30,
    height = 20,
    units = "cm",
    dpi = 600,
    bg = "white")
  system(paste0("magick ", shQuote(file_path), " -compress LZW ", shQuote(file_path)))
}

#Instructions for saving table results 
table_sites <- names(site$summary) # Original names
sites_stepts <- gsub("[^[:alnum:]_]", "_", table_sites)  # Names modified by replacing anything that is not a letter/number with "_"

# Instructions: Save each table as CSV
for (i in seq_along(site$summary)) {
  tabla <- site$summary[[i]]
  if (is.data.frame(tabla)) {
    write.csv(
      tabla,
      file = file.path("Chao_site", paste0("resumen_", sites_stepts[i], ".csv")),
      row.names = FALSE)
  }
}

#### Chao 4 steps for  zoones ####
# Instructions for obtaining shallow and mesophotic data by adding the corresponding sites
shallow <- data$Altura.Baja + data$La.Ahogada
mesophotic <- data$Jade + data$Las.24 + data$Salema

# Instructions for building the matrix by zone
data_zones <- data.frame(
  Shallow = shallow,
  Mesophotic = mesophotic)

# Instructions for performing 4 steps
zone <- iNEXT4steps(data = data_zones,
                    datatype = "abundance",
                    nboot = 150)

# Instructions for saving the file as RData
save(zone, file = "Chao_4steps_zones.RData")

# Instructions for loading RData (if it is necessary to rerun the analyses)
load("Chao_4steps_zones.RData")

# Instructions for...
# creating a new folder 
dir.create("Chao_zone", showWarnings = FALSE)

# Saving and compressing graphics
for (i in seq_along(zone$figure)) {
  file_path <- file.path("Chao_zone", paste0("grafico_", i, ".tiff"))
  ggsave(
    filename = file_path,
    plot = zone$figure[[i]],
    device = "tiff",
    width = 30,
    height = 20,
    units = "cm",
    dpi = 600,
    bg = "white")
  system(paste0("magick ", shQuote(file_path), " -compress LZW ", shQuote(file_path)))
}

#Instructions for saving table results 
table_zones <- names(zone$summary) # Original names
zones_stepts <- gsub("[^[:alnum:]_]", "_", table_zones)  # Names modified by replacing anything that is not a letter/number with "_"

# Instructions: Save each table as CSV
for (i in seq_along(zone$summary)) {
  tabla <- zone$summary[[i]]
  if (is.data.frame(tabla)) {
    write.csv(
      tabla,
      file = file.path("Chao_zone", paste0("resumen_", zones_stepts[i], ".csv")),
      row.names = FALSE)
  }
}

#################################### E N D ###########################