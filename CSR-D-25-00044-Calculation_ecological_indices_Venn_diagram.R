# Title:  Calculation of ecological indices and Venn diagram
# Author: Abigail Pañola-Madrigal
# Date:   09/02/2026

# Instructions for loading packages
# This command allows you to load multiple packages with a single instruction.
if (!require("pacman")) install.packages("pacman")
packages <- c("openxlsx", "vegan", "tidyverse", "ggrepel", "ggvenn")
pacman::p_load(char = packages)

# Instructions for setting up the working directory
# This directory must change according to the configuration of the home directory
setwd(dir) # set work directory where the files are included 

# Instructions for uploading the data
data <- read.xlsx(
  "CSR-D-25-00044-SM Tables.xlsx",
  sheet = "SM9",
  startRow = 22,
  detectDates = TRUE)

# Instructions for separating the columns of species and variables
species <- data[, 27:ncol(data)]
variables <- data[, 1:26]

# Instructions for calculating diversity indices
richness <- rowSums(species > 0)                  # Species richness
shannon <- diversity(species, index = "shannon")  # Shannon index
simpson <- diversity(species, index = "simpson")  # Simpson index

# Instructions for adding diversity indices to the original database
data$richness <- richness
data$shannon <- shannon
data$simpson <- simpson

# Instructions for filtering data with richness equal to or less than 2
data2 <- data %>%
  filter(richness >= 2)

# Instructions for omits calculated indices.
filtered_species <- data2[, 27:(ncol(data)-3)]

# Instructions for omits columns where the sum of species is zero.
filtered_species <- filtered_species[, colSums(filtered_species) > 0]

# instructions for a new dataframe of the filtered species, variables, and indices
data_final <- cbind(
  data2[, 1:26], 
  filtered_species,
  data2[, (ncol(data2)-2):ncol(data2)])

# Instructions for separating the variable columns from the new filtered database
variables_2 <- data_final[, 1:26]

#### Instructions for creating a Venn diagram ####
#Instructions for excluding columns where genus and species were not identified
sp <- filtered_species[,-c(11, 23, 27, 33, 39, 46)]

# Instructions for adding the zone to the dataframe of species
sp_shallow <- colSums(sp[data_final$Zone == "Shallow", ] > 0)
sp_mesophotic <- colSums(sp[data_final$Zone == "Mesophotic", ] > 0)
spp <- colSums(sp)

#Instructions for filtering species from zone
spp_shallow <- names(sp_shallow[sp_shallow > 0])
spp_mesophotic <- names(sp_mesophotic[sp_mesophotic > 0])
spp2 <- names(spp)

# Unique and shared species by area
unique_shallow <- setdiff(spp_shallow, spp_mesophotic)         # 16 species
unique_mesophotic <- setdiff(spp_mesophotic, spp_shallow)      # 9 species
shared_species <- intersect(spp_shallow, spp_mesophotic)       # 20 species

# Venn diagram
venn_data <- list(
  Shallow = spp_shallow,
  Mesophotic = spp_mesophotic)

#### plot Venn diagram 
(p <- ggvenn(venn_data,
             fill_color = c("darkred", "darkblue"),
             stroke_color = "transparent",
             stroke_size = 1,
             set_name_size = 12,
             text_size = 10)+
    theme_bw(
      base_size = 30)+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
      strip.background = element_blank()))

# Instructions for saving the plot
tiff("Venn_diagram.tiff",
     width = 30,
     height = 20,
     units = 'cm',
     bg = "white",
     res = 300)
print(p)
dev.off()

# Instructions for reducing image size, only works on Mac
system("magick Venn_diagram.tiff -compress LZW Venn_diagram.tiff")

#################################### E N D ##########################
