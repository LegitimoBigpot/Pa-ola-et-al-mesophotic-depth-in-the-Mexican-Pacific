# Title:  Calculation of DistML and dbRDA
# Author: Abigail Pañola-Madrigal
# Date:   09/02/2026

# Instructions for loading packages
# This command allows you to load multiple packages with a single instruction.
if (!require("pacman")) install.packages("pacman")
packages <- c("openxlsx","vegan", "tidyverse", "ggplot2", "ggnewscale", "RColorBrewer")
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

#Instructions for variables
env <- data[, 1:20]             # Instructions for separating variables from species
rownames(env) <- env$ID         # Instructions for naming columns with IDs

# Instructions for species
species <- data[, 21:ncol(data)]      # Instructions for separating species from variables
rownames(species) <- data$ID          # Instructions for naming columns with IDs

# Instructions for normalizing square root species
species.sqrt <- sqrt(species)

#### Distance-based linear model (DistLM) ####
# Instructions for obtaining group models and combined models
# Full model (Geo + Env + Hab)
(mod <- capscale(species.sqrt ~ 
                   Lat+ Lon + Depth +                                             # Geospatial variables
                   Temp + KdPAR + Salinity + OD +                                 # Environmental variables
                   Sand + Gravel + Block + Rock + Algaes + Sponges + Ascidians,   # Habitat variables
                 data = env, distance = "bray"))

# Instructions for obtaining a collinearity diagnosis (VIF)
vif.cca(mod)                          # Collinearity is observed between Lat, Lon, Depth, Temp, Salinity and OD

# Models excluding LAT, OD and SALINITY
(mod_reduced <- capscale( species.sqrt ~ 
                            Lat + Depth +                                                 # Geospatial variables
                            Temp + KdPAR +                                                # Environmental variables
                            Sand + Gravel + Block + Rock + Algaes + Sponges + Ascidians,  # Habitat variables
  data = env, distance = "bray"))

# Instructions for obtaining a collinearity diagnosis (VIF)
(vif.cca(mod_reduced))

# Instructions for validating the reduced model
(anova(mod_reduced, permutations = 9999))
(RsquareAdj(mod_reduced))

#  Instructions for geospatial model
(mod_geo <- capscale(species.sqrt ~ 
                      Lat + Depth, 
                    data = env, distance = "bray"))

# Instructions for environmental model
(mod_env <- capscale(species.sqrt ~ 
                      Temp + KdPAR,
                    data = env, distance = "bray"))

# Habitat model instructions
(mod_hab <- capscale(species.sqrt ~ 
                      Sand + Gravel + Block + Rock + Algaes + Sponges + Ascidians,
  data = env, distance = "bray"))

# Instructions for combined geospatial/environmental model
(mod_geo_env <- capscale(species.sqrt ~
                           Lat + Depth +                   # Geospatial variables
                           Temp + KdPAR,                   # Environmental variables
                         data = env,distance = "bray"))

# Instructions for combined geospatial/habitat model
(mod_geo_hab <- capscale( species.sqrt ~
                            Lat + Depth +                                                    # Geospatial variables
                            Sand + Gravel + Block + Rock + Algaes + Sponges + Ascidians,     # Habitat variables
                          data = env, distance = "bray"))

# Instructions for combined environmental/habitat model
(mod_env_hab <- capscale(species.sqrt ~ 
                           Temp + KdPAR +                                                    # Environmental variables
                           Sand + Gravel + Block + Rock + Algaes + Sponges + Ascidians,      # Habitat variables
                         data = env,  distance = "bray"))

# Matrices for models
geospacial <- env[, c("Lat", "Depth")]
enviromental <- env[, c("Temp", "KdPAR")]
habitat <- env[, c("Sand", "Gravel", "Block", "Rock", "Algaes", "Sponges", "Ascidians")]

# Variance
(var_model <- varpart( species.sqrt, geospacial, enviromental, habitat))

# Partitioning of the environmental model
(mod_env_pure <- capscale(species.sqrt ~
                            Temp + KdPAR +
                            Condition(Lat + Depth + Sand + Gravel + Block + Rock + Algaes + Sponges + Ascidians),
                          data = env, distance = "bray"))

# Instructions for obtaining p-value and adjusted r2
c(R2_adj = RsquareAdj(mod_env_pure)$adj.r.squared,
  p = anova(mod_env_pure)$`Pr(>F)`[1])

# Instructions for validating the model
(anova(mod_env_pure, permutations = 9999))

# Partitioning of the environmental model
(mod_geo_pure <- capscale(species.sqrt ~
                            Lat + Depth +
                            Condition(Temp + KdPAR + Sand + Gravel + Block + Rock + Algaes + Sponges + Ascidians),
                          data = env, distance = "bray"))

# Instructions for obtaining p-value and adjusted r2
c(R2_adj = RsquareAdj(mod_geo_pure)$adj.r.squared,
  p = anova(mod_geo_pure)$`Pr(>F)`[1])

# Instructions for validating the model
(anova(mod_geo_pure, permutations = 9999))

# Partitioning of the habitat model
(mod_hab_pure <- capscale(species.sqrt ~
                            Sand + Gravel + Block + Rock + Algaes + Sponges + Ascidians +
                            Condition(Lat + Depth + Temp +  KdPAR),
                          data = env, distance = "bray"))

# Instructions for obtaining p-value and adjusted r2
c(R2_adj = RsquareAdj(mod_hab_pure)$adj.r.squared,
  p = anova(mod_hab_pure)$`Pr(>F)`[1])

# Instructions for validating the model
(anova(mod_hab_pure, permutations = 9999))

# Instructions for extracting results from models
extract_model <- function(model) {
  c(
    R2_adj = RsquareAdj(model)$adj.r.squared,
    p = anova(model)$`Pr(>F)`[1])
}
(results <- rbind(
  Geospace = extract_model(mod_geo),
  Environment = extract_model(mod_env),
  Habitat = extract_model(mod_hab),
  Geospace_Environment = extract_model(mod_geo_env),
  Geospace_Habitat = extract_model(mod_geo_hab),
  Environment_Habitat = extract_model(mod_env_hab),
  Model_Full = extract_model(mod_reduced),
  Space_pure = extract_model(mod_geo_pure),
  Env_pure = extract_model(mod_env_pure),
  Habitat_pure = extract_model(mod_hab_pure)))

#### Distance-based Redundancy Analysis (dbRDA) ####
# Eigenvalues restringidos
eig_constrained <- eigenvals(mod_reduced, model = "constrained")

# Total inertia
total_inertia <- mod_reduced$tot.chi

# % of fitted
cap1_fitted <- round(eig_constrained[1] / sum(eig_constrained) * 100, 1)
cap2_fitted <- round(eig_constrained[2] / sum(eig_constrained) * 100, 1)

# % of total variation
cap1_total <- round(eig_constrained[1] / total_inertia * 100, 1)
cap2_total <- round(eig_constrained[2] / total_inertia * 100, 1)

# Scores
site_scores <- scores(mod_reduced, display = "sites", scaling = 2)
env_scores  <- scores(mod_reduced, display = "bp", scaling = 2)

sites_df <- as.data.frame(site_scores)
sites_df$Site <- rownames(sites_df)
sites_df$Zone <- as.factor(env[rownames(sites_df), "Zone"])

env_df <- as.data.frame(env_scores)
env_df$Variable <- rownames(env_df)

# Instructions for scaling vectors
mult <- 1.3
env_df$CAP1 <- env_df$CAP1 * mult
env_df$CAP2 <- env_df$CAP2 * mult

# plot
  (p <- ggplot() +
      
      # Sites by zone
      geom_point(data = sites_df,
               aes(x = CAP1,
                   y = CAP2,
                   color = Zone),
               size = 3,
               alpha = 0.85) +
      scale_color_manual(values = c(
        "Shallow" = "darkblue",
        "Mesophotic" = "darkred")) +
      new_scale_color() +
      
      # Vectors by variable
      geom_segment(data = env_df,
                 aes(x = 0, y = 0,
                     xend = CAP1,
                     yend = CAP2,
                     color = Variable),
                 arrow = arrow(length = unit(0.3, "cm")),
                 linewidth = 1) +
      geom_text(data = env_df,
              aes(x = CAP1,
                  y = CAP2,
                  label = Variable,
                  color = Variable),
              size = 5,
              fontface = "bold",
              vjust = -0.6) +
      scale_color_brewer(palette = "Paired", name = "Variable") +
      labs(
        x = paste0("dbRDA1 (", cap1_fitted,
                 "% of fitted, ",
                 cap1_total,
                 "% of total variation)"),
        y = paste0("dbRDA2 (", cap2_fitted,
                 "% of fitted, ",
                 cap2_total,
                 "% of total variation)")) +
      theme_bw(base_size = 18) +
      theme(
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 1, color = "black")))

# Instructions for saving the plot
tiff("dbRDA.tiff",
     width = 30,
     height = 20,
     units = 'cm',
     bg = "white",
     res = 300)
print(p)
dev.off()

# Instructions for reducing image size, only works on Mac
system("magick dbRDA.tiff -compress LZW dbRDA.tiff")

#################################### E N D ###########################