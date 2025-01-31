################################################################################
########################## Data Manipulation & Prep ############################
################################################################################

# Last update - Josh: 01/31/2025

# Libraries
source("scripts/libraries.R")

# Data
plots <- read.csv("data/growth_raw/perm_plots_all.csv")

# View
names(plots)
head(plots)
table(plots$Plot)

# Subset to only those with spatial data
plots_spatial <- plots %>% 
  subset(Plot %in% c("MRS1", "BW2", "BW3", "MRS4", "MRS5", "BL6",
                     "MRS7", "MRS11", "MRS12", "MRS13"))


