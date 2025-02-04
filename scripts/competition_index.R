################################################################################
########################## Competition Indices #################################
################################################################################

# Last update - Josh: 02/04/2025

# Libraries
source("scripts/libraries.R")
# functions
source("scripts/functions.R")

# Data prepped for competition indexes
load("data/growth_initial_prep/data_growth.RData")

################################################################################
# Compute Hegyi's Index using siplab
################################################################################

# Three Census Periods
mrs1_comp <- compute_comp_ci(data_growth, plot_name = "MRS1", size_threshold = 10)
bw2_comp <- compute_comp_ci(data_growth, plot_name = "BW2", size_threshold = 10)
bw3_comp <- compute_comp_ci(data_growth, plot_name = "BW3", size_threshold = 10)
mrs4_comp <- compute_comp_ci(data_growth, plot_name = "MRS4", size_threshold = 10)
mrs5_comp <- compute_comp_ci(data_growth, plot_name = "MRS5", size_threshold = 10)
bl6_comp <- compute_comp_ci(data_growth, plot_name = "BL6", size_threshold = 10)
mrs7_comp <- compute_comp_ci(data_growth, plot_name = "MRS7", size_threshold = 10)
mrs11_comp <- compute_comp_ci(data_growth, plot_name = "MRS11", size_threshold = 10)
mrs12_comp <- compute_comp_ci(data_growth, plot_name = "MRS12", size_threshold = 10)
mrs13_comp <- compute_comp_ci(data_growth, plot_name = "MRS13", size_threshold = 10)

# Combine
growth_comp <- rbind(mrs1_comp,
                     bw2_comp,
                     bw3_comp,
                     mrs4_comp, 
                     mrs5_comp,
                     bl6_comp,
                     mrs7_comp,
                     mrs11_comp,
                     mrs12_comp,
                     mrs13_comp)

# save
save(growth_comp, file = "data/comp/growth_comp.RData")
