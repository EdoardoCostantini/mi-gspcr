# Project:   mi-gspcr
# Objective: Diagnostics on imputed data
# Author:    Edoardo Costantini
# Created:   2023-08-17
# Modified:  2023-08-17
# Notes: 

# Load Packages
source("0-prep-load-packages.R")

# Load variable type map
var_types <- readRDS("../input/var_types.rds")

# MI-GSPCR ---------------------------------------------------------------------

# Read imptued data
mids_migspcr <- readRDS("../output/20230815-221458-mids-mi-gspcr.rds")

# Convergence checks
plot(
    mids_migspcr,
    var_types$ord[1:10]
)

# Density plots
densityplot(
    mids_migspcr,
    ~ v7
)

# Plausible values
stripplot(mids_migspcr, age_r3 ~ .imp, pch = 20, cex = 2)

# MI-EXPERT --------------------------------------------------------------------