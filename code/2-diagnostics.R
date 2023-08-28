# Project:   mi-gspcr
# Objective: Diagnostics on imputed data
# Author:    Edoardo Costantini
# Created:   2023-08-17
# Modified:  2023-08-28
# Notes: 

# Load Packages
source("0-prep-load-packages.R")

# Load variable type map
var_types <- readRDS("../input/var_types.rds")

# Methods
meths <- readRDS("../input/mi-model-forms.rds")

# MI-GSPCR ---------------------------------------------------------------------

# Read imptued data
mids_migspcr <- readRDS("../output/20230817-155605-mids-mi-gspcr.rds")

# Convergence checks
plot(
    mids_migspcr,
    var_types$ord[1:10],
    layout = c(2, 5)
)

# Density plots
densityplot(
    mids_migspcr,
    ~ v174_LR | .imp
)

# Plausible values
stripplot(mids_migspcr, age_r3 ~ .imp, pch = 20, cex = 2)

# MI-EXPERT --------------------------------------------------------------------

# Read imptued data
mids_miexpert <- readRDS("../output/20230822-101619-mids-mi-expert.rds")

# Convergence checks
plot(
    mids_miexpert,
    var_types$ord[1:10]
)

# Density plots
densityplot(
    mids_miexpert,
    ~ v174_LR | .imp
)

# Plausible values
stripplot(mids_miexpert, v279d_r ~ .imp, pch = 20, cex = 2)