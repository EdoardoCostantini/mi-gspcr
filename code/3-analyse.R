# Project:   mi-gspcr
# Objective: Analysis of pooled data
# Author:    Edoardo Costantini
# Created:   2023-08-17
# Modified:  2023-08-28
# Notes: 

# Load Packages
source("0-prep-load-packages.R")

# Load variable type map
var_types <- readRDS("../input/var_types.rds")

# Load data --------------------------------------------------------------------

mids_migspcr <- readRDS("../output/20230817-155605-mids-mi-gspcr.rds")

# Read imptued data
mids_miexpert <- readRDS("../output/20230822-101619-mids-mi-expert.rds")

# Estiamte models --------------------------------------------------------------

# Define model
lm_model_3 <- paste0(model2_amv[1], " ~ ", paste0(model2_amv[-1], collapse = " + "))

# Estimate model based on GSPCR
fits_gspcr <- with(mids_migspcr, lm(as.formula(lm_model_3)))

# Pool GSPCR
pool_gspcr <- pool(fits_gspcr)

# Estimate model based on Expert
fits_expert <- with(mids_miexpert, lm(as.formula(lm_model_3)))

# pool expert
pool_expert <- pool(fits_expert)

# Define what to compare
measures <- c("estimate", "ubar", "b", "t", "riv", "lambda", "fmi")

# Compare one by one
measures_compared <- lapply(measures, function(x) {
    data.frame(
        pool_expert$pooled[, 1:2],
        expert = round(pool_expert$pooled[, x], 5),
        gscpr = round(pool_gspcr$pooled[, x], 5)
    )
})

# Give meaningful names
names(measures_compared) <- measures