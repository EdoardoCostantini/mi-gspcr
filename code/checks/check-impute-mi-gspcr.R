# Project:   mi-gspcr-evs
# Objective: Check MI-GSPCR works all types of data
# Author:    Edoardo Costantini
# Created:   2023-07-20
# Modified:  2023-07-20
# Notes: 

# Load data --------------------------------------------------------------------

EVS <- readRDS("../input/ZA7500_processed.rds")

# Continuous data --------------------------------------------------------------

# Create continuous data
EVS_numeric <- sapply(EVS[, 3:9], as.numeric)

# Try impute
mids_cont <- mice(
    data = EVS_numeric,
    m = 5,
    maxit = 100,
    method = "gspcr.norm"
)

# Check trace plots
plot(mids_cont)

# Continuous data with threshold -----------------------------------------------

# Binary data ------------------------------------------------------------------

# Ordinal data -----------------------------------------------------------------

# Define columns for ordinal data
var_ord <- 3:9

# Try impute
mids_ord <- mice(
    data = EVS[, var_ord],
    m = 5,
    maxit = 5,
    method = "gspcr.polyreg"
)

# Check traceplots
plot(mids_ord)

# Categorical data -------------------------------------------------------------

# Poisson data -----------------------------------------------------------------

# Mixed data -------------------------------------------------------------------