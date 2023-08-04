# Project:   mi-gspcr-evs
# Objective: Impute prepared EVS data with Mi-GSPCR
# Author:    Edoardo Costantini
# Created:   2023-07-12
# Modified:  2023-08-04
# Notes: 

# Load data --------------------------------------------------------------------

EVS <- readRDS("../input/ZA7500_processed.rds")
# Store session info -----------------------------------------------------------

# Define run date
date_time <- format(Sys.time(), "%Y%m%d-%H%M%S")

# Store info about the R session
R_session <- utils::sessionInfo()

# Save the file in the input folder
saveRDS(R_session, paste0("../input/R-session-", date_time, ".rds"))


# Imputation -------------------------------------------------------------------

# Impute
mids_mi_gspcr <- mice(
    data = EVS_small,
    m = 5,
    maxit = 5,
    method = "gspcr.polyreg"
)

# Save mids object
EVS_small <- saveRDS(
    object = mids_mi_gspcr,
    file = "../output/mids_mi_gspcr.rds"
)