# Project:   mi-gspcr-evs
# Objective: Impute prepared EVS data with MI-Expert imputaiton
# Author:    Edoardo Costantini
# Created:   2023-08-08
# Modified:  2023-08-22
# Notes: 

# Prepare environment ----------------------------------------------------------

# Load Packages
source("0-prep-load-packages.R")

# Load EVS data
EVS <- readRDS("../input/ZA7500_processed.rds")
EVS <- readRDS("../input/ZA7500_fc_processed.rds") # smaller version

# Load variable types
var_types <- readRDS("../input/var_types.rds")

# Load imputation methods vector
meth <- readRDS("../input/mi-model-forms.rds")

# Load imputation methods vector
predMat <- readRDS("../input/mi-model-expert-predMat.rds")

# Ad-hoc data prep -------------------------------------------------------------

# Transform ordinal variables with more than 5 categories to numeric for PCA
for (j in var_types$ord) {
    if (nlevels(EVS[, j]) >= 5) {
        EVS[, j] <- as.numeric(EVS[, j])
    }
}

# Replace v279d_r variable with its log
plot(density(na.omit(EVS$v279d_r)))
EVS$v279d_r[!is.na(EVS$v279d_r)] <- log(EVS$v279d_r[!is.na(EVS$v279d_r)])
EVS$v279d_r <- EVS$v279d_r

# Replace v242 variable with its log
plot(density(na.omit(EVS$v242)))
EVS$v242[!is.na(EVS$v242)] <- log(EVS$v242[!is.na(EVS$v242)])

# Store session info -----------------------------------------------------------

# Define run date
date_time <- format(Sys.time(), "%Y%m%d-%H%M%S")

# Store info about the R session
R_session <- utils::sessionInfo()

# Save the file in the input folder
saveRDS(R_session, paste0("../input/", date_time, "-R-session.rds"))

# Imputation -------------------------------------------------------------------

# Sequential MICE --------------------------------------------------------------
mids_mi_expert <- mice(
    data = EVS,
    m = 1,
    maxit = 20,
    method = meth,
    predictorMatrix = predMat,
    seed = 20230822
)

# Continue an interrupted sequential mice
mids_mi_expert_cont <- mice.mids(mids_mi_expert, maxit = 30)

# Parallel MICE ----------------------------------------------------------------

# Parellel MICE run
mids_mi_expert <- futuremice(
    # Parallel specific arguments
    parallelseed = 20230822,
    n.core = 5,
    # General mice arguments
    data = EVS,
    m = 5,
    maxit = 20,
    predictorMatrix = predMat,
    method = meth
)

# Save mids object
saveRDS(
    object = mids_mi_expert,
    file = paste0("../output/", date_time, "-mids-mi-expert.rds")
)

# Make a meaningful covergence plot
plot(mids_mi_expert)