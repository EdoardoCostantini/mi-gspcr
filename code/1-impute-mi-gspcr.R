# Project:   mi-gspcr-evs
# Objective: Impute prepared EVS data with Mi-GSPCR
# Author:    Edoardo Costantini
# Created:   2023-07-12
# Modified:  2023-08-17
# Notes: 

# Prepare environment ----------------------------------------------------------

# Load Packages
source("0-prep-load-packages.R")

# Load EVS data
EVS <- readRDS("../input/ZA7500_fc_processed.rds") # smaller version
EVS <- readRDS("../input/ZA7500_processed.rds")

# Prepare Western European countries for ImmerzeelEtAl2016
EVS <- EVS %>%
    filter(
        country %in% c(
            "Austria",
            "Belgium",
            "Denmark",
            "Finland",
            "France",
            "Germany",
            "Greece",
            "Italy",
            "Luxembourg",
            "Netherlands",
            "Norway",
            "Swizerland"
        )
    )

# Load variable types
var_types <- readRDS("../input/var_types.rds")

# Load imputation methods vector
meths <- readRDS("../input/mi-model-forms.rds")

# Attach GSPCR tag for mi-gscpr imputation
meths <- paste0("gspcr.", meths)

# Ad-hoc data prep -------------------------------------------------------------

# Transform ordinal variables with more than 5 categories to numeric for PCA
for(j in var_types$ord){
    if(nlevels(EVS[, j]) >= 5){
        EVS[, j] <- as.numeric(EVS[, j])
    }
}

# Replace v279d_r variable with its log
plot(density(na.omit(EVS$v279d_r)))
EVS$v279d_r[!is.na(EVS$v279d_r)] <- log(EVS$v279d_r[!is.na(EVS$v279d_r)])

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

# Sequential MICE ----------------------------------------------------------------

# Sequential MICE run
mids_mi_gspcr <- mice(
    # General mice arguments
    data = EVS,
    m = 5,
    maxit = 20,
    method = meths,
    ridge = 0,
    eps = 0, # bypasses remove.lindep()
    threshold = 1L,
    seed = 20230804,
    # GSPCR specific arguments
    thrs = "PR2",
    fit_measure = "BIC",
    nthrs = 10,
    npcs_range = 1:5,
    K = 1
)

# Continue an interrupted sequential mice
mids_mi_gspcr_cont <- mice.mids(mids_mi_gspcr, maxit = 30)

# Parallel MICE ----------------------------------------------------------------

# Parellel MICE run
mids_mi_gspcr <- futuremice(
    # Parallel specific arguments
    parallelseed = 20230804,
    n.core = 5,
    # General mice arguments
    data = EVS,
    m = 5,
    maxit = 20,
    method = meths,
    ridge = 0,
    eps = 0, # bypasses remove.lindep()
    threshold = 1L,
    # GSPCR specific arguments
    thrs = "PR2",
    fit_measure = "BIC",
    nthrs = 10,
    npcs_range = 1:5,
    K = 1
)

# Save mids object
saveRDS(
    object = mids_mi_gspcr,
    file = paste0("../output/", date_time, "-mids-mi-gspcr.rds")
)

# Make a meaningful covergence plot
plot(imp1)