# Project:   mi-gspcr-evs
# Objective: Check MI-GSPCR works all types of data
# Author:    Edoardo Costantini
# Created:   2023-07-20
# Modified:  2023-08-04
# Notes: 

# Load data --------------------------------------------------------------------

EVS_small <- readRDS("../input/ZA7500_fc_processed.rds")
EVS <- readRDS("../input/ZA7500_processed.rds")

# Load variable types
var_types <- readRDS("../input/var_types.rds")

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

# Binary data ------------------------------------------------------------------

# Subset a few binary and ordinal variables
EVS_bin <- EVS_small[, c(var_types$bin)]

# Create a method vector
methods <- rep("gspcr.logreg", ncol(EVS_bin))

# Try impute
mids_bin <- mice(
    data = EVS_bin,
    m = 5,
    maxit = 5,
    ridge = 0, 
    eps = 0, # bypasses remove.lindep()
    threshold = 1L,
    method = methods,
    seed = 20230801
)

# Ordinal data -----------------------------------------------------------------

# Subset a few binary and ordinal variables
EVS_ord <- EVS_small[, c(var_types$ord)]

# Create a method vector
methods_polr <- rep("gspcr.polr", ncol(EVS_ord))

# Try impute with polr
mids_ord <- mice(
    data = EVS_ord,
    m = 5,
    maxit = 5,
    ridge = 0, 
    eps = 0, # bypasses remove.lindep()
    threshold = 1L,
    method = methods_polr,
    seed = 20230801
)

# Ordinal data with pmm --------------------------------------------------------

# Transform ordinal variables with more than 5 categories to numeric for PCA
for (j in var_types$ord) {
    if (nlevels(EVS_ord[, j]) >= 5) {
        EVS_ord[, j] <- as.numeric(EVS_ord[, j])
    }
}

# Create a method vector
methods_pmm <- rep("gspcr.pmm", ncol(EVS_ord))

# Try impute with pmm
mids_ord <- mice(
    data = EVS_ord,
    m = 5,
    maxit = 5,
    ridge = 0, 
    eps = 0, # bypasses remove.lindep()
    threshold = 1L,
    method = methods_pmm,
    seed = 20230802
)

# Check traceplots
plot(mids_ord)

# Impute many chains to check the convergence
mids_ord_chians <- futuremice(
    parallelseed = 20230802,
    n.core = 5,
    data = EVS_ord,
    m = 5,
    maxit = 50,
    ridge = 0,
    eps = 0, # bypasses remove.lindep()
    threshold = 1L,
    method = methods_pmm,
    thrs = "PR2",
    fit_measure = "BIC",
    nthrs = 10,
    npcs_range = 1:5,
    K = 1,
    donors = 5L
)

# Look at traceplots
plot(mids_ord_chians)

# Categorical data -------------------------------------------------------------

# Subset a few binary and ordinal variables
EVS_cat <- EVS_small[, c(var_types$cat)]

# Create a method vector
methods_polyreg <- rep("gspcr.polyreg", ncol(EVS_cat))

# Try impute with polr
mids_cat <- mice(
    data = EVS_cat,
    m = 5,
    maxit = 5,
    ridge = 0, 
    eps = 0, # bypasses remove.lindep()
    threshold = 1L,
    method = methods_polyreg,
    seed = 20230801
)

# Could look at traceplots even for this
plot(mids_ord)

# All data ---------------------------------------------------------------------

# Load imputation methods vector
imp_meth_vec <- readRDS("../input/imputation_methods-mi-gspcr.rds")

# Impute
mids_mi_gspcr <- mice(
    data = EVS_small,
    m = 5,
    maxit = 5,
    method = imp_meth_vec,
    ridge = 0, 
    eps = 0, # bypasses remove.lindep()
    threshold = 1L,
    seed = 20230801
)

# Parallel MICE ----------------------------------------------------------------

imp1 <- futuremice(
    data = EVS_numeric,
    m = 2,
    n.core = 2,
    maxit = 5,
    method = "gspcr.norm",
    ridge = 0,
    eps = 0, # bypasses remove.lindep()
    parallelseed = 20230802,
    threshold = 1L
)

plot(imp1)