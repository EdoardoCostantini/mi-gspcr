# Project:   mi-gspcr-evs
# Objective: Check gspcr works on EVS data
# Author:    Edoardo Costantini
# Created:   2023-07-20
# Modified:  2023-07-20
# Notes: 

# Load data --------------------------------------------------------------------

EVS <- readRDS("../input/ZA7500_processed.rds")

# Work only with some variables
EVS_gspcr <- EVS[, 1:20]

# Complete cases
EVS_gspcr <- EVS_gspcr[rowSums(is.na(EVS_gspcr)) == 0, ]

# Confirm no missing values
md.pattern(EVS_gspcr)

# Train GSPCR ------------------------------------------------------------------

# Use GSPCR on CC data
out <- cv_gspcr(
    dv = EVS_gspcr[, 3],
    ivs = EVS_gspcr[, -3],
    fam = "cumulative",
    nthrs = 20,
    npcs_range = 1:10,
    K = 1,
    thrs = "PR2",
    fit_measure = "BIC",
    min_features = 1,
    max_features = ncol(EVS_gspcr[, -1]),
    oneSE = TRUE
)

# Solution
out$sol_table

# What predictors have been included?
out$pred_map

# Use the plotting function
plot_output <- plot(
    x = out,
    y = "BIC",
    labels = TRUE,
    errorBars = TRUE,
    discretize = FALSE
)

# And plot
plot_output

# Out of sample predictions ----------------------------------------------------

# Estimate the model with final decisions
gspcr_est <- est_gspcr(
    dv = EVS_gspcr[, 1],
    ivs = EVS_gspcr[, -1],
    fam = "baseline",
    ndim = out$sol_table[1, "Q"],
    active_set = out$pred_map[, out$sol_table[1, "thr_number"]]
)

# Use model to get predictions on the training sample
y_hat_in_sample <- predict(
    object = gspcr_est
)

# Use model to get prediction on the test sample
y_hat_out_sample <- predict(
    object = gspcr_est,
    newdata = EVS_gspcr[, -1]
)
