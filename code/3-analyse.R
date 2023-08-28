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
mids_miexpert <- readRDS("../output/20230828-104250-mids-mi-expert.rds")

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

# Plots ------------------------------------------------------------------------

# > Side by side barplots ------------------------------------------------------

# Decide what to plot
(parameter <- colnames(pool_expert$pooled)[6])

# Create a dataset for plot
data_plot <- data.frame(
    pool_expert$pooled[-1, 1, drop = FALSE],
    expert = abs(round(pool_expert$pooled[-1, parameter], 5)),
    gscpr = abs(round(pool_gspcr$pooled[-1, parameter], 5))
)

# Melt the data
gg_shape <- reshape2::melt(data_plot, id.vars = "term", value.name = parameter)

# Give useful names
colnames(gg_shape) <- c("coefficient", "imputation", parameter)

# Make plot
ggplot(
    data = gg_shape,
    aes(
        x = coefficient,
        y = get(parameter),
        fill = imputation
    )
) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        alpha = 0.75
    ) +
    labs(
        y = parameter
        ) +
    theme(
        axis.text.x = element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1
        )
    )
