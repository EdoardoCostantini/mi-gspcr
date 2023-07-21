# Project:   mi-gspcr-evs
# Objective: Describe missing values in the EVS data
# Author:    Edoardo Costantini
# Created:   2023-07-21
# Modified:  2023-07-21
# Notes: 

# Load data --------------------------------------------------------------------

EVS <- readRDS("../input/ZA7500_fc_processed.rds")
EVS <- readRDS("../input/ZA7500_processed.rds")

# Missing data patterns --------------------------------------------------------

# Counts of missing values:
cm <- colSums(is.na(EVS))
cm

# Proportions of missing values:
pm <- colMeans(is.na(EVS))

# Percentage of missing values
barplot(round(pm, 3) * 100)

# Compute variable-wise count of observed values:
co <- colSums(!is.na(EVS))
co

# Compute variable-wise proportions of observed values:
po <- colMeans(!is.na(EVS))
po


# Create missing data patterns matrix
EVS_mdp <- mice::md.pattern(EVS)

# Variables with missing values
sum(EVS_mdp[nrow(EVS_mdp), -ncol(EVS_mdp)] != 0)

# Missing data proportion of missing cases