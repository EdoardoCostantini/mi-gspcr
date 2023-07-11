# Project:   mi-gspcr-evs
# Objective: Creating synthetic data based on EVS data
# Author:    Edoardo Costantini
# Created:   2023-07-11
# Modified:  2023-07-11
# Notes: 

# 1. Load data -----------------------------------------------------------------

# Read imputed data
imp <- readRDS("../input/ZA7500_mi.rds")

# Extract first impute data
EVS <- complete(imp, 1)

# 2. Synthetic data with mice --------------------------------------------------

# Load packages based on vignette description
library(mice) # to create the synthetic data
library(ggmice) # to make visualizations of the synthetic data
library(ggplot2) # required when using ggmice
library(patchwork) # to stitch multiple figures together
library(psych) # to obtain descriptive statistics
library(purrr) # to work with multiply imputed synthetic datasets
library(synthpop) # to assess the utility of our synthetic data

# specify which values we want to overimpute
where <- make.where(EVS, "all")

# create a method vector to specify which method to use for each variable
method <- make.method(EVS, where = where)

# define all methods as cart
method[method != ""] <- "cart"

# Use mice to create 10 synthetic datasets
syn_cart <- mice(
    data = EVS,
    m = 10,
    maxit = 1,
    method = method,
    where = where,
    printFlag = TRUE
)

# 2.1 Synthetic data utility ---------------------------------------------------

# 2.2 - Univariate data utility

# 2.3 - Multivariate data utility

# 2.4 - Statistical disclosure control

# Check observations in the original data also occur in the synthetic data

complete(syn_cart, "long") %>%
    dplyr::select(-c(.imp, .id)) %>%
    dplyr::bind_rows(heart_failure) %>%
    duplicated() %>%
    which()

# 3. Synthetic data with synthpop ----------------------------------------------