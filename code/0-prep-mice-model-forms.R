# Project:   mi-gspcr
# Objective: Study data to make decisions on the imputaiton model
# Author:    Edoardo Costantini
# Created:   2023-08-10
# Modified:  2023-10-03
# Notes: 

# Prepare environment ----------------------------------------------------------

# Load Packages
source("0-prep-load-packages.R")

# Load evs data
EVS <- readRDS("../input/ZA7500_fc_processed.rds")
EVS <- readRDS("../input/ZA7500_processed.rds")

# Load variable type map
var_types <- readRDS("../input/var_types.rds")

# Define an empty method vector for imputation
meths <- rep(NA, ncol(EVS))

# Give it the variables names
names(meths) <- colnames(EVS)

# Summary of variable types
sapply(var_types, length)

# Binary data ------------------------------------------------------------------

# Which variables are you treating
var_types$bin

# Define plot space
par(mfrow = c(3, 3))

# Check the distribution of category options
for (j in var_types$bin) {
    barplot(
        table(na.omit(EVS[, j])),
        main = j
    )
}

# v18 - do you belong to Self-help group, mutual aid group (example extreme case)
table(EVS[, "v18"])

# Impute all of these with logreg
meths[var_types$bin] <- "logreg"

# Categorical data -------------------------------------------------------------

# How many right now?
length(var_types$cat)

# Define plot space
par(mfrow = c(3, 3))

# Check the distribution of category options
for (j in var_types$cat) {
    barplot(
        table(na.omit(EVS[, j])),
        main = j
    )
}

# Impute all of these with polyreg
meths[var_types$cat] <- "polyreg"

# Ordinal data -----------------------------------------------------------------

var_types$ord

# Define plot space
par(mfrow = c(3, 3))

# Plot density for each
for (j in var_types$ord) {
    plot(
        density(
            na.omit(as.numeric(EVS[, j]))
        ),
        main = j,
        xlab = "",
        ylab = ""
    )
}

table(EVS$v106)

# Impute most of these with pmm
meths[var_types$ord] <- "pmm"

# Except some exceptionally normally distributed variables

norm <- paste0("v", c(212:216, 218:219, 184))

for (j in norm) {
    plot(
        density(
            na.omit(as.numeric(EVS[, j]))
        ),
        main = j,
        xlab = "",
        ylab = ""
    )
}

meths[norm] <- "norm"

# Continuous data --------------------------------------------------------------

var_types$con

# > v279d_r: Interview time ----------------------------------------------------

# Store variable with no NAs
v279d_r <- na.omit(as.numeric(as.character(EVS$v279d_r)))

# Log version
v279d_r_log <- log(v279d_r)

# Show density plot
plot(density(v279d_r))

# Focused density plot
plot(density(v279d_r), xlim = c(0, 200))

# Kurtosis (extreme)
e1071::kurtosis(v279d_r)

# Skewness (extreme)
e1071::skewness(v279d_r)

# Logged version is much much better
plot(density(v279d_r_log))

# Use norm to impute the log version
meths["v279d_r"] <- "norm"

# > v242: age complete education -----------------------------------------------

# Store variable with no NAs
v242 <- na.omit(EVS$v242)

# Log version
v242_log <- log(v242)

# Show density plot
plot(density(v242))

# Kurtosis (extreme)
e1071::kurtosis(v242)

# Skewness (extreme)
e1071::skewness(v242)

# Logged version is much much better
plot(density(v242_log))

# Kurtosis and skewness are much better
e1071::kurtosis(v242_log)
e1071::skewness(v242_log)

# Use pmm to impute the original version
meths["v242"] <- "pmm"

# Count data -------------------------------------------------------------------

var_types$cou

# > v239_r: Number of children -------------------------------------------------

# Store variable with no NAs
v239_r <- na.omit(EVS$v239_r)

# Show density plot
plot(density(v239_r))

# Kurtosis (extreme)
e1071::kurtosis(v239_r)

# Skewness (extreme)
e1071::skewness(v239_r)

# Use pmm to impute
meths["v239_r"] <- "pmm"

# > v240: People in household --------------------------------------------------

# Store variable with no NAs
v240 <- na.omit(EVS$v240)

# Show density plot
plot(density(v240))

# Kurtosis (extreme)
e1071::kurtosis(v240)

# Skewness (extreme)
e1071::skewness(v240)

# Impute both with pmm
meths["v240"] <- "pmm"

# Check the imputation methods vector ------------------------------------------

# Summary of used methods
table(meths)

# Variable measurement level and methods overivew
data.frame(
    class = sapply(EVS, function(j) paste0(class(j), collapse = " ")),
    method = meths
)

# Every variable has an imputation method
testthat::expect_true(any(is.na(meths)) == FALSE)

# Bianry variables have 2 levels exactly
testthat::expect_true(
    all(
        sapply(
            EVS[, names(meths[which(meths == "logreg")])],
            function(j) nlevels(j)
        ) == 2
    )
)

# Norm variables have 5 or more categories (will be transformed to numeric)
testthat::expect_true(
    all(
        sapply(
            EVS[, names(meths[which(meths == "norm")])],
            function(j) length(unique(j))
        ) >= 5
    )
)

# Only unordered factors in polyreg methods
testthat::expect_false(
    any(
        sapply(
            lapply(
                EVS[, names(meths[which(meths == "polyreg")])],
                class
            ),
            function(j) any(j != "factor")
        )
    )
)

# Store imputation methods vector ----------------------------------------------

saveRDS(meths, "../input/mi-model-forms.rds")
