# Project:   mi-gspcr-evs
# Objective: Check how EVS variables are distributed
# Author:    Edoardo Costantini
# Created:   2023-07-20
# Modified:  2023-07-28
# Notes: 

# Load data --------------------------------------------------------------------

# Load evs data
EVS <- readRDS("../input/ZA7500_fc_processed.rds")
EVS <- readRDS("../input/ZA7500_processed.rds")

# Load variable type map
var_types <- readRDS("../input/var_types.rds")

# Define an empty method vector for imputation
imputation_methods <- rep(NA, ncol(EVS))

# Give it the variables names
names(imputation_methods) <- colnames(EVS)

# Binary data ------------------------------------------------------------------

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

# Impute all of these with gspcr.logreg
imputation_methods[var_types$bin] <- "gspcr.logreg"

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

# Impute all of these with gspcr.polyreg
imputation_methods[var_types$cat] <- "gspcr.polyreg"

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

# Impute most of these with gspcr.pmm
imputation_methods[var_types$ord] <- "gspcr.pmm"

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

imputation_methods[norm] <- "gspcr.norm"

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

# but there is a -Inf value now
range(v279d_r_log)

# which was originally
v279d_r[which(-Inf == v279d_r_log)]

# It's impossible the interview took 0 minutes, so we replace with NA
EVS$v279d_r[which(EVS$v279d_r == 0)] <- NA

# Kurtosis and skewness are much better on the log scale
e1071::kurtosis(v279d_r_log[-which(-Inf == v279d_r_log)])
e1071::skewness(v279d_r_log[-which(-Inf == v279d_r_log)])

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

# > v240: People in household --------------------------------------------------

# Store variable with no NAs
v240 <- na.omit(EVS$v240)

# Show density plot
plot(density(v240))

# Kurtosis (extreme)
e1071::kurtosis(v240)

# Skewness (extreme)
e1071::skewness(v240)

# Impute both with gspcr.pmm
imputation_methods[var_types$cou] <- "gspcr.pmm"

# Store imputation methods vector ----------------------------------------------

saveRDS(imputation_methods, "../input/imputation_methods-mi-gspcr.rds")