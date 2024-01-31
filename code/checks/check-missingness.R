# Project:   mi-gspcr-evs
# Objective: Describe missing values in the EVS data
# Author:    Edoardo Costantini
# Created:   2023-07-21
# Modified:  2023-09-28
# Notes: 

# Load data --------------------------------------------------------------------

EVS <- readRDS("../input/ZA7500_fc_processed.rds")
EVS <- readRDS("../input/ZA7500_processed.rds")

# Proportion of missing values -------------------------------------------------

# Counts of missing values:
cm <- colSums(is.na(EVS))
cm

# Proportions of missing values:
pm <- colMeans(is.na(EVS))

# Percentage of missing values
barplot(round(pm, 3) * 100)

# Number of variables with missing values
sum(cm != 0)

# Range of pm for incomplete variables
range(pm[cm != 0]) * 100

# > Check Variables with high PM -----------------------------------------------
pm[pm > .30]

# - v173 is high because only asked in European countries (see codebook for list)
# - v174_LR is high because of high missing rate
# - v175_LR is high because of high missing rate

# > Check Variables with moderate PM -------------------------------------------
pm[pm > .10 & pm < .30]

# - all due to real missing values
# - v276_r data for Great Britain and NL not available

# > Check pm for variables in analysis model -----------------------------------

# Define model variables
model_amv <- c(
    # Left / Right voting
    lr = "v174_LR",

    # Female
    sex = "v225",

    # Employment Status
    SES = "v246_egp",

    # Native attitudes (mean of itmes)
    nativ_1 = "v185", # jobs
    nativ_2 = "v186", # crime
    nativ_3 = "v187", # strain on welfare

    # Authoritarian Attitudes
    # Low and order attitudes
    strongL = "v145",
    order = "v110",

    # Political Interest
    pol_1 = "v97",

    # Political Action
    pa_1 = "v98",
    pa_2 = "v99",
    pa_3 = "v100",
    pa_4 = "v101",

    # Covariates
    age = "age_r3",
    edu = "v243_ISCED_1",
    mat = "v234",

    # urb = v276_r
    urb = "v276_r",

    # Religiousness
    rel = "v6",

    # Denomination
    denom = "v52_r"
)

# Average pm across the predictors 
mean(pm[model_amv][-1])
length(pm[model_amv][-1])

# Missing data patterns --------------------------------------------------------

# Create missing data patterns matrix
EVS_mdp <- mice::md.pattern(EVS, plot = FALSE)

# Number of missing data patterns
nrow(EVS_mdp) - 1

# Frequency of pattern-wise missing counts:
table(EVS_mdp[-nrow(EVS_mdp), ncol(EVS_mdp)])

# Covariance coverage ----------------------------------------------------------

# md pairs
EVS_mdpairs <- mice::md.pairs(EVS)

# Compute covariance coverage matrix:
cc_mat <- round(EVS_mdpairs$rr / nrow(EVS), 3)*100
cc_mat

# Extract unique coverage values from the covariance coverage matrix:
cc <- cc_mat[lower.tri(cc_mat)]

# Obtain the range of the coverage values
range(cc)

# Find problematic pairs (use entire covariance coverage matrix):
pat <- cc_mat <= 80
apply(pat, 2, function(x) names(x)[x])