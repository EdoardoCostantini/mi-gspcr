# Project:   mi-gspcr-evs
# Objective: Study data to make decisions on how to impute the data
# Author:    Edoardo Costantini
# Created:   2023-08-08
# Modified:  2023-08-08
# Notes: 

# Prepare environment ----------------------------------------------------------

# Load Packages
source("0-prep-load-packages.R")

# Load evs data
EVS <- readRDS("../input/ZA7500_fc_processed.rds")
EVS <- readRDS("../input/ZA7500_processed.rds")

# Prepare Western European countries for ImmerzeelEtAl2016
EVS <- EVS %>%
    filter(country %in% c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece", "Italy", "Luxembourg", "Netherlands", "Norway", "Swizerland"))

# Drop levels from country
EVS$country <- droplevels(EVS$country)

# Load variable type map
var_types <- readRDS("../input/var_types.rds")

# Analysis model selection -----------------------------------------------------

# Model 1: Euthanasia ----------------------------------------------------------

# Define the analysis model variables
amv <- c(
    euth = "v156",
    trust_gen = "v31",
    trust_hcs = "v126",
    trust_pre = "v118",
    trust_sta_1 = "v120",
    trust_sta_2 = "v121",
    trust_sta_3 = "v127",
    trust_sta_4 = "v130",
    rel = "v6",
    den = "v52_r",
    edu = "v243_ISCED_1",
    age = "age_r3",
    sex = "v225",
    country = "country"
)

# Observe their structure
str(EVS[, amv])

# Observe their distribution
par(mfrow = c(4, 4))
for (j in amv) {
    barplot(
        table(na.omit(EVS[, j])),
        main = j
    )
}

# Create md pattern matrix
mdpatterns <- mice::md.pattern(
    EVS[, amv],
    plot = FALSE
)

# Proportions of per variable missing values:
round(colMeans(is.na(EVS[, amv])) * 100, 3)

# Proportion of complete cases
as.numeric(rownames(mdpatterns)[1]) / nrow(EVS) * 100

# Model 2: Right-left voting ---------------------------------------------------

model2_amv <- c(
    # Left / Right voting
    lr = "v174_LR",

    # Country
    country = "country",

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

# Observe their structure
str(EVS[, model2_amv])

# Observe their distribution
par(mfrow = c(5, 5))
for (j in model2_amv) {
    barplot(
        table(na.omit(EVS[, j])),
        main = j
    )
}

# Create md pattern matrix
mdpatterns <- mice::md.pattern(
    EVS[, model2_amv],
    plot = FALSE
)

# Proportions of per variable missing values:
round(colMeans(is.na(EVS[, model2_amv])) * 100, 3)

# Proportion of complete cases
as.numeric(rownames(mdpatterns)[1]) / nrow(EVS) * 100

# Create empty predictor matrix
predMat <- diag(0, ncol = ncol(EVS), nrow = ncol(EVS))

# Give it meaningful names
dimnames(predMat) <- list(colnames(EVS), colnames(EVS))

# Analysis model variables -----------------------------------------------------

predMat[model2_amv, model2_amv] <- 1

# Relation to non-response -----------------------------------------------------

plots <- NULL

# Define a target of imputation
target <- model2_amv["nativ_1"]

# Define potential auxiliary variables (PAs)
PAs <- colnames(EVS)[!colnames(EVS) %in% model2_amv]

# for(i in )
for(j in PAs){

    # Observed cases on j
    pred_obs <- !is.na(EVS[, j])

    # Group observed cases on j based on weather they are missing or not on target
    R <- is.na(EVS[pred_obs, target])

    # Extract predictor of interest
    pred <- factor(EVS[pred_obs, j], labels = 1:length(unique(EVS[pred_obs, j])))

    # Plot histograms
    plots[[j]] <- histogram(
        x = ~ pred | R,
        xlab = j,
        scales = list(y = list(rot = 45), x = list(rot = 45))
    )
}

# Print all of them and analyse
plots[1:10]

# As examples, consider this variables should be left out of the prediction model
pred_drop <- c("v232", "v267", "v268")
plots[names(plots) %in% pred_drop]

# Write down number of variables that should be used used as predictors for the imputaiton of the target
pred_add <- c("v196", "v216", "v261")
plots[names(plots) %in% pred_add]

# Update prediction matrix
predMat[target, pred_add] <- 1

# Check out currrent target prediction
predMat[target, ,drop = FALSE]

# Correlation threshold --------------------------------------------------------

# Study a range of values and their effect on the decisions

# Influx and outflux -----------------------------------------------------------

# Other items from checklist ---------------------------------------------------