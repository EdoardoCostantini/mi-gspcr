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

# Toy dataset
# EVS <- EVS[, c("v1", "v2", "v9", "v10", "v54", "v55", "v279d_r", "v242", "country")]

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

# Relationship types -----------------------------------------------------------

# Craete variable pairs
var_paris <- expand.grid(names(var_types), names(var_types))
var_paris <- var_paris[!duplicated(t(apply(var_paris, 1, sort))), ]

# Assign relationship
data.frame(
    var_paris,
    asso = c(
        "cramer",
        "cramer",
        "cramer",
        "pearson",
        "pearson",
        "cramer",
        "cramer",
        "?",
        "?",
        "cramer",
        "cramer",
        "peaarson",
        "peaarson",
        "peaarson",
        "peaarson"
    )
)

# Relation to non-response -----------------------------------------------------

# Create a matrix of response vectors
R <- is.na(EVS)

# Make them factors
R <- as.data.frame(lapply(as.data.frame(R), factor))

# Define matrix to store relation to non-resp
mat_relno <- diag(0, ncol = ncol(EVS), nrow = ncol(EVS))
colnames(mat_relno) <- colnames(EVS)
rownames(mat_relno) <- colnames(EVS)

# Apply logistic regression Pseudo R2
for (j in 1:ncol(EVS)) {
    print(j)

    # Check there is missing value
    if (nlevels(R[, j]) == 2){
        # Compute the PR2 for all simple models
        ascores <- cp_thrs_PR2(
            dv = R[, j],
            ivs = EVS[, -j],
            fam = "binomial"
        )
    } else {
        ascores <- rep(0, ncol(EVS)-1)
        names(ascores) <- names(EVS[, -j])
    }
    
    # Put in matrix
    mat_relno[j, names(ascores)] <- ascores^2
    mat_relno[names(ascores), j] <- ascores^2
}

# Quick peak
round(mat_relno, 2)

# Correlation threshold --------------------------------------------------------

# Define a storing object
mat_asso <- diag(0, ncol = ncol(EVS), nrow = ncol(EVS))
colnames(mat_asso) <- colnames(EVS)
rownames(mat_asso) <- colnames(EVS)

for (j in 1:ncol(EVS)) {
    print(j)
    # Var type
    vtype <- class(EVS[, j])
    ncat <- length(unique(EVS[, j]))

    # Define the family
    if ("ordered" %in% vtype) {
        fam <- "cumulative"
    } else {
        if ("numeric" %in% vtype) {
            if (is.integer(EVS[, j])) {
                fam <- "poisson"
            } else {
                fam <- "gaussian"
            }
        }
        if ("factor" %in% vtype) {
            if (ncat == 2) {
                fam <- "binomial"
            } else {
                fam <- "baseline"
            }
        }
    }

    # Compute the PR2 for all simple models
    ascores <- cp_thrs_PR2(
        dv = EVS[, j],
        ivs = EVS[, -j],
        fam = fam
    )

    # Put in matrix
    mat_asso[j, names(ascores)] <- ascores^2
    mat_asso[names(ascores), j] <- ascores^2
}

# Quick look
round(mat_asso, 2)

# Save them for future use
saveRDS(
    list(
        mat_asso = mat_asso,
        mat_relno = mat_relno
    ),
    file = "../input/mi-model-expert-inputs.rds"
)

# Read the saved file
mats <- readRDS("../input/mi-model-expert-inputs.rds")

# Combine the two sources of info
maxc <- pmax(mats$mat_asso, mats$mat_relno)

# Keep the values higher than .1
predMat[maxc > .1] <- 1

# And you can compare with the results quickpred
quickpred(EVS)

# Usabable cases ---------------------------------------------------------------

# Define a target percentage of usable cases (puc)
minpuc <- .5

# Compute all md paris
p <- md.pairs(EVS)

# Compute percentage of usable cases
puc <- p$mr / (p$mr + p$mm)

# exclude predictors with a percentage usable cases below minpuc
predMat[puc < minpuc] <- 0

# Influx and outflux -----------------------------------------------------------

# Other items from checklist ---------------------------------------------------