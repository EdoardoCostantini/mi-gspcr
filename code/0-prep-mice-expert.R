# Project:   mi-gspcr-evs
# Objective: Study data to make decisions on how to impute the data
# Author:    Edoardo Costantini
# Created:   2023-08-08
# Modified:  2023-08-21
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

# Ad-hoc data prep -------------------------------------------------------------

# Transform ordinal variables with more than 5 categories to numeric for PCA
for (j in var_types$ord) {
    if (nlevels(EVS[, j]) >= 5) {
        EVS[, j] <- as.numeric(EVS[, j])
    }
}

# Replace v279d_r variable with its log
EVS$v279d_r[!is.na(EVS$v279d_r)] <- log(EVS$v279d_r[!is.na(EVS$v279d_r)])
EVS$v279d_r <- EVS$v279d_r

# Replace v242 variable with its log
EVS$v242[!is.na(EVS$v242)] <- log(EVS$v242[!is.na(EVS$v242)])

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

# Usabable cases ---------------------------------------------------------------

# Define a target percentage of usable cases (puc)
minpuc <- .5

# Compute all md paris
p <- md.pairs(EVS)

# Compute percentage of usable cases
puc <- p$mr / p$mr + p$mm

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

# Start progress bar
pb <- txtProgressBar(
    min = 1,
    max = ncol(EVS),
    initial = 1,
    style = 3
)

# Apply logistic regression Pseudo R2
for (j in 1:ncol(EVS)) {
    # Update progress bar
    setTxtProgressBar(pb, j)

    # Define variables for which enough cases are available
    active_set <- names(which(puc[j, -j] >= minpuc))

    # Check there is missing value
    if (nlevels(R[, j]) == 2){
        # Compute the PR2 for all simple models
        ascores <- cp_thrs_PR2(
            dv = R[, j],
            ivs = EVS[, active_set],
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

# Close progress bar
close(pb)

# Quick peak
round(mat_relno, 2)

# Correlation threshold --------------------------------------------------------

# Define a storing object
mat_asso <- diag(0, ncol = ncol(EVS), nrow = ncol(EVS))
colnames(mat_asso) <- colnames(EVS)
rownames(mat_asso) <- colnames(EVS)

# Start progress bar
pb <- txtProgressBar(
    min = 1,
    max = ncol(EVS),
    initial = 1,
    style = 3
)

# Loop over data columns
for (j in 1:ncol(EVS)) {
    # Update progress bar
    setTxtProgressBar(pb, j)

    # Define variables for which enough cases are available
    active_set <- names(which(puc[j, -j] >= minpuc))

    # Define variables for this loop
    dv <- EVS[, j]
    ivs <- EVS[, active_set]

    # Var type
    vtype <- class(EVS[, j])
    ncat <- length(unique(na.omit(EVS[, j])))

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

    # Train the null model and the p simple models
    if (fam == "gaussian" | fam == "binomial" | fam == "poisson") {
        r2 <- sapply(1:ncol(ivs), function(k) {
            # Define complete cases
            ry <- !is.na(dv) & !is.na(ivs[, k])

            # Fit null model
            glm0 <- stats::glm(dv[ry] ~ 1, family = fam)

            # Fit simple models
            glm1 <- stats::glm(dv[ry] ~ ivs[ry, k], family = fam)

            # Compute the PR2
            cp_gR2(
                ll_n = logLik(glm0),
                ll_f = logLik(glm1),
                n = sum(ry)
            )
        })
    }
    if (fam == "baseline") {
        r2 <- sapply(1:ncol(ivs), function(k) {
            # Define complete cases
            ry <- !is.na(dv) & !is.na(ivs[, k])

            # Fit null model
            glm0 <- nnet::multinom(
                formula = dv[ry] ~ 1,
                trace = FALSE
            )

            # Fit simple models
            glm1 <- tryCatch(
                expr = {
                    nnet::multinom(
                        formula = dv[ry] ~ ivs[ry, k],
                        trace = FALSE
                    )
                },
                error = function(e) {
                    glm0
                }
            )

            # Compute the PR2
            cp_gR2(
                ll_n = logLik(glm0),
                ll_f = logLik(glm1),
                n = sum(ry)
            )
        })
    }
    if (fam == "cumulative") {
        r2 <- sapply(1:ncol(ivs), function(k) {
            # Define complete cases
            ry <- !is.na(dv) & !is.na(ivs[, k])

            # Fit null model
            glm0 <- MASS::polr(
                formula = dv[ry] ~ 1,
                method = "logistic"
            )

            # Fit simple models
            glm1 <- MASS::polr(
                        formula = dv[ry] ~ ivs[ry, k],
                        method = "logistic"
                    )

            # Compute the PR2
            cp_gR2(
                ll_n = logLik(glm0),
                ll_f = logLik(glm1),
                n = sum(ry)
            )
        })
    }

    # Give meaningful names
    names(r2) <- names(ivs)

    # Put in matrix
    mat_asso[j, names(r2)] <- r2
    mat_asso[names(r2), j] <- r2
}

# Close progress bar
close(pb)

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
predMat_qp <- quickpred(EVS)

# Monitor differences between quickpred and our approach
cbind(
    rowSums(predMat),
    rowSums(predMat_qp)
)

# Fix extreme: reduce high numbers ---------------------------------------------

round(mats$mat_asso[j, names(r2)], 3)
round(sqrt(r2), 3)

barplot(sort(sqrt(r2)))

# Fix extreme: reduce low numbers ----------------------------------------------

# There were a few variables that did not have any predictors meeting requirements
no_predictors <- rownames(predMat)[rowSums(predMat) == 0]

# Look at the distribution of the values
barplot(sort(mats$mat_asso[no_predictors[1], ]))

# What is the correlation version of this plot?
nvar <- ncol(EVS)
predictorMatrix <- matrix(0, nrow = nvar, ncol = nvar, dimnames = list(names(EVS), names(EVS)))
x <- data.matrix(EVS)
EVS$country
head(data.matrix(EVS)[, 1:5])
dim(EVS)
r <- !is.na(x)
v <- abs(cor(x, use = "pairwise.complete.obs", method = "pearson"))
idx <- which(colnames(v) == no_predictors[1])
barplot(tail(sort(v[idx, -idx]), 10))
barplot(tail(sort(sqrt(r2)), 10))
dim(v)
barplot(sort(v[idx, -idx]))
barplot(sort(sqrt(r2)))
length(r2)

# Influx and outflux -----------------------------------------------------------

# Other items from checklist ---------------------------------------------------