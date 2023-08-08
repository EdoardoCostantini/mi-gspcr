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
levels(EVS$country)

 <- c("Austria",'Belgium', 'Denmark', 'Finland', 'France', 'Germany', 'Greece', 'Italy', 'Luxembourg', 'Netherlands', 'Norway', 'Swizerland')

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

# Correlation threshold --------------------------------------------------------

# Study a range of values and their effect on the decisions

# Influx and outflux -----------------------------------------------------------

# Other items from checklist ---------------------------------------------------