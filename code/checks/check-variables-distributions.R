# Project:   mi-gspcr-evs
# Objective: Check how EVS variables are distributed
# Author:    Edoardo Costantini
# Created:   2023-07-20
# Modified:  2023-07-20
# Notes: 

# Load data --------------------------------------------------------------------

# Load evs data
EVS <- readRDS("../input/ZA7500_processed.rds")

# Load variable type map
var_types <- readRDS("../input/var_types.rds")

# Count data -------------------------------------------------------------------

# Continuous data --------------------------------------------------------------

# > v279d_r: Interview time ----------------------------------------------------

# Store variable with no NAs
v279d_r <- na.omit(as.numeric(as.character(EVS2017$v279d_r)))

# Logged version
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

# It's impossible the interview took 0 minutes, maybe replace with NAs?

# Kurtosis and skewness are much better
e1071::kurtosis(v279d_r_log[-which(-Inf == v279d_r_log)])
e1071::skewness(v279d_r_log[-which(-Inf == v279d_r_log)])