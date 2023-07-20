# Project:   mi-gspcr-evs
# Objective: Store session info for replicability
# Author:    Edoardo Costantini
# Created:   2023-07-12
# Modified:  2023-07-12
# Notes:     Run this script to store the information on the session info 
#            before running analysis

# Load packages
#TODO: call a script loading the packages required by the project
library(mice)
library(gspcr)
library(foreign) # to import .dta data
library(labelled) # to extract variable labels
library(mice)
library(dplyr)
library(forcats) # for fct_collapse() function to recode factors

# Define run date
date_time <- format(Sys.time(), "%Y%m%d-%H%M%S")

# Store info about the R session
R_session <- utils::sessionInfo()

# Save the file in the input folder
saveRDS(R_session, paste0("../input/R-session-", date_time, ".rds"))
