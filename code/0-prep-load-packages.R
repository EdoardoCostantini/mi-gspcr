# Project:   mi-gspcr-evs
# Objective: Package loading
# Author:    Edoardo Costantini
# Created:   2023-07-12
# Modified:  2023-08-09
# Notes:     Run this script to store the information on the session info 
#            before running analysis

# Load packages
library(mice)
library(gspcr)
library(foreign) # to import .dta data
library(labelled) # to extract variable labels
library(mice)
library(dplyr)
library(forcats) # for fct_collapse() function to recode factors
library(lattice)
library(gridExtra) # to arrange lattice plots

# Communicate versions of delicate packages to the user
message(
    paste0(
        "The following package versions are in use:\n\n",
        "- mice: ", packageVersion("mice"), "\n\n",
        "- gspcr: ", packageVersion("gspcr"), "\n\n"
    )
)
