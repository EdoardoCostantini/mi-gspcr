# Project:   mi-gspcr-evs
# Objective: function to drop variables by name
# Author:    Edoardo Costantini
# Created:   2023-07-11
# Modified:  2023-07-11
# Notes:

dropVars <- function(data, variables) {
  # Internals -------------------------------------------------------------

  # data = ... # any data set with named columns
  # variables = ... # character vectors with names of variables to drop

  # Body ------------------------------------------------------------------

  keep <- !colnames(data) %in% variables
  data[, keep]

}
