# Simple imputation model specification with principal component regression: a tutorial

## Summary of project

With this project, I want showc how the MI-GSPCR approach can be used to impute real survey data. In particular, I want to show how you can use it to simplify the imputation model specification step of multiple imputation while generating imputations of the same quality as the standard approach to selecting the predictors in the imputation model.

## Replication

To replicate the results of the study documented in this repository you should:

1. Download this repository
2. Request access and download the data from the [EVS website](https://europeanvaluesstudy.eu/methodology-data-documentation/data-downloads/)
3. Run the `0-prep-install.R` script to install all the packages needed to run the code
4. Run the `0-prep-EVS-data.R` script to prepare the EVS data.
5. Run the imputation routines in scripts `1-impute-mi-expert.R` and `1-impute-mi-gspcr.R`.
6. Run `2-diagnostics.R` and `3-analyse.R` to obtain results.

## Project housekeeping

### Output files record

The results for the study are stored in the following files:

- `20230815-221458-mids-mi-gspcr.rds` trail run of imputation with mi-gscpr method (`maxit = 5`)
- `20230817-155605-mids-mi-gspcr.rds` main results of imputation with mi-gscpr method with 20 iterations
- `20230829-100551-mids-mi-expert.rds` main results of imputation with mi-expert method with 20 iterations
- `20240126-234235-mids-mi-expert.rds` main results of imputation with mi-expert method with 150 iterations

These files are not stored on GitHub because of their size. They are available upon request.