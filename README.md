# Multiple Imputation of EVS data with GSPCR

## Summary of project

With this project, I want to show how the MI-GSPCR approach can be used to impute real survey data. In particular, I want to show how you can use it to simplify the imputation model specification step of multiple imputation while generating imputations of the same quality as the standard approach to selecting the predictors in the imputation model.

## Replication

To replicate the results of the study documented in this repository you should:

1. Download this repository
2. Request access and download the data from the [EVS website](https://europeanvaluesstudy.eu/methodology-data-documentation/data-downloads/)
3. Run the `0-prep-install.R` script to install all the packages needed to run the code
4. Run the `0-prep-EVS-data.R` script to prepare the EVS data.
5. Run the imputation routines in scripts `1-impute-mi-expert.R` and `1-impute-mi-gspcr.R` to obtain the imputations. These scripts contain both the serial and parallel `mice()` calls. I recommend using the parallel calls.
6. Run `2-diagnostics.R` and `3-analyse.R` to obtain results.

## Project housekeeping

### Output files record

The results of the study are stored in the following files:

- `20230815-221458-mids-mi-gspcr.rds` trail run of imputation with MI-GSPCR method (`maxit = 5`)
- `20230817-155605-mids-mi-gspcr.rds` main results of imputation with MI-GSPCR method with 20 iterations
- `20230829-100551-mids-mi-expert.rds` main results of imputation with MI-Standard method with 20 iterations
- `20240126-234235-mids-mi-expert.rds` main results of imputation with MI-Standard method with 150 iterations

These files are not stored on GitHub because of their size. They are available upon request.

### Additional analysis

To check if a larger number of imputations would result in closer between-imputation variance estimates I ran both MI-GSPCR and MI-Expert again. The results were stored in:

- `20240129-141433-mids-mi-gspcr.rds` main results of imputation with MI-GSPCR method with `m = 15` and `maxit = 10`
- `coming soon` main results of imputation with MI-Standard with `m = 15` and `maxit = 10`
