# Simple imputation model specification with principal component regression: a tutorial

## Summary of project

With this project I want to tutorialise the imputation of missing data to showcase through the MI-GSPCR approach. In particular, I want to show how you can use it to simplify the imputation model specification step of multiple imputation. The choices that must be made are:

1. Model form (GLM link function) - This step stays the same
2. Selection of the predictors - All predictors are kept, but the user can still make a few decisions:
    1. Threshold types
    2. Grid of threshold values
    3. Tuning strategy
        1. cross-validation
        2. likelihood-based
    4. Number of PCs
        1. Fixed
        2. Cross-validated

After describing these choices in detail, I would like to present an application where we apply this method to the EVS data as is, with no synthetic data. The data should be imputed with the MI-GSPCR approach, with a guided description of how the decisions in point 2 were taken. The data should also be imputed based on a traditional mice imputation procedure and the steps taken to respond to step 2 should be explained clearly.