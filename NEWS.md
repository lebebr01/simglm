# simglm 0.5.2
* Addition of count outcome from sim_glm.
    - This added an additional argument that must be specified:
        * outcome_type = 'logistic' = 0/1 dichotomous simulation
        * outcome_type = 'poisson' = count outcomes.

# simglm 0.5.1
* Bug fix for sim_glm when using fact_vars generation options.

# simglm 0.5.0
* Heterogeneity of variance simulation
* Flexible time specification for longitudinal models
* Change 'lvl' to 'level' throughout package
* Flexible specification of unbalanced simulation
* Misspecification of model for power analysis
* Expand power output.

# simglm 0.4.0

* Update to add ability to simulate covariates from any R distribution function
    + Old code will no longer work with this new version.
    + Added new opts argument to cov_param for optional distribution arguments.
* Adjusted vignettes to follow new code
* Adjusted unit tests.
* Added documentation for changes, including in vignettes.

# simglm 0.3.4

* Added a `NEWS.md` file to track changes to the package.



