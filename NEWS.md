# simglm 1.0.0

This major release accompanies the publication of the `simglm` book. It
expands the package's simulation and power-analysis capabilities and aligns
the package API with the examples presented in the book.

## New outcome and data-generation options

* Added support for ordinal and multinomial outcomes.
* Added floor and ceiling controls for continuous outcomes.
* Changed factor simulation to preserve the declared order of levels rather
  than sorting levels alphabetically.
* Added `force_equal = TRUE` for generating equally represented factor levels.
* Expanded support for unbalanced designs and flexible sample-size
  specifications.
* Generalized cross-classified data generation through multiple-membership
  simulation.
* Added post-processing and outcome aggregation tools.
* Extended formula parsing and simulation support for list-based
  specifications and multiple-equation workflows.

## Power analysis and model fitting

* Reworked the power-simulation framework, including improved support for
  between- and within-subject designs.
* Added type S and type M error summaries.
* Added `robust_model()` for models using robust standard errors.

## Propensity score workflows

* Added `simulate_propensity()` for generating non-random treatment
  assignment.
* Added `fit_propensity()` and support for covariate adjustment, inverse
  probability weighting (`ipw`), and stabilized balancing weights (`sbw`).
* Added support, examples, and tests for multilevel propensity score designs.

## Documentation, testing, and maintenance

* Expanded the vignettes and documentation for missing-data, factor, ordinal,
  multinomial, post-processing, power, and propensity score workflows.
* Added lightweight compatibility tests based on code published in the book.
* The package now requires R 4.1.0 or later because examples and simulation workflows use the native R pipe (|>)
* Removed `Matrix` from package imports and added `gtools`, `sandwich`, and
  `lmtest`.

# simglm 0.7.2
* Small maintenance fix for incoming 0.8 dplyr.

# simglm 0.7.1
* Release for new tidy simulation framework
* New vignettes showing this functionality

# simglm 0.6.3
* Add piecewise linear simulation.

# simglm 0.6.2
* Add cross classified model simulation
* Add option to specify any model to fit for power analysis
   - This brought about a change to use broom::tidy.

# simglm 0.6.1
* Generalize fact_vars code
   - This now is similar to cov_param

# simglm 0.6.0
* Shiny Application works again!
   - Can now simulate and run power. 
   - Able to download simulation and power tables (I think).

# simglm 0.5.3
* Fixed basic functionality of Shiny application
   - This includes simulation and power
       + Needs more testing at this stage.

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

