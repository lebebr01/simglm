# simglm 1.0.0

**Changes:**
* Change default behavior for factor simulations, order of levels are maintained now instead of alphabetical order which is the default in R. 

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



