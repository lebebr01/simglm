---
title: "Simulate Data from Generalized Linear Models"
date: "2015-10-29"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to simReg}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---


# Simulated Logistic Models
The `simReg` package offers users the ability to simulate from a variety of generalized linear models, both single level and multilevel generalized models. Instead of using the `sim_reg` function to call these, there is now a `sim_glm` function to use. 

Similar to the `sim_reg` function, one benefit of this package for simulation is that the intermediate steps are returned as well. This is useful for additional processing of the data, for example to add in your own missing data function.

Here is an example for simulating a single level logistic model:







