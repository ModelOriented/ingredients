# ingredients: Effects and Importances of Model Ingredients

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ingredients)](https://cran.r-project.org/package=ingredients)
[![Build Status](https://travis-ci.org/ModelOriented/ingredients.svg?branch=master)](https://travis-ci.org/ModelOriented/ingredients)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/ingredients/master.svg)](https://codecov.io/github/ModelOriented/ingredients?branch=master)

Collection of tools for assessment of feature importance and feature effects.

Key functions are: 

* `feature_importance()` for assessment of global level feature importance, 
* `ceteris_paribus()` for calculation of the What-If Plots,
* `partial_dependency()` for Partial Dependency Plots,
* `conditional_dependency()` for Conditional Dependency Plots also called M Plots,
* `accumulated_dependency()` for Accumulated Local Effects Plots,
* `aggregate_profiles()` and `cluster_profiles()` for aggregation of Ceteris Paribus Profiles,
* `theme_drwhy() `with a ggplot2 skin for all plots,
* generic `print()` and `plot()` for better usability of selected explainers.
 
 The package `ingredients` is a part of the `DrWhy.AI` universe. 

## Install

From GitHub

```{r}
devtools::install_github("ModelOriented/ingredients")
```

