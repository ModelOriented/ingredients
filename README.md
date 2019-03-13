# ingredients: Effects and Importances of Model Ingredients

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ingredients)](https://cran.r-project.org/package=ingredients)
[![Build Status](https://travis-ci.org/ModelOriented/ingredients.svg?branch=master)](https://travis-ci.org/ModelOriented/ingredients)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/ingredients/master.svg)](https://codecov.io/github/ModelOriented/ingredients?branch=master)

Collection of tools for assessment of feature importance and feature effects. 
Part of the `DrWhy` universe. Key functions are: 
`ceteris_paribus()` for the what-if analysis,  it creates ceteris paribus profiles, 
`aggregate_profiles()` and `cluster_profiles()` for aggregation of Ceteris Paribus profiles, this way you can calculate Partial Dependency Plots and much more, 
`feature_importance()` for assessment of batch level feature importance.

## Install

From GitHub

```{r}
devtools::install_github("ModelOriented/ingredients")
```

