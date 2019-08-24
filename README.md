# ingredients: Effects and Importances of Model Ingredients

[![Build Status](https://api.travis-ci.org/ModelOriented/ingredients.png)](https://travis-ci.org/ModelOriented/ingredients)
[![Coverage
Status](https://img.shields.io/codecov/c/github/ModelOriented/ingredients/master.svg)](https://codecov.io/github/ModelOriented/ingredients?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ingredients)](https://cran.r-project.org/package=ingredients)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ingredients?color=orange)](http://cranlogs.r-pkg.org/badges/grand-total/ingredients)

## Overview

The `ingredients` package is a collection of tools for assessment of feature importance and feature effects.

Key functions are: 

* `feature_importance()` for assessment of global level feature importance, 
* `ceteris_paribus()` for calculation of the Ceteris Paribus / What-If Profiles (read more at https://pbiecek.github.io/PM_VEE/ceterisParibus.html),
* `calculate_oscillations()` for calculation of the Ceteris Paribus Oscillations (read more at https://pbiecek.github.io/PM_VEE/ceterisParibusOscillations.html),
* `ceteris_paribus_2d()` for Ceteris Paribus 2D Profiles  (read more at https://pbiecek.github.io/PM_VEE/ceterisParibus2d.html),
* `partial_dependency()` for Partial Dependency Plots,
* `conditional_dependency()` for Conditional Dependency Plots also called M Plots,
* `accumulated_dependency()` for Accumulated Local Effects Plots,
* `aggregate_profiles()` and `cluster_profiles()` for aggregation of Ceteris Paribus Profiles,
* `aspect_importance()` for LIME style explanations,
* generic `print()` and `plot()` for better usability of selected explainers,
* generic `plotD3()` for interactive, D3 based explanations,
* generic `describe()` for explanations in natural language.
 
The philosophy behind `ingredients` explanations is described in the [Predictive Models: Explore, Explain, and Debug](https://pbiecek.github.io/PM_VEE/) e-book. The `ingredients` package is a part of [DrWhy.AI](http://DrWhy.AI) universe. 


## Installation

```{r}
# the easiest way to get ingredients is to install it from CRAN:
install.packages("ingredients")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("ModelOriented/ingredients")
```

## Interactive plots with D3

`feature_importance()`, `ceteris_paribus()` and `aggregated_profiles()` also work with **D3**! 
[see an example](https://modeloriented.github.io/ingredients/ceterisParibusDemo.html) 
![plotD3](images/demo.gif)


## Acknowledgments

Work on this package was financially supported by the 'NCN Opus grant 2016/21/B/ST6/02176'.
    
