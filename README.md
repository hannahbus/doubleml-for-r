
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DoubleML - Double Machine Learning in python and R

## About `DoubleML`

`DoubleML` is an implementation of the Double Machine Learning framework
of [Chernozhukov et al. (2018)](https://arxiv.org/abs/1608.00060) in
python and R. An extensive user guide in both languages can be found
`here`. The documentation of the R package can be found `here`.

## Introduction

The R package `DoubleML` R provides an implementation of the double
machine learning framework of [Chernozhukov et
al. (2018)](https://arxiv.org/abs/1608.00060). The package allows for

  - uniformly valid statistical inference with a variety of machine
    learning methods, for example construct t-statistics, p-values and
    joint confidence bands,

  - generic double machine learning with and without (repeated)
    cross-fitting,

  - a list of causal models, for example, the partially linear
    regression model and the interactive regression model as well as its
    instrumental variable versions,

  - flexible interface for machine learning methods as provided by the
    [mlr3](https://mlr3.mlr-org.com/) package and its extensions, for
    example [mlr3learner](https://mlr3learners.mlr-org.com/) and
    [mlr3extralearners](https://mlr3extralearners.mlr-org.com/),

  - built-in advanced parameter tuning as provided by
    [mlr3tuning](https://mlr3tuning.mlr-org.com/) and parameter passing
    from external tuning with
    [mlr3tuning](https://mlr3tuning.mlr-org.com/) or other packages.

The object oriented implementation of the R package is based on the [R6
package for R](https://r6.r-lib.org/). This enables collaborators to
easily extend the existing models, for example, by varying the score
functions, or to add new model classes.

## Installation

The following command will install the development version of the R
package (requires previous installation of the [`remotes`
package](https://remotes.r-lib.org/index.html))

``` r
remotes::install_github("DoubleML/doubleml-for-r")
```

    ## Using github PAT from envvar GITHUB_PAT

    ## Skipping install of 'DoubleML' from a github remote, the SHA1 (80fb5ab4) has not changed since last install.
    ##   Use `force = TRUE` to force installation

Note that the package is still in an early phase of development and many
parts can be subject of changes.
