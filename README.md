
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DoubleML - Double Machine Learning in python and R

The R package **DoubleML** provides an implementation of the double /
debiased machine learning framework of [Chernozhukov et
al. (2018)](https://arxiv.org/abs/1608.00060). It is built on top of
[mlr3](https://mlr3.mlr-org.com/) and the [mlr3
ecosystem](https://github.com/mlr-org/mlr3/wiki/Extension-Packages).

Note that the R package was developed together with a python twin based
on [scikit-learn](https://scikit-learn.org/). The python package is also
available on [GitHub](https://github.com/DoubleML/doubleml-for-py).

## Documentation and maintenance

Documentation of functions in R: `here`

User guide: <http://doubleml.org>

**DoubleML** is currently maintained by
[`@MalteKurz`](https://github.com/MalteKurz) and
[`@PhilippBach`](https://github.com/PhilippBach).

## Main Features

Double / debiased machine learning framework of [Chernozhukov et
al. (2018)](https://arxiv.org/abs/1608.00060) for

  - Partially linear regression models (PLR)
  - Partially linear IV regression models (PLIV)
  - Interactive regression models (IRM)
  - Interactive IV regression models (IIVM)

The object-oriented implementation of **DoubleML** that is based on the
[R6 package for R](https://r6.r-lib.org/) is very flexible. The model
classes `DoubleMLPLR`, `DoubleMLPLIV`, `DoubleMLIRM` and `DoubleIIVM`
implement the estimation of the nuisance functions via machine learning
methods and the computation of the Neyman orthogonal score function. All
other functionalities are implemented in the abstract base class
`DoubleML`. In particular functionalities to estimate double machine
learning models and to perform statistical inference via the methods
`fit`, `bootstrap`, `confint`, `p_adjust` and `tune`. This
object-oriented implementation allows a high flexibility for the model
specification in terms of …

  - … the machine learners for the nuisance functions,
  - … the resampling schemes,
  - … the double machine learning algorithm,
  - … the Neyman orthogonal score functions,
  - …

It further can be readily extended with regards to

  - … new model classes that come with Neyman orthogonal score functions
    being linear in the target parameter,
  - … alternative score functions via callables,
  - … alternative resampling schemes,
  - …

## Installation

**DoubleML** requires

  - R
  - R6
  - mlr3
  - mlr3tuning
  - mlr3learners
  - data.table
  - paradox
  - stats
  - utils
  - clusterGeneration
  - foreign (\<= 0.8-76)

We plan to push a first release of the **DoubleML** package to CRAN very
soon.

Until then we recommend to install from source via

``` r
remotes::install_github("DoubleML/doubleml-for-r")
```

## References

  - Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
    C., Newey, W. and Robins, J. (2018), Double/debiased machine
    learning for treatment and structural parameters. The Econometrics
    Journal, 21: C1-C68. <doi:10.1111/ectj.12097>.
