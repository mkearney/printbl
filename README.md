
<!-- README.md is generated from README.Rmd. Please edit that file -->

# printbl

<!--[![Build status](https://travis-ci.org/mkearney/printbl.svg?branch=master)](https://travis-ci.org/mkearney/printbl)
[![CRAN status](https://www.r-pkg.org/badges/version/printbl)](https://cran.r-project.org/package=printbl)
[![Coverage Status](https://codecov.io/gh/mkearney/printbl/branch/master/graph/badge.svg)](https://codecov.io/gh/mkearney/printbl?branch=master)

#![Downloads](https://cranlogs.r-pkg.org/badges/printbl)
#![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/printbl)-->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

> Printable Tibbles

## Installation

Install the development version from Github with:

``` r
## install remotes pkg if not already
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}

## install from github
remotes::install_github("mkearney/printbl")
```

### Problem

A limitation of tibble printing is that variables are hidden if they
cannot fit within the device’s width.

### Solution

**{printbl}** solves this problem via **printing rows**. This means
users get to preview ALL of the data while preserving the pretty
printing behaviors of tibbles.

## Example

``` r
## load {printbl} package
library(printbl)

## generate a data frame with lots of variables
rt <- rtweet::search_tweets("lang:en -filter:quote", include_rts = FALSE)

## select non-ID columns
x <- rt[, c(3, 6:30, 61:69, 76:82)]

## print rows
print_tbl_df(x)
```

<p align="center">

<img width="90%" src="tools/readme/sss.png">

</p>
