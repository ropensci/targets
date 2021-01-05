
# targets <img src='man/figures/logo.png' align="right" height="139"/>

[![ropensci](https://badges.ropensci.org/401_status.svg)](https://github.com/ropensci/software-review/issues/401)
[![zenodo](https://zenodo.org/badge/200093430.svg)](https://zenodo.org/badge/latestdoi/200093430)
[![R
Targetopia](https://img.shields.io/badge/R_Targetopia-member-blue?style=flat&labelColor=gray)](https://wlandau.github.io/targetopia/)
[![cran](http://www.r-pkg.org/badges/version/targets)](https://cran.r-project.org/package=targets)
[![status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![check](https://github.com/wlandau/targets/workflows/check/badge.svg)](https://github.com/wlandau/targets/actions?query=workflow%3Acheck)
[![codecov](https://codecov.io/gh/wlandau/targets/branch/main/graph/badge.svg?token=3T5DlLwUVl)](https://codecov.io/gh/wlandau/targets)
[![lint](https://github.com/wlandau/targets/workflows/lint/badge.svg)](https://github.com/wlandau/targets/actions?query=workflow%3Alint)

The `targets` package is a
[Make](https://www.gnu.org/software/make/)-like pipeline toolkit for
Statistics and data science in R. With `targets`, you can maintain a
reproducible workflow without repeating yourself. `targets` learns how
your pipeline fits together, skips costly runtime for tasks that are
already up to date, runs only the necessary computation, supports
implicit parallel computing, abstracts files as R objects, and shows
tangible evidence that the results match the underlying code and data.
For deep dives into why and how to use `targets`, as well as help
transitioning from [`drake`](https://github.com/ropensci/drake), please
see the [user manual](https://wlandau.github.io/targets-manual).

## Installation

Install the GitHub development version to access the latest features and
patches.

``` r
library(remotes)
install_github("wlandau/targets")
```

## Help

  - Post to the [GitHub issue
    tracker](https://github.com/wlandau/targets/issues) to elicit help
    from the maintainer.
  - The [RStudio Community](https://community.rstudio.com/) forum is
    full of friendly enthusiasts of R and the tidyverse. Use the
    [`targets` tag](https://community.rstudio.com/tag/targets).
  - [Stack Overflow](https://stackoverflow.com/) broadcasts to the
    entire open source community. Use the [`targets-r-package`
    tag](https://stackoverflow.com/questions/tagged/targets-r-package).

## Documentation

  - [User manual](https://wlandau.github.io/targets-manual): in-depth
    discussion about how to use `targets`.
  - [Reference website](https://wlandau.github.io/targets/): formal
    documentation of all user-side functions, the statement of need, and
    multiple design documents of the internal architecture.

## Examples

  - [Minimal example](https://github.com/wlandau/targets-minimal).
  - [Machine learning with
    Keras](https://github.com/wlandau/targets-keras).
  - [Validating a Stan model](https://github.com/wlandau/targets-stan).

## Recorded talks

  - [R/Pharma 2020
    (9:24)](https://www.youtube.com/watch?v=GRqKJBaC5g4&list=PLMtxz1fUYA5C0YflXsR8EEAQXfjntlV1H&index=6)
  - [New York Open Statistical Programming Meetup, December 2020
    (1:54:28)](https://youtu.be/Gqn7Xn4d5NI)
  - [LA R Users Meetup, October 2020
    (1:14:40)](https://www.youtube.com/watch?v=Qq25BUxpJu4)

## Courses

  - [Official half-day interactive
    tutorial](https://github.com/wlandau/targets-tutorial).

## The R Targetopia

The R Targetopia consists of specialized workflow frameworks tailored to
individual fields of Statistics and data science, and they use domain
knowledge to abstract away most of the careful planning and engineering
typically required to write pipelines. They leverage the full power of
[`targets`](https://github.com/wlandau/targets) while requiring minimal
expertise with [`targets`](https://github.com/wlandau/targets) itself.
Examples include [`stantargets`](https://github.com/wlandau/stantargets)
and [`tarchetypes`](https://wlandau.github.io/tarchetypes).

## Apps

  - [`tar_watch()`](https://wlandau.github.io/targets/reference/tar_watch.html):
    a built-in Shiny app to visualize progress while a pipeline is
    running.
  - [`targetsketch`](https://wlandau.shinyapps.io/targetsketch): a Shiny
    app to help sketch pipelines
    ([app](https://wlandau.shinyapps.io/targetsketch),
    [source](https://github.com/wlandau/targetsketch)).

## Participation

Development is a community effort, and we welcome discussion and
contribution. By participating in this project, you agree to abide by
the [code of
conduct](https://github.com/wlandau/targets/blob/main/CODE_OF_CONDUCT.md)
and the [contributing
guide](https://github.com/wlandau/targets/blob/main/CONTRIBUTING.md).

## Citation

``` r
citation("targets")
#> Warning in citation("targets"): no date field in DESCRIPTION file of package
#> 'targets'
#> 
#> To cite package 'targets' in publications use:
#> 
#>   William Michael Landau (2021). targets: Dynamic Function-Oriented
#>   'Make'-Like Declarative Workflows.
#>   https://wlandau.github.io/targets/,
#>   https://github.com/wlandau/targets.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {targets: Dynamic Function-Oriented 'Make'-Like Declarative Workflows},
#>     author = {William Michael Landau},
#>     year = {2021},
#>     note = {https://wlandau.github.io/targets/,
#> https://github.com/wlandau/targets},
#>   }
```
