---
name: Performance
about: "Runtime, memory, or storage inefficiency"
title: ""
labels: "topic: performance"
assignees: wlandau

---

## Prework

* [ ] Read and agree to the [code of conduct](https://github.com/wlandau/targets/blob/master/CODE_OF_CONDUCT.md) and [contributing guidelines](https://github.com/wlandau/targets/blob/master/CONTRIBUTING.md).
* [ ] Post a [minimal reproducible example](https://www.tidyverse.org/help/) so the maintainer can troubleshoot the problems you identify. A reproducible example is:
    * [ ] **Runnable**: post enough R code and data so any onlooker can create the error on their own computer.
    * [ ] **Minimal**: reduce runtime wherever possible and remove complicated details that are irrelevant to the issue at hand.
    * [ ] **Readable**: format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Description

Please describe the performance issue.

## Reproducible example

* [ ] Post a [minimal reproducible example](https://www.tidyverse.org/help/) so the maintainer can troubleshoot the problems you identify. A reproducible example is:
    * [ ] **Runnable**: post enough R code and data so any onlooker can create the error on their own computer.
    * [ ] **Minimal**: reduce runtime wherever possible and remove complicated details that are irrelevant to the issue at hand.
    * [ ] **Readable**: format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Benchmarks

How poorly does `targets` perform? To find out, we recommend the [`proffer`](https://github.com/wlandau/proffer) package and take screenshots of the results displayed in your browser.

```r
library(targets)
library(proffer)
px <- pprof({
  # All your targets code goes here.
})
```
