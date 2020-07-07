---
name: Performance
about: "Runtime, memory, or storage inefficiency"
title: ""
labels: "topic: performance"
assignees: wlandau

---

## Prework

* [ ] I understand and agree to `targets`' [code of conduct](https://github.com/wlandau/targets/blob/master/CODE_OF_CONDUCT.md).
* [ ] I understand and agree to `targets`' [contributing guidelines](https://github.com/wlandau/targets/blob/master/CONTRIBUTING.md).

## Description

Please describe the performance issue.

## Reproducible example

Provide a minimal reproducible example with code and output that demonstrates the problem. The `reprex()` function from the [`reprex`](https://github.com/tidyverse/reprex) package is extremely helpful for this.

To help us read your code, please follow the [tidyverse style guide](https://style.tidyverse.org/). The `style_text()` and `style_file()` functions from the [`styler`](https://github.com/r-lib/styler) package make it easier.

## Benchmarks

How poorly does `targets` perform? To find out, we recommend the [`proffer`](https://github.com/wlandau/proffer) package and take screenshots of the results displayed in your browser.

```r
library(targets)
library(proffer)
px <- pprof({
  # All your targets code goes here.
})
```
