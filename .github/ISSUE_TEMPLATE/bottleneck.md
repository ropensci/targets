---
name: Performance
about: "Runtime, memory, or storage inefficiency"
title: ""
labels: "topic: performance"
assignees: wlandau

---

## Prework

* [ ] I understand and agree to the [code of conduct](https://github.com/wlandau/targets/blob/master/CODE_OF_CONDUCT.md).
* [ ] I understand and agree to the [contributing guidelines](https://github.com/wlandau/targets/blob/master/CONTRIBUTING.md).
* [ ] Be considerate of the maintainer's time and make it as easy as possible to troubleshoot any problems you identify. **That means posting a complete, runnable, easy to understand [reproducible example](https://www.tidyverse.org/help/)**. Please format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Description

Please describe the performance issue.

## Reproducible example

Be considerate of the maintainer's time and make it as easy as possible to troubleshoot any problems you identify. **That means posting a complete, runnable, easy to understand [reproducible example](https://www.tidyverse.org/help/)**. Please format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Benchmarks

How poorly does `targets` perform? To find out, we recommend the [`proffer`](https://github.com/wlandau/proffer) package and take screenshots of the results displayed in your browser.

```r
library(targets)
library(proffer)
px <- pprof({
  # All your targets code goes here.
})
```
