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
* [ ] Be considerate of the maintainer's time and make it as easy as possible to troubleshoot any problems you identify. Read [here](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) and [here](https://www.tidyverse.org/help/) to learn about minimal reproducible examples. Format your code according to the [tidyverse style guide](https://style.tidyverse.org/) to make it easier for others to read.

## Description

Please describe the performance issue.

## Reproducible example

Be considerate of the maintainer's time and make it as easy as possible to troubleshoot any problems you identify. Read [here](https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example) and [here](https://www.tidyverse.org/help/) to learn about minimal reproducible examples. Format your code according to the [tidyverse style guide](https://style.tidyverse.org/) to make it easier for others to read.

## Benchmarks

How poorly does `targets` perform? To find out, we recommend the [`proffer`](https://github.com/wlandau/proffer) package and take screenshots of the results displayed in your browser.

```r
library(targets)
library(proffer)
px <- pprof({
  # All your targets code goes here.
})
```
