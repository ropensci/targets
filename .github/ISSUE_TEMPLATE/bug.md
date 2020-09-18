---
name: Bug
about: Something is wrong with targets.
title: ""
labels: "type: bug"
assignees: wlandau
---

## Prework

* [ ] I understand and agree to the [code of conduct](https://github.com/wlandau/targets/blob/master/CODE_OF_CONDUCT.md).
* [ ] I understand and agree to the [contributing guidelines](https://github.com/wlandau/targets/blob/master/CONTRIBUTING.md).
* [ ] I am reasonably sure this is a genuine bug in `targets` and most likely not a user error. (If you run into an error and do not know the cause, please submit a "Trouble" issue instead.)
* [ ] Be considerate of the maintainer's time and make it as easy as possible to troubleshoot any problems you identify. **That means posting a complete, runnable, easy to understand [reproducible example](https://www.tidyverse.org/help/)**. Please format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Description

Please describe the bug.

## Reproducible example

Be considerate of the maintainer's time and make it as easy as possible to troubleshoot any problems you identify. **That means posting a complete, runnable, easy to understand [reproducible example](https://www.tidyverse.org/help/)**. Please format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Expected result

What should have happened? Please be as specific as possible.

## Diagnostic information

* A [reproducible example](https://github.com/tidyverse/reprex).
* Session info, available through `sessionInfo()` or [`reprex(si = TRUE)`](https://github.com/tidyverse/reprex).
* A stack trace from `traceback()` or `rlang::trace_back()`.
* The [SHA-1 hash](https://git-scm.com/book/en/v1/Getting-Started-Git-Basics#Git-Has-Integrity) of the GitHub commit of `targets` currently installed. `packageDescription("targets")$GithubSHA1` shows you this.
