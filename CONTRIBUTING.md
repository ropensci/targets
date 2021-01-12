# Contributing

Development is a community effort, and we welcome participation.

## Code of Conduct

By participating in this project, you agree to abide by the [code of conduct](https://github.com/ropensci/targets/blob/main/CODE_OF_CONDUCT.md).

## Issues

Anyone can submit an issue to <https://github.com/ropensci/targets/issues> or comment on an existing thread. Common reasons are to

* Suggest features.
* Report bugs.
* Request help with difficult use cases.
* Ask questions.

Please abide by the following guidelines.

* Before posting a new issue, please take a moment to search for existing similar issues in order to avoid duplication.
* For bug reports: if you can, please install the latest GitHub version of `targets` (i.e. `remotes::install_github("ropensci/targets")`) and verify that the issue still persists.
* Describe your issue in prose as clearly and concisely as possible.
* For any problem you identify, post a [minimal reproducible example](https://www.tidyverse.org/help/) like [this one](https://github.com/ropensci/targets/issues/256#issuecomment-754229683) so the maintainer can troubleshoot. A reproducible example is:
    * **Runnable**: post enough R code and data so any onlooker can create the error on their own computer.
    * **Minimal**: reduce runtime wherever possible and remove complicated details that are irrelevant to the issue at hand.
    * **Readable**: format your code according to the [tidyverse style guide](https://style.tidyverse.org/).

## Development

External code contributions are extremely helpful in the right circumstances. Here are the recommended steps.

1. Prior to contribution, please propose your idea in a [new issue thread](https://github.com/ropensci/targets/issues) so you and the maintainer can define the intent and scope of your work.
2. [Fork the repository](https://help.github.com/articles/fork-a-repo/).
3. Follow the [GitHub flow](https://guides.github.com/introduction/flow/index.html) to create a new branch, add commits, and open a pull request.
4. Discuss your code with the maintainer in the pull request thread.
5. If everything looks good, the maintainer will merge your code into the project.

Please also follow these additional guidelines.

* Respect the architecture and reasoning of the package. Depending on the scope of your work, you may want to read the design documents (package vignettes).
* If possible, keep contributions small enough to easily review manually. It is okay to split up your work into multiple pull requests.
* Format your code according to the [tidyverse style guide](https://style.tidyverse.org/) and check your formatting with the `lint_package()` function from the [`lintr`](https://github.com/jimhester/lintr) package.
* For new features or functionality, add tests in `tests`. Tests that can be automated should go in `tests/testthat/`. Tests that cannot be automated should go in `tests/interactive/`. For features affecting performance, it is good practice to add profiling studies to `tests/performance/`.
* Check code coverage with `covr::package_coverage()`. Automated tests should cover all the new or changed functionality in your pull request.
* Run overall package checks with `devtools::check()` and `goodpractice::gp()`
* Describe your contribution in the project's [`NEWS.md`](https://github.com/ropensci/targets/blob/main/NEWS.md) file. Be sure to mention relevent GitHub issue numbers and your GitHub name as done in existing news entries.
* If you feel contribution is substantial enough for official author or contributor status, please add yourself to the `Authors@R` field of the [`DESCRIPTION`](https://github.com/ropensci/targets/blob/main/DESCRIPTION) file.
