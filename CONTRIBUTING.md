# Contributing

Development is a community effort, and we welcome participation.

## Code of Conduct

By participating in this project, you agree to abide by the [code of conduct](https://github.com/wlandau/targets/blob/master/CODE_OF_CONDUCT.md).

## Issues

Anyone can submit an issue to <https://github.com/wlandau/targets/issues> or comment on an existing thread. Common reasons are to

* Suggest features.
* Report bugs.
* Request help with difficult use cases.
* Ask questions.

Please abide by the following guidelines.

* Before posting a new issue, please take a moment to search for existing similar issues in order to avoid duplication.
* For bug reports: if you can, please install the latest GitHub version of `targets` (i.e. `remotes::install_github("wlandau/targets")`) and verify that the issue still persists.
* Describe your issue in prose as clearly and concisely as possible. The following three-paragraph format is preferred.
    1. State the general problem or question in 1-2 matter-of-fact high-level sentences.
    2. Explain supporting context and details necessary to understand paragraph 1.
    3. Suggest solutions, implementation details, or other ideas if you have them.
* Include diagnostic details about the problem, including
    * A [reproducible example](https://github.com/tidyverse/reprex).
    * Session info, available through `sessionInfo()` or [`reprex(si = TRUE)`](https://github.com/tidyverse/reprex).
    * A stack trace from `traceback()` or `rlang::trace_back()`.
    * The [SHA-1 hash](https://git-scm.com/book/en/v1/Getting-Started-Git-Basics#Git-Has-Integrity) of the GitHub commit of `targets` currently installed. `packageDescription("targets")$GithubSHA1` shows you this.

## Development

External code contributions are extremely helpful in the right circumstances. Here are the recommended steps.

1. Prior to contribution, please propose your idea in a [new issue thread](https://github.com/wlandau/targets/issues) so you and the maintainer can define the intent and scope of your work.
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
* Describe your contribution in the project's [`NEWS.md`](https://github.com/wlandau/targets/blob/master/NEWS.md) file. Be sure to mention relevent GitHub issue numbers and your GitHub name as done in existing news entries.
* If you feel contribution is substantial enough for official author or contributor status, please add yourself to the `Authors@R` field of the [`DESCRIPTION`](https://github.com/wlandau/targets/blob/master/DESCRIPTION) file.
