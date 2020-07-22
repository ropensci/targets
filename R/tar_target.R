#' @title Declare a target.
#' @export
#' @description A target is a single step of computation in a pipeline.
#'   It runs an R command (`expr` argument) and returns a value.
#'   This value gets treated as an R object that can be used
#'   by the commands of targets downstream. Targets that
#'   are already up to date are skipped. See the user manual
#'   for more details.
#' @return A target object. Users should not modify these directly,
#'   just feed them to [tar_pipeline()] in your `_targets.R` file.
#' @param name Symbol, name of the target.
#' @param command R code to run the target.
#' @param pattern Language to define branching for a target.
#'   For example, in a pipeline with numeric vector targets `x` and `y`,
#'   `tar_target(z, x + y, pattern = map(x, y))` implicitly defines
#'   branches of `z` that each compute `x[1] + y[1]`, `x[2] + y[2]`,
#'   and so on. See the user manual for details.
#' @param tidy_eval Logical, whether to enable tidy evaluation
#'   when interpreting `expr`. If `TRUE`, you can use the
#'   "bang-bang" operator `!!` to programmatically insert
#'   the values of global objects.
#' @param packages Character vector of packages to load right before
#'   the target builds. Use `tar_options()` to set packages
#'   globally for all subsequent targets you define.
#' @param library Character vector of library paths to try
#'   when loading `packages`.
#' @param format Optional storage format for the target's return value.
#'   With the exception of `format = "file"`, each target
#'   gets a file in `_targets/objects`, and each format is a different
#'   way to save and load this file.
#'   Possible formats:
#'   * `"file"`: A dynamic file. To use this format,
#'     the target needs to manually identify or save some data
#'     and return a character vector of paths
#'     to the data. Those paths must point to files or directories,
#'     and they must not contain characters `|` or `*`.
#'     Then, `targets` automatically checks those files and cues the
#'     appropriate build decisions if those files are out of date.
#'   * `"rds"`: Default, uses `saveRDS()` and `readRDS()`. Should work for
#'     most objects, but slow.
#'   * `"qs"`: Uses `qs::qsave()` and `qs::qread()`. Should work for
#'     most objects, much faster than `"rds"`.
#'   * `"fst"`: Uses `fst::write_fst()` and `fst::read_fst()`.
#'     Much faster than `"rds"`, but the value must be
#'     a data frame.
#'   * `"fst_dt"`: Same as `"fst"`, but the value is a `data.table`.
#'   * `"fst_tbl"`: Same as `"fst"`, but the value is a `tibble`.
#'   * `"keras"`: Uses `keras::save_model_hdf5()` and
#'     `keras::load_model_hdf5()`. The value must be a Keras model.
#' @param iteration Character of length 1, name of the iteration mode
#'   of the target. Choices:
#'   * `"vector"`: branching happens with `vectors::vec_slice()` and
#'     aggregation happens with `vctrs::vec_c()`.
#'   * `"list"`, branching happens with `[[]]` and aggregation happens with
#'     `list()`.
#'   * `"group"`: `dplyr::group_by()`-like functionality to branch over
#'     subsets of a data frame. The target's return value must be a data
#'     frame with a special `tar_group` column of consecutive integers
#'     from 1 through the number of groups. Each integer designates a group,
#'     and a branch is created for each collection of rows in a group.
#'     See the [tar_group()] function to see how you can
#'     create the special `tar_group` column with `dplyr::group_by()`.
#' @param error Character of length 1, what to do if the target
#'   runs into an error. If `"stop"`, the whole pipeline stops
#'   and throws an error. If `"continue"`, the error is recorded,
#'   but the pipeline keeps going.
#' @param memory Character of length 1, memory strategy.
#'   If `"persistent"`, the target stays in memory
#'   until the end of the pipeline.
#'   If `"transient"`, the target gets unloaded
#'   after every new target completes.
#'   Either way, the target gets automatically loaded into memory
#'   whenever another target needs the value.
#' @param deployment Character of length 1, only relevant to
#'   [tar_make_clustermq()] and [tar_make_future()]. If `"remote"`,
#'   the target builds on a remote parallel worker. If `"local"`,
#'   the target builds on the host machine / process managing the pipeline.
#' @param priority Numeric of length 1 between 0 and 1. Controls which
#'   targets get deployed first when multiple competing targets are ready
#'   simultaneously. Targets with priorities closer to 1 get built earlier.
#' @param template Relevant to [tar_make_clustermq()] only.
#'   Named list of values to fill in the `clustermq` template file.
#'   Unsupported for now. May be supported in the future if
#'   `clustermq` ever supports heterogeneous workers with varying
#'   resource requirements. In the meantime, use the `template`
#'   argument of [tar_make_clustermq()].
#' @param resources Relevant to [tar_make_future()] only.
#'   A named list of resources passed to `future::future()` when
#'   defining a new worker.
#' @param storage Character of length 1, only relevant to
#'   [tar_make_clustermq()] and [tar_make_future()].
#'   If `"local"`, the target's return value is sent back to the
#'   host machine and saved locally. If `"remote"`, the remote worker
#'   saves the value.
#' @param retrieval Character of length 1, only relevant to
#'   [tar_make_clustermq()] and [tar_make_future()].
#'   If `"local"`, the target's dependencies are loaded on the host machine
#'   and sent to the remote worker before the target builds.
#'   If `"remote"`, the remote worker loads the targets dependencies.
#' @param cue An optional object from `tar_cue()` to customize the
#'   rules that decide whether the target is up to date.
#' @examples
#' # Defining targets does not run them.
#' data <- tar_target(target_name, get_data(), packages = "tidyverse")
#' analysis <- tar_target(analysis, analyze(x), pattern = map(x))
#' # Pipelines accept targets.
#' pipeline <- tar_pipeline(data, analysis)
#' # Tidy evaluation
#' tar_options()
#' n_rows <- 30L
#' data <- tar_target(target_name, get_data(!!n_rows))
#' print(data)
#' # Disable tidy evaluation:
#' data <- tar_target(target_name, get_data(!!n_rows), tidy_eval = FALSE)
#' print(data)
tar_target <- function(
  name,
  command,
  pattern = NULL,
  tidy_eval = targets::tar_option("tidy_eval", TRUE),
  packages = targets::tar_option("packages", (.packages())),
  library = targets::tar_option("library"),
  format = targets::tar_option("format", "rds"),
  iteration = targets::tar_option("iteration", "vector"),
  error = targets::tar_option("error", "stop"),
  memory = targets::tar_option("memory", "persistent"),
  template = targets::tar_option("template", NULL),
  deployment = targets::tar_option("deployment", "remote"),
  priority = 0,
  resources = targets::tar_option("resources", list()),
  storage = targets::tar_option("storage", "local"),
  retrieval = targets::tar_option("retrieval", storage),
  cue = targets::tar_option("cue", NULL)
) {
  name <- deparse_language(substitute(name))
  assert_chr(name, "name arg of tar_target() must be a symbol")
  assert_lgl(tidy_eval, "tidy_eval in tar_target() must be logical.")
  assert_chr(packages, "packages in tar_target() must be character.")
  assert_chr(
    library %||% character(0),
    "library in tar_target() must be NULL or character."
  )
  format <- match.arg(format, store_formats())
  iteration <- match.arg(iteration, c("vector", "list", "group"))
  error <- match.arg(error, c("stop", "continue"))
  memory <- match.arg(memory, c("persistent", "transient"))
  deployment <- match.arg(deployment, c("remote", "local"))
  assert_scalar(priority)
  assert_ge(priority, 0)
  assert_le(priority, 1)
  warn_template(template)
  assert_list(resources, "resources in tar_target() must be a named list.")
  storage <- match.arg(storage, c("local", "remote"))
  retrieval <- match.arg(retrieval, c("local", "remote"))
  if (!is.null(cue)) {
    cue_validate(cue)
  }
  expr <- as.expression(substitute(command))
  envir <- tar_option("envir", globalenv())
  if (tidy_eval) {
    expr <- as.call(c(quote(rlang::expr), expr))
    expr <- rlang::quo_squash(eval(expr, envir = envir))
  }
  pattern <- substitute(pattern)
  target_init(
    name = name,
    expr = expr,
    pattern = pattern,
    packages = packages,
    library = library,
    envir = envir,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    deployment = deployment,
    priority = priority,
    template = template,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}
