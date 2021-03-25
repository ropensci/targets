#' @title Declare a target.
#' @export
#' @description A target is a single step of computation in a pipeline.
#'   It runs an R command and returns a value.
#'   This value gets treated as an R object that can be used
#'   by the commands of targets downstream. Targets that
#'   are already up to date are skipped. See the user manual
#'   for more details.
#' @return A target object. Users should not modify these directly,
#'   just feed them to [list()] in your `_targets.R` file.
#' @param name Symbol, name of the target. Subsequent targets
#'   can refer to this name symbolically to induce a dependency relationship:
#'   e.g. `tar_target(downstream_target, f(upstream_target))` is a
#'   target named `downstream_target` which depends on a target
#'   `upstream_target` and a function `f()`. In addition, a target's
#'   name determines its random number generator seed. In this way,
#'   each target runs with a reproducible seed so someone else
#'   running the same pipeline should get the same results,
#'   and no two targets in the same pipeline share the same seed.
#'   (Even dynamic branches have different names and thus different seeds.)
#'   You can recover the seed of a completed target
#'   with `tar_meta(your_target, seed)` and run `set.seed()` on the result
#'   to locally recreate the target's initial RNG state.
#' @param command R code to run the target.
#' @param pattern Language to define branching for a target.
#'   For example, in a pipeline with numeric vector targets `x` and `y`,
#'   `tar_target(z, x + y, pattern = map(x, y))` implicitly defines
#'   branches of `z` that each compute `x[1] + y[1]`, `x[2] + y[2]`,
#'   and so on. See the user manual for details.
#' @param tidy_eval Logical, whether to enable tidy evaluation
#'   when interpreting `command` and `pattern`. If `TRUE`, you can use the
#'   "bang-bang" operator `!!` to programmatically insert
#'   the values of global objects.
#' @param packages Character vector of packages to load right before
#'   the target builds. Use `tar_option_set()` to set packages
#'   globally for all subsequent targets you define.
#' @param library Character vector of library paths to try
#'   when loading `packages`.
#' @param format Optional storage format for the target's return value.
#'   With the exception of `format = "file"`, each target
#'   gets a file in `_targets/objects`, and each format is a different
#'   way to save and load this file.
#'   Possible formats:
#'   * `"rds"`: Default, uses `saveRDS()` and `readRDS()`. Should work for
#'     most objects, but slow.
#'   * `"qs"`: Uses `qs::qsave()` and `qs::qread()`. Should work for
#'     most objects, much faster than `"rds"`. Optionally set the
#'     preset for `qsave()` through the `resources` argument, e.g.
#'     `tar_target(..., resources = list(preset = "archive"))`.
#'     Requires the `qs` package (not installed by default).
#'   * `"feather"`: Uses `arrow::write_feather()` and
#'     `arrow::read_feather()` (version 2.0). Much faster than `"rds"`,
#'     but the value must be a data frame. Optionally set
#'     `compression` and `compression_level` in `arrow::write_feather()`
#'     through the `resources` argument, e.g.
#'     `tar_target(..., resources = list(compression = ...))`.
#'     Requires the `arrow` package (not installed by default).
#'   * `"parquet"`: Uses `arrow::write_parquet()` and
#'     `arrow::read_parquet()` (version 2.0). Much faster than `"rds"`,
#'     but the value must be a data frame. Optionally set
#'     `compression` and `compression_level` in `arrow::write_parquet()`
#'     through the `resources` argument, e.g.
#'     `tar_target(..., resources = list(compression = ...))`.
#'     Requires the `arrow` package (not installed by default).
#'   * `"fst"`: Uses `fst::write_fst()` and `fst::read_fst()`.
#'     Much faster than `"rds"`, but the value must be
#'     a data frame. Optionally set the compression level for
#'     `fst::write_fst()` through the `resources` argument, e.g.
#'     `tar_target(..., resources = list(compress = 100))`.
#'     Requires the `fst` package (not installed by default).
#'   * `"fst_dt"`: Same as `"fst"`, but the value is a `data.table`.
#'     Optionally set the compression level the same way as for `"fst"`.
#'   * `"fst_tbl"`: Same as `"fst"`, but the value is a `tibble`.
#'     Optionally set the compression level the same way as for `"fst"`.
#'   * `"keras"`: Uses `keras::save_model_hdf5()` and
#'     `keras::load_model_hdf5()`. The value must be a Keras model.
#'     Requires the `keras` package (not installed by default).
#'   * `"torch"`: Uses `torch::torch_save()` and `torch::torch_load()`.
#'     The value must be an object from the `torch` package
#'     such as a tensor or neural network module.
#'     Requires the `torch` package (not installed by default).
#'   * `"file"`: A dynamic file. To use this format,
#'     the target needs to manually identify or save some data
#'     and return a character vector of paths
#'     to the data. (These paths must be existing files
#'     and nonempty directories.)
#'     Then, `targets` automatically checks those files and cues
#'     the appropriate build decisions if those files are out of date.
#'     Those paths must point to files or directories,
#'     and they must not contain characters `|` or `*`.
#'     All the files and directories you return must actually exist,
#'     or else `targets` will throw an error. (And if `storage` is `"worker"`,
#'     `targets` will first stall out trying to wait for the file
#'     to arrive over a network file system.)
#'   * `"url"`: A dynamic input URL. It works like `format = "file"`
#'     except the return value of the target is a URL that already exists
#'     and serves as input data for downstream targets. Optionally
#'     supply a custom `curl` handle through the `resources` argument, e.g.
#'     `tar_target(..., resources = list(handle = curl::new_handle()))`.
#'     The data file at the URL needs to have an ETag or a Last-Modified
#'     time stamp, or else the target will throw an error because
#'     it cannot track the data. Also, use extreme caution when
#'     trying to use `format = "url"` to track uploads. You must be absolutely
#'     certain the ETag and Last-Modified time stamp are fully updated
#'     and available by the time the target's command finishes running.
#'     `targets` makes no attempt to wait for the web server.
#'   * `"aws_rds"`, `"aws_qs"`, `"aws_parquet"`, `"aws_fst"`, `"aws_fst_dt"`,
#'     `"aws_fst_tbl"`, `"aws_keras"`: AWS-powered versions of the
#'     respective formats `"rds"`, `"qs"`, etc. The only difference
#'     is that the data file is uploaded to the AWS S3 bucket
#'     you supply to `resources`. See the cloud computing chapter
#'     of the manual for details.
#'   * `"aws_file"`: arbitrary dynamic files on AWS S3. The target
#'     should return a path to a temporary local file, then
#'     `targets` will automatically upload this file to an S3
#'     bucket and track it for you. Unlike `format = "file"`,
#'     `format = "aws_file"` can only handle one single file,
#'     and that file must not be a directory.
#'     [tar_read()] and downstream targets
#'     download the file to `_targets/scratch/` locally and return the path.
#'     `_targets/scratch/` gets deleted at the end of [tar_make()].
#'     Requires the same `resources` and other configuration details
#'     as the other AWS-powered formats. See the cloud computing
#'     chapter of the manual for details.
#' @param iteration Character of length 1, name of the iteration mode
#'   of the target. Choices:
#'   * `"vector"`: branching happens with `vctrs::vec_slice()` and
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
#'   until the end of the pipeline (unless `storage` is `"worker"`,
#'   in which case `targets` unloads the value from memory
#'   right after storing it in order to avoid sending
#'   copious data over a network).
#'   If `"transient"`, the target gets unloaded
#'   after every new target completes.
#'   Either way, the target gets automatically loaded into memory
#'   whenever another target needs the value.
#'   For cloud-based dynamic files such as `format = "aws_file"`,
#'   this memory policy applies to
#'   temporary local copies of the file in `_targets/scratch/"`:
#'   `"persistent"` means they remain until the end of the pipeline,
#'   and `"transient"` means they get deleted from the file system
#'   as soon as possible. The former conserves bandwidth,
#'   and the latter conserves local storage.
#' @param garbage_collection Logical, whether to run `base::gc()`
#'   just before the target runs.
#' @param deployment Character of length 1, only relevant to
#'   [tar_make_clustermq()] and [tar_make_future()]. If `"worker"`,
#'   the target builds on a parallel worker. If `"main"`,
#'   the target builds on the host machine / process managing the pipeline.
#' @param priority Numeric of length 1 between 0 and 1. Controls which
#'   targets get deployed first when multiple competing targets are ready
#'   simultaneously. Targets with priorities closer to 1 get built earlier
#'   (and polled earlier in [tar_make_future()]).
#'   Only applies to [tar_make_future()] and [tar_make_clustermq()]
#'   (not [tar_make()]). [tar_make_future()] with no extra settings is
#'   a drop-in replacement for [tar_make()] in this case.
#' @param resources A named list of computing resources. Uses:
#'   * Template file wildcards for `future::future()` in [tar_make_future()].
#'   * Template file wildcards `clustermq::workers()` in [tar_make_clustermq()].
#'   * Custom target-level `future::plan()`, e.g.
#'     `resources = list(plan = future.callr::callr)`.
#'   * Custom `curl` handle if `format = "url"`,
#'     e.g. `resources = list(handle = curl::new_handle(nobody = TRUE))`.
#'     In custom handles, most users should manually set `nobody = TRUE`
#'     so `targets` does not download the entire file when it
#'     only needs to check the time stamp and ETag.
#'   * Custom preset for `qs::qsave()` if `format = "qs"`, e.g.
#'     `resources = list(handle = "archive")`.
#'   * Arguments `compression` and `compression_level` to
#'     `arrow::write_feather()` and `arrow:write_parquet()` if `format` is
#'     `"feather"`, `"parquet"`, `"aws_feather"`, or `"aws_parquet"`.
#'   * Custom compression level for `fst::write_fst()` if
#'     `format` is `"fst"`, `"fst_dt"`, or `"fst_tbl"`, e.g.
#'     `resources = list(compress = 100)`.
#'   * AWS bucket and prefix for the `"aws_"` formats, e.g.
#'     `resources = list(bucket = "your-bucket", prefix = "folder/name")`.
#'     `bucket` is required for AWS formats. See the cloud computing chapter
#'     of the manual for details.
#' @param storage Character of length 1, only relevant to
#'   [tar_make_clustermq()] and [tar_make_future()].
#'   If `"main"`, the target's return value is sent back to the
#'   host machine and saved locally. If `"worker"`, the worker
#'   saves the value.
#' @param retrieval Character of length 1, only relevant to
#'   [tar_make_clustermq()] and [tar_make_future()].
#'   If `"main"`, the target's dependencies are loaded on the host machine
#'   and sent to the worker before the target builds.
#'   If `"worker"`, the worker loads the targets dependencies.
#' @param cue An optional object from `tar_cue()` to customize the
#'   rules that decide whether the target is up to date.
#' @examples
#' # Defining targets does not run them.
#' data <- tar_target(target_name, get_data(), packages = "tidyverse")
#' analysis <- tar_target(analysis, analyze(x), pattern = map(x))
#' # Pipelines accept targets.
#' pipeline <- list(data, analysis)
#' # Tidy evaluation
#' tar_option_set(envir = environment())
#' n_rows <- 30L
#' data <- tar_target(target_name, get_data(!!n_rows))
#' print(data)
#' # Disable tidy evaluation:
#' data <- tar_target(target_name, get_data(!!n_rows), tidy_eval = FALSE)
#' print(data)
#' tar_option_reset()
#' # In a pipeline:
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_script(tar_target(x, 1 + 1), ask = FALSE)
#' tar_make()
#' tar_read(x)
#' })
#' }
tar_target <- function(
  name,
  command,
  pattern = NULL,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue")
) {
  name <- deparse_language(substitute(name))
  assert_chr(name, "name arg of tar_target() must be a symbol")
  assert_lgl(tidy_eval, "tidy_eval in tar_target() must be logical.")
  assert_chr(packages, "packages in tar_target() must be character.")
  assert_chr(
    library %|||% character(0),
    "library in tar_target() must be NULL or character."
  )
  assert_format(format)
  assert_flag(iteration, c("vector", "list", "group"))
  assert_flag(error, c("stop", "continue", "workspace"))
  assert_flag(memory, c("persistent", "transient"))
  assert_lgl(garbage_collection, "garbage_collection must be logical.")
  assert_scalar(garbage_collection, "garbage_collection must be a scalar.")
  assert_flag(deployment, c("worker", "main"))
  assert_dbl(priority)
  assert_scalar(priority)
  assert_ge(priority, 0)
  assert_le(priority, 1)
  assert_list(resources, "resources in tar_target() must be a named list.")
  assert_flag(storage, c("main", "worker"))
  assert_flag(retrieval, c("main", "worker"))
  if (!is.null(cue)) {
    cue_validate(cue)
  }
  envir <- tar_option_get("envir")
  expr <- as.expression(substitute(command))
  pattern <- as.expression(substitute(pattern))
  target_init(
    name = name,
    expr = tar_tidy_eval(expr, envir, tidy_eval),
    pattern = tar_tidy_eval(pattern, envir, tidy_eval),
    packages = packages,
    library = library,
    format = format,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue
  )
}
