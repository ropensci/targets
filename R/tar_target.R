#' @title Declare a target.
#' @export
#' @family targets
#' @description A target is a single step of computation in a pipeline.
#'   It runs an R command and returns a value.
#'   This value gets treated as an R object that can be used
#'   by the commands of targets downstream. Targets that
#'   are already up to date are skipped. See the user manual
#'   for more details.
#'
#'   [tar_target()] defines a target using non-standard evaluation.
#'   The `name` argument is an unevaluated symbol,
#'   and the `command` and `pattern`
#'   arguments are unevaluated expressions. Example:
#'   `tar_target(name = data, command = get_data())`.
#'
#'   [tar_target_raw()] defines a target with standard evaluation.
#'   The `name` argument is a character string,
#'   and the `command` and `pattern`
#'   arguments are evaluated expressions. Example:
#'   `tar_target_raw(name = "data", command = quote(get_data()))`.
#'   [tar_target_raw()] also has extra arguments `deps` and `string`
#'   for advanced customization.
#' @section Target objects:
#'   Functions like `tar_target()` produce target objects,
#'   special objects with specialized sets of S3 classes.
#'   Target objects represent skippable steps of the analysis pipeline
#'   as described at <https://books.ropensci.org/targets/>.
#'   Please read the walkthrough at
#'   <https://books.ropensci.org/targets/walkthrough.html>
#'   to understand the role of target objects in analysis pipelines.
#'
#'   For developers,
#'   <https://wlandau.github.io/targetopia/contributing.html#target-factories>
#'   explains target factories (functions like this one which generate targets)
#'   and the design specification at
#'   <https://books.ropensci.org/targets-design/>
#'   details the structure and composition of target objects.
#' @section Storage formats:
#'   `targets` has several built-in storage formats to control how return
#'   values are saved and loaded from disk:
#'
#'   * `"rds"`: Default, uses `saveRDS()` and `readRDS()`. Should work for
#'     most objects, but slow.
#'   * `"auto"`: either `"file"` or `"qs"`, depending on the return value
#'     of the target. If the return value is a character vector of
#'     existing files (and/or directories), then the format becomes
#'     `"file"` before [tar_make()] saves the target. Otherwise,
#'     the format becomes `"qs"`.
#'   * `"qs"`: Uses `qs::qsave()` and `qs::qread()`. Should work for
#'     most objects, much faster than `"rds"`. Optionally set the
#'     preset for `qsave()` through `tar_resources()` and `tar_resources_qs()`.
#'   * `"feather"`: Uses `arrow::write_feather()` and
#'     `arrow::read_feather()` (version 2.0). Much faster than `"rds"`,
#'     but the value must be a data frame. Optionally set
#'     `compression` and `compression_level` in `arrow::write_feather()`
#'     through `tar_resources()` and `tar_resources_feather()`.
#'     Requires the `arrow` package (not installed by default).
#'   * `"parquet"`: Uses `arrow::write_parquet()` and
#'     `arrow::read_parquet()` (version 2.0). Much faster than `"rds"`,
#'     but the value must be a data frame. Optionally set
#'     `compression` and `compression_level` in `arrow::write_parquet()`
#'     through `tar_resources()` and `tar_resources_parquet()`.
#'     Requires the `arrow` package (not installed by default).
#'   * `"fst"`: Uses `fst::write_fst()` and `fst::read_fst()`.
#'     Much faster than `"rds"`, but the value must be
#'     a data frame. Optionally set the compression level for
#'     `fst::write_fst()` through `tar_resources()` and `tar_resources_fst()`.
#'     Requires the `fst` package (not installed by default).
#'   * `"fst_dt"`: Same as `"fst"`, but the value is a `data.table`.
#'     Deep copies are made as appropriate in order to protect
#'     against the global effects of in-place modification.
#'     Optionally set the compression level the same way as for `"fst"`.
#'   * `"fst_tbl"`: Same as `"fst"`, but the value is a `tibble`.
#'     Optionally set the compression level the same way as for `"fst"`.
#'   * `"keras"`: superseded by [tar_format()] and incompatible
#'     with `error = "null"` (in [tar_target()] or [tar_option_set()]).
#'     Uses `keras::save_model_hdf5()` and
#'     `keras::load_model_hdf5()`. The value must be a Keras model.
#'     Requires the `keras` package (not installed by default).
#'   * `"torch"`: superseded by [tar_format()] and incompatible
#'     with `error = "null"` (in [tar_target()] or [tar_option_set()]).
#'     Uses `torch::torch_save()` and `torch::torch_load()`.
#'     The value must be an object from the `torch` package
#'     such as a tensor or neural network module.
#'     Requires the `torch` package (not installed by default).
#'   * `"file"`: A dynamic file. To use this format,
#'     the target needs to manually identify or save some data
#'     and return a character vector of paths
#'     to the data (must be a single file path if `repository`
#'     is not `"local"`). (These paths must be existing files
#'     and nonempty directories.)
#'     Then, `targets` automatically checks those files and cues
#'     the appropriate run/skip decisions if those files are out of date.
#'     Those paths must point to files or directories,
#'     and they must not contain characters `|` or `*`.
#'     All the files and directories you return must actually exist,
#'     or else `targets` will throw an error. (And if `storage` is `"worker"`,
#'     `targets` will first stall out trying to wait for the file
#'     to arrive over a network file system.)
#'     If the target does not create any files, the return value should be
#'     `character(0)`.
#'
#'     If `repository` is not `"local"` and `format` is `"file"`,
#'     then the character vector returned by the target must be of length 1
#'     and point to a single file. (Directories and vectors of multiple
#'     file paths are not supported for dynamic files on the cloud.)
#'     That output file is uploaded to the cloud and tracked for changes
#'     where it exists in the cloud. The local file is deleted after
#'     the target runs.
#'   * `"url"`: A dynamic input URL. For this storage format,
#'     `repository` is implicitly `"local"`,
#'     URL format is like `format = "file"`
#'     except the return value of the target is a URL that already exists
#'     and serves as input data for downstream targets. Optionally
#'     supply a custom `curl` handle through
#'     `tar_resources()` and `tar_resources_url()`.
#'     in `new_handle()`, `nobody = TRUE` is important because it
#'     ensures `targets` just downloads the metadata instead of
#'     the entire data file when it checks time stamps and hashes.
#'     The data file at the URL needs to have an ETag or a Last-Modified
#'     time stamp, or else the target will throw an error because
#'     it cannot track the data. Also, use extreme caution when
#'     trying to use `format = "url"` to track uploads. You must be absolutely
#'     certain the ETag and Last-Modified time stamp are fully updated
#'     and available by the time the target's command finishes running.
#'     `targets` makes no attempt to wait for the web server.
#'   * A custom format can be supplied with `tar_format()`. For this choice,
#'     it is the user's responsibility to provide methods for (un)serialization
#'     and (un)marshaling the return value of the target.
#'   * The formats starting with `"aws_"` are deprecated as of 2022-03-13
#'     (`targets` version > 0.10.0). For cloud storage integration, use the
#'     `repository` argument instead.
#'
#'   Formats `"rds"`, `"file"`, and `"url"` are general-purpose formats
#'   that belong in the `targets` package itself.
#'   Going forward, any additional formats should be implemented with
#'   [tar_format()] in third-party packages like `tarchetypes`
#'   and `geotargets` (for example: `tarchetypes::tar_format_nanoparquet()`).
#'   Formats `"qs"`, `"fst"`, etc. are legacy formats from before the
#'   existence of [tar_format()], and they will continue to remain in
#'   `targets` without deprecation.
#' @param repository Character of length 1, remote repository for target
#'   storage. Choices:
#'   * `"local"`: file system of the local machine.
#'   * `"aws"`: Amazon Web Services (AWS) S3 bucket. Can be configured
#'     with a non-AWS S3 bucket using the `endpoint` argument of
#'     [tar_resources_aws()], but versioning capabilities may be lost
#'     in doing so.
#'     See the cloud storage section of
#'     <https://books.ropensci.org/targets/data.html>
#'     for details for instructions.
#'   * `"gcp"`: Google Cloud Platform storage bucket.
#'     See the cloud storage section of
#'     <https://books.ropensci.org/targets/data.html>
#'     for details for instructions.
#'   * A character string from [tar_repository_cas()] for content-addressable
#'     storage.
#'
#'   Note: if `repository` is not `"local"` and `format` is `"file"`
#'   then the target should create a single output file.
#'   That output file is uploaded to the cloud and tracked for changes
#'   where it exists in the cloud. The local file is deleted after
#'   the target runs.
#' @param iteration Character of length 1, name of the iteration mode
#'   of the target. Choices:
#'   * `"vector"`: branching happens with `vctrs::vec_slice()` and
#'     aggregation happens with `vctrs::vec_c()`.
#'   * `"list"`, branching happens with `[[]]` and aggregation happens with
#'     `list()`.
#'   * `"group"`: `dplyr::group_by()`-like functionality to branch over
#'     subsets of a non-dynamic data frame.
#'     For `iteration = "group"`, the target must not by dynamic
#'     (the `pattern` argument of [tar_target()] must be left `NULL`).
#'     The target's return value must be a data
#'     frame with a special `tar_group` column of consecutive integers
#'     from 1 through the number of groups. Each integer designates a group,
#'     and a branch is created for each collection of rows in a group.
#'     See the [tar_group()] function to see how you can
#'     create the special `tar_group` column with `dplyr::group_by()`.
#' @return A target object. Users should not modify these directly,
#'   just feed them to [list()] in your target script file
#'   (default: `_targets.R`).
#' @param name Symbol, name of the target.
#'   In [tar_target()], `name` is an unevaluated symbol, e.g.
#'   `tar_target(name = data)`.
#'   In [tar_target_raw()], `name` is a character string, e.g.
#'   `tar_target_raw(name = "data")`.
#'
#'   A target name must be a valid name for a symbol in R, and it
#'   must not start with a dot. Subsequent targets
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
#'   with `tar_meta(your_target, seed)` and run [tar_seed_set()]
#'   on the result to locally recreate the target's initial RNG state.
#' @param command R code to run the target.
#'   In [tar_target()], `command` is an unevaluated expression, e.g.
#'   `tar_target(command = data)`.
#'   In [tar_target_raw()], `command` is an evaluated expression, e.g.
#'   `tar_target_raw(command = quote(data))`.
#' @param pattern Code to define a dynamic branching branching for a target.
#'   In [tar_target()], `pattern` is an unevaluated expression, e.g.
#'   `tar_target(pattern = map(data))`.
#'   In [tar_target_raw()], `command` is an evaluated expression, e.g.
#'   `tar_target_raw(pattern = quote(map(data)))`.
#'
#'   To demonstrate dynamic branching patterns, suppose we have
#'   a pipeline with numeric vector targets `x` and `y`. Then,
#'   `tar_target(z, x + y, pattern = map(x, y))` implicitly defines
#'   branches of `z` that each compute `x[1] + y[1]`, `x[2] + y[2]`,
#'   and so on. See the user manual for details.
#' @param tidy_eval Logical, whether to enable tidy evaluation
#'   when interpreting `command` and `pattern`. If `TRUE`, you can use the
#'   "bang-bang" operator `!!` to programmatically insert
#'   the values of global objects.
#' @param packages Character vector of packages to load right before
#'   the target runs or the output data is reloaded for
#'   downstream targets. Use `tar_option_set()` to set packages
#'   globally for all subsequent targets you define.
#' @param library Character vector of library paths to try
#'   when loading `packages`.
#' @param format Optional storage format for the target's return value.
#'   With the exception of `format = "file"`, each target
#'   gets a file in `_targets/objects`, and each format is a different
#'   way to save and load this file. See the "Storage formats" section
#'   for a detailed list of possible data storage formats.
#' @param error Character of length 1, what to do if the target
#'   stops and throws an error. Options:
#'   * `"stop"`: the whole pipeline stops and throws an error.
#'   * `"continue"`: the whole pipeline keeps going.
#'   * `"null"`: The errored target continues and returns `NULL`.
#'     The data hash is deliberately wrong so the target is not
#'     up to date for the next run of the pipeline.
#'   * `"abridge"`: any currently running targets keep running,
#'     but no new targets launch after that.
#'   * `"trim"`: all currently running targets stay running. A queued
#'     target is allowed to start if:
#'
#'       1. It is not downstream of the error, and
#'       2. It is not a sibling branch from the same [tar_target()] call
#'         (if the error happened in a dynamic branch).
#'
#'     The idea is to avoid starting any new work that the immediate error
#'     impacts. `error = "trim"` is just like `error = "abridge"`,
#'     but it allows potentially healthy regions of the dependency graph
#'     to begin running.
#'   (Visit <https://books.ropensci.org/targets/debugging.html>
#'   to learn how to debug targets using saved workspaces.)
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
#'   For cloud-based dynamic files
#'   (e.g. `format = "file"` with `repository = "aws"`),
#'   this memory strategy applies to the
#'   temporary local copy of the file:
#'   `"persistent"` means it remains until the end of the pipeline
#'   and is then deleted,
#'   and `"transient"` means it gets deleted as soon as possible.
#'   The former conserves bandwidth,
#'   and the latter conserves local storage.
#' @param garbage_collection Logical, whether to run `base::gc()`
#'   just before the target runs.
#' @param deployment Character of length 1. If `deployment` is
#'   `"main"`, then the target will run on the central controlling R process.
#'   Otherwise, if `deployment` is `"worker"` and you set up the pipeline
#'   with distributed/parallel computing, then
#'   the target runs on a parallel worker. For more on distributed/parallel
#'   computing in `targets`, please visit
#'   <https://books.ropensci.org/targets/crew.html>.
#' @param priority Numeric of length 1 between 0 and 1. Controls which
#'   targets get deployed first when multiple competing targets are ready
#'   simultaneously. Targets with priorities closer to 1 get dispatched earlier
#'   (and polled earlier in [tar_make_future()]).
#' @param resources Object returned by `tar_resources()`
#'   with optional settings for high-performance computing
#'   functionality, alternative data storage formats,
#'   and other optional capabilities of `targets`.
#'   See `tar_resources()` for details.
#' @param storage Character of length 1, only relevant to
#'   [tar_make_clustermq()] and [tar_make_future()].
#'   Must be one of the following values:
#'   * `"main"`: the target's return value is sent back to the
#'   host machine and saved/uploaded locally.
#'   * `"worker"`: the worker saves/uploads the value.
#'   * `"none"`: almost never recommended. It is only for
#'     niche situations, e.g. the data needs to be loaded
#'     explicitly from another language. If you do use it,
#'     then the return value of the target is totally ignored
#'     when the target ends, but
#'     each downstream target still attempts to load the data file
#'     (except when `retrieval = "none"`).
#'
#'     If you select `storage = "none"`, then
#'     the return value of the target's command is ignored,
#'     and the data is not saved automatically.
#'     As with dynamic files (`format = "file"`) it is the
#'     responsibility of the user to write to
#'     the data store from inside the target.
#'
#'     The distinguishing feature of `storage = "none"`
#'     (as opposed to `format = "file"`)
#'     is that in the general case,
#'     downstream targets will automatically try to load the data
#'     from the data store as a dependency. As a corollary, `storage = "none"`
#'     is completely unnecessary if `format` is `"file"`.
#' @param retrieval Character of length 1, only relevant to
#'   [tar_make_clustermq()] and [tar_make_future()].
#'   Must be one of the following values:
#'   * `"main"`: the target's dependencies are loaded on the host machine
#'     and sent to the worker before the target runs.
#'   * `"worker"`: the worker loads the targets dependencies.
#'   * `"none"`: the dependencies are not loaded at all.
#'     This choice is almost never recommended. It is only for
#'     niche situations, e.g. the data needs to be loaded
#'     explicitly from another language.
#' @param cue An optional object from `tar_cue()` to customize the
#'   rules that decide whether the target is up to date.
#' @param description Character of length 1, a custom free-form human-readable
#'   text description of the target. Descriptions appear as target labels
#'   in functions like [tar_manifest()] and [tar_visnetwork()],
#'   and they let you select subsets of targets for the `names` argument of
#'   functions like [tar_make()]. For example,
#'   `tar_manifest(names = tar_described_as(starts_with("survival model")))`
#'   lists all the targets whose descriptions start with the character
#'   string `"survival model"`.
#' @param deps Optional character vector of the adjacent upstream
#'   dependencies of the target, including targets and global objects.
#'   If `NULL`, dependencies are resolved automatically as usual.
#'   The `deps` argument is only for developers of extension
#'   packages such as `tarchetypes`,
#'   not for end users, and it should almost never be used at all.
#'   In scenarios that at first appear to requires `deps`,
#'   there is almost always a simpler and more robust workaround
#'   that avoids setting `deps`.
#' @param string Optional string representation of the command.
#'   Internally, the string gets hashed to check if the command changed
#'   since last run, which helps `targets` decide whether the
#'   target is up to date. External interfaces can take control of
#'   `string` to ignore changes in certain parts of the command.
#'   If `NULL`, the strings is just deparsed from `command` (default).
#' @examples
#' # Defining targets does not run them.
#' data <- tar_target(target_name, get_data(), packages = "tidyverse")
#' analysis <- tar_target(analysis, analyze(x), pattern = map(x))
#' # In a pipeline:
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(
#'     tar_target(name = x, command = 1 + 1),
#'     tar_target_raw(name = "y", command = quote(x + y))
#'   )
#' })
#' tar_make()
#' tar_read(x)
#' })
#' # Tidy evaluation
#' tar_option_set(envir = environment())
#' n_rows <- 30L
#' data <- tar_target(target_name, get_data(!!n_rows))
#' print(data)
#' # Disable tidy evaluation:
#' data <- tar_target(target_name, get_data(!!n_rows), tidy_eval = FALSE)
#' print(data)
#' tar_option_reset()
#' }
tar_target <- function(
  name,
  command,
  pattern = NULL,
  tidy_eval = targets::tar_option_get("tidy_eval"),
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = targets::tar_option_get("garbage_collection"),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  name <- tar_deparse_language(substitute(name))
  tar_assert_chr(name)
  tar_assert_nzchar(name)
  tar_assert_lgl(tidy_eval)
  envir <- tar_option_get("envir")
  command <- as.expression(substitute(command))
  tar_assert_nonmissing(command[[1]], paste("target", name, "has no command."))
  command <- tar_tidy_eval(command, envir, tidy_eval)
  pattern <- as.expression(substitute(pattern))
  pattern <- tar_tidy_eval(pattern, envir, tidy_eval)
  tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = packages,
    library = library,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}
