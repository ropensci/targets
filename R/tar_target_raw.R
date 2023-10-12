#' @title Define a target using unrefined names and language objects.
#' @export
#' @family targets
#' @description `tar_target_raw()` is just like [tar_target()] except
#'   it avoids non-standard evaluation for the arguments: `name`
#'   is a character string, `command` and `pattern` are language objects,
#'   and there is no `tidy_eval` argument. Use `tar_target_raw()`
#'   instead of [tar_target()] if you are creating entire batches
#'   of targets programmatically (metaprogramming, static branching).
#' @return A target object. Users should not modify these directly,
#'   just feed them to [list()] in your target script file
#'   (default: `_targets.R`).
#'   See the "Target objects" section for details.
#' @inheritSection tar_target Target objects
#' @inheritParams tar_target
#' @inheritParams tar_option_set
#' @param name Character of length 1, name of the target. A target
#'   name must be a valid name for a symbol in R, and it
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
#' @param command Similar to the `command` argument of [`tar_target()`] except
#'   the object must already be an expression instead of
#'   informally quoted code.
#'   `base::expression()` and `base::quote()` can produce such objects.
#' @param pattern Similar to the `pattern` argument of [`tar_target()`]
#'   except the object must already be an expression instead of
#'   informally quoted code.
#'   `base::expression()` and `base::quote()` can produce such objects.
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
#' # The following are equivalent.
#' y <- tar_target(y, sqrt(x), pattern = map(x))
#' y <- tar_target_raw("y", expression(sqrt(x)), expression(map(x)))
#' # Programmatically create a chain of interdependent targets
#' target_list <- lapply(seq_len(4), function(i) {
#'   tar_target_raw(
#'     letters[i + 1],
#'     substitute(do_something(x), env = list(x = as.symbol(letters[i])))
#'   )
#' })
#' print(target_list[[1]])
#' print(target_list[[2]])
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script(tar_target_raw("x", quote(1 + 1)), ask = FALSE)
#' tar_make()
#' tar_read(x)
#' })
#' }
tar_target_raw <- function(
  name,
  command,
  pattern = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  deps = NULL,
  string = NULL,
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
  cue = targets::tar_option_get("cue")
) {
  tar_assert_nonmissing(name)
  tar_assert_name(name)
  tar_assert_nonmissing(command, paste("target", name, "has no command."))
  if (is.expression(command)) {
    tar_assert_nonmissing(
      command[[1]],
      paste("target", name, "has no command.")
    )
  }
  tar_assert_scalar(
    as.expression(command),
    paste("the command of target", name, "must have length 1.")
  )
  tar_assert_chr(packages)
  tar_assert_chr(
    library %|||% character(0),
    "library in tar_target_raw() must be NULL or character."
  )
  tar_assert_format(format)
  tar_assert_repository(repository)
  if (format == "file_fast" && repository != "local") {
    format <- "file"
  }
  tar_assert_flag(iteration, c("vector", "list", "group"))
  tar_assert_flag(
    error,
    c("stop", "continue", "abridge", "workspace", "null")
  )
  deprecate_error_workspace(error)
  tar_assert_flag(memory, c("persistent", "transient"))
  tar_assert_lgl(garbage_collection)
  tar_assert_scalar(garbage_collection)
  tar_assert_flag(deployment, c("worker", "main"))
  tar_assert_dbl(priority)
  tar_assert_scalar(priority)
  tar_assert_ge(priority, 0)
  tar_assert_le(priority, 1)
  tar_assert_resources(resources)
  tar_assert_flag(storage, c("main", "worker", "none"))
  tar_assert_flag(retrieval, c("main", "worker", "none"))
  warn_error_format(error = error, format = format)
  if (!is.null(cue)) {
    cue_validate(cue)
  }
  target_init(
    name = name,
    expr = command,
    pattern = pattern,
    packages = packages,
    library = library,
    deps = deps,
    string = string,
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
    cue = cue
  )
}

warn_error_format <- function(error, format) {
  if (format %in% c("keras", "torch") && error == "null") {
    message <- paste0(
      "In targets, format = \"",
      format,
      "\" is incompatible with error = \"null\" ",
      "(and superseded by tar_format())."
    )
    tar_warn_validate(message)
  }
}
