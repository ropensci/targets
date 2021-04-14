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
#'   just feed them to [list()] in your `_targets.R` file.
#' @inheritParams tar_target
#' @inheritParams tar_option_set
#' @param name Character of length 1, name of the target. Subsequent targets
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
#' @param string Optional string representation of the command.
#'   Internally, the string gets hashed to check if the command changed
#'   since last run, which helps `targets` decide whether the
#'   target is up to date. External interfaces can take control of
#'   `string` to ignore changes in certain parts of the command.
#'   If `NULL`, the strings is just deparsed from `command` (default).
#' @examples
#'   # The following are equivalent.
#'   y <- tar_target(y, sqrt(x), pattern = map(x))
#'   y <- tar_target_raw("y", expression(sqrt(x)), expression(map(x)))
#'   # Programmatically create a chain of interdependent targets
#'   target_list <- lapply(seq_len(4), function(i) {
#'     tar_target_raw(
#'       letters[i + 1],
#'       substitute(do_something(x), env = list(x = as.symbol(letters[i])))
#'     )
#'   })
#'   print(target_list[[1]])
#'   print(target_list[[2]])
#' if (identical(Sys.getenv("TAR_LONG_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
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
  assert_nonmissing(name, "every target must have a name.")
  assert_chr(name, "name arg of tar_target_raw() must be character")
  assert_nzchar(name, "target name must be nonempty.")
  assert_nonmissing(command, paste("target", name, "has no command."))
  assert_chr(packages, "packages in tar_target_raw() must be character.")
  assert_chr(
    library %|||% character(0),
    "library in tar_target_raw() must be NULL or character."
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
  assert_list(
    resources,
    "resources in tar_target_raw() must be a named list."
  )
  assert_flag(storage, c("main", "worker"))
  assert_flag(retrieval, c("main", "worker"))
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
