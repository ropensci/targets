#' @title Currently running `targets` function.
#' @export
#' @family utilities
#' @description Get the name of the currently running `targets`
#'   interface function. Returns `NULL` if not invoked inside
#'   a target or `_targets.R` (i.e. if not directly invoked
#'   by [tar_make()], [tar_visnetwork()], etc.).
#' @return Character of length 1, name of the currently running `targets`
#'   interface function. For example, suppose you have a call to
#'   `tar_function()` inside a target or `_targets.R`. Then if you run
#'   `tar_make()`, `tar_function()` will return `"tar_make"`.
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) {
#' tar_dir({ # tar_dir() runs code from a temporary directory.
#' tar_function() # NULL
#' tar_script({
#'   message("called function: ", tar_function())
#'   tar_target(x, tar_function())
#' })
#' tar_manifest() # prints "called function: tar_manifest"
#' tar_make() # prints "called function: tar_make"
#' tar_read(x) # "tar_make"
#' })
#' }
tar_function <- function() {
  tar_runtime$get_fun()
}
