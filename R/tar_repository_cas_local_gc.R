#' @title Local CAS garbage collection
#' @export
#' @family content-addressable storage
#' @description Garbage collection for a local content-addressable
#'   storage system.
#' @details Deletes all the files in the local CAS which are not in
#'   `tar_meta(targets_only = TRUE)$data`, including all locally saved
#'   historical data of the pipeline. This clears disk space, but
#'   at the expense of removing historical data and data from
#'   other colleagues who worked on the same project.
#' @inheritSection tar_repository_cas Content-addressable storage
#' @return `NULL` (invisibly). Called for its side effects.
#'   Removes files from the CAS repository at `path`.
#' @inheritParams tar_meta
#' @inheritParams tar_repository_cas_local
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   tar_option_set(seed = NA, repository = tar_repository_cas_local())
#'   list(tar_target(x, sample.int(n = 9e9, size = 1)))
#' })
#' for (index in seq_len(3)) tar_make(reporter = "silent")
#' list.files("_targets/cas")
#' tar_repository_cas_local_gc()
#' list.files("_targets/cas")
#' tar_meta(names = any_of("x"), fields = any_of("data"))
#' })
#' }
tar_repository_cas_local_gc <- function(
  path = NULL,
  store = targets::tar_config_get("store")
) {
  tar_assert_scalar(path %|||% "x")
  tar_assert_chr(path %|||% "x")
  tar_assert_nzchar(path %|||% "x")
  path <- path %|||% path_cas_dir(store)
  meta <- targets::tar_meta(
    fields = tidyselect::any_of("data"),
    targets_only = TRUE,
    store = store
  )
  keys <- list.files(path)
  remove <- setdiff(keys, meta$data)
  unlink(file.path(path, remove), force = TRUE, recursive = TRUE)
  invisible()
}
