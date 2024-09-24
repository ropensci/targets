#' @title List saved targets
#' @export
#' @family storage
#' @description List targets currently saved to `_targets/objects/`
#'   or the cloud. Does not include local files
#'   with `tar_target(..., format = "file", repository = "local")`.
#' @inheritSection tar_meta Storage access
#' @return Character vector of targets saved to `_targets/objects/`.
#' @inheritParams tar_validate
#' @param names Names of targets to select.
#'   The object supplied to `names` should be `NULL` or a
#'   `tidyselect` expression like [any_of()] or [starts_with()]
#'   from `tidyselect` itself, or [tar_described_as()] to select target names
#'   based on their descriptions.
#' @param cloud Logical of length 1, whether to include
#'   cloud targets in the output
#'   (e.g. `tar_target(..., repository = "aws")`).
#' @examples
#' if (identical(Sys.getenv("TAR_EXAMPLES"), "true")) { # for CRAN
#' tar_dir({ # tar_dir() runs code from a temp dir for CRAN.
#' tar_script({
#'   library(targets)
#'   library(tarchetypes)
#'   list(tar_target(x, "value"))
#' }, ask = FALSE)
#' tar_make()
#' tar_objects()
#' tar_objects(starts_with("x")) # see also any_of()
#' })
#' }
tar_objects <- function(
  names = NULL,
  cloud = TRUE,
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_objects", store)
  if (!file.exists(store)) {
    return(character(0))
  }
  local <- if_any(
    dir.exists(path_objects_dir(store)),
    list.files(path_objects_dir(store), all.files = TRUE, no.. = TRUE),
    character(0)
  )
  names_quosure <- rlang::enquo(names)
  local <- tar_tidyselect_eval(names_quosure, local) %|||% local
  meta <- tar_meta(store = store)
  index <- !is.na(meta$repository) & (meta$repository != "local")
  meta <- meta[index,, drop = FALSE] # nolint
  names <- tar_tidyselect_eval(names_quosure, meta$name) %|||% meta$name
  remote <- character(0)
  if (cloud) {
    exists <- map_lgl(
      names,
      ~tar_exist_cloud_target(name = .x, meta = meta, path_store = store)
    )
    remote <- names[exists]
  }
  sort_chr(unique(as.character(c(local, remote))))
}
