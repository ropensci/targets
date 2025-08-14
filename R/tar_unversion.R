#' @title Delete cloud object version IDs from local metadata.
#' @export
#' @family clean
#' @description Delete version IDs from local metadata.
#' @inheritSection tar_read Cloud target data versioning
#' @return `NULL` (invisibly).
#' @param names Tidyselect expression to identify the targets to drop
#'   version IDs.
#'   The object supplied to `names` should be `NULL` or a
#'   `tidyselect` expression like [any_of()] or [starts_with()]
#'   from `tidyselect` itself, or [tar_described_as()] to select target names
#'   based on their descriptions.
#' @inheritParams tar_validate
tar_unversion <- function(
  names = tidyselect::everything(),
  store = targets::tar_config_get("store")
) {
  tar_assert_allow_meta("tar_unversion", store)
  tar_assert_store(store = store)
  tar_assert_path(path_meta(store))
  meta <- meta_init(path_store = store)
  data <- as.data.frame(meta$database$read_condensed_data())
  names_quosure <- rlang::enquo(names)
  names <- tar_tidyselect_eval(names_quosure, data$name)
  tar_assert_chr(names, "names arg of tar_unversion() must eval to chr")
  replacement <- "version="
  pattern <- paste0("^", replacement, ".*")
  unversion <- data$name %in%
    names &
    !is.na(data$repository) &
    data$repository != "local"
  for (index in which(unversion)) {
    data$path[[index]] <- gsub(
      pattern = pattern,
      replacement = replacement,
      x = data$path[[index]]
    )
  }
  meta$database$overwrite_storage(data)
  invisible()
}
