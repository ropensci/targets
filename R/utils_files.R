dir_create <- function(x) {
  if (!file.exists(x)) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
  }
  invisible()
}

file_exists_runtime <- function(x) {
  out <- counter_exist_names(tar_runtime$file_exist, x)
  out[!out] <- file.exists(x[!out])
  out
}

file_info_runtime <- function(x) {
  if_any(
    all(counter_exist_names(tar_runtime$file_info_exist, x)),
    tar_runtime$file_info[x,, drop = FALSE], # nolint
    file.info(x, extra_cols = FALSE)
  )
}
