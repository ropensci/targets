dir_create <- function(x) {
  if (!file.exists(x)) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
  }
  invisible()
}

dir_create_runtime <- function(x) {
  if (is.null(tar_runtime$file_exist)) {
    dir_create(x)
    return()
  }
  if (!all(counter_exist_names(tar_runtime$file_exist, x))) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
    counter_set_names(tar_runtime$file_exist, x)
  }
  invisible()
}

file_exists_runtime <- function(x) {
  if (is.null(tar_runtime$file_exist)) {
    return(file.exists(x))
  }
  out <- counter_exist_names(tar_runtime$file_exist, x)
  out[!out] <- file.exists(x[!out])
  out
}

file_info_runtime <- function(x) {
  if_any(
    !is.null(tar_runtime$file_info) &&
      !is.null(tar_runtime$file_info_exist) &&
      all(counter_exist_names(tar_runtime$file_info_exist, x)),
    file_info_runtime_select(tar_runtime$file_info, x), # nolint
    file_info(x)
  )
}

file_info_runtime_select <- function(info, x) {
  list(size = info$size[x], mtime_numeric = info$mtime_numeric[x])
}

file_move <- function(from, to) {
  if (!suppressWarnings(file.rename(from = from, to = to))) {
    # Not feasible to test:
    # nocov start
    file.copy(from = from, to = to, overwrite = TRUE)
    # nocov end
  }
  invisible()
}
