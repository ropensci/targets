as_data_frame <- function(...) {
  as.data.frame(..., stringsAsFactors = FALSE)
}

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

tar_empty_envir <- new.env(parent = baseenv())

expand_grid <- function(...) {
  rev(expand.grid(rev(list(...)), stringsAsFactors = FALSE))
}

get_field <- function(field, collection) {
  collection[[field]]
}

replace_na <- function(x, y) {
  x[is.na(x)] <- y
  x
}

dir_create <- function(x) {
  if (!file.exists(x)) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
  }
  invisible()
}

mask_pointers <- function(x) {
  gsub("<pointer: 0x[0-9a-zA-Z]*>", "", x)
}

omit_rownames <- function(x) {
  rownames(x) <- NULL
  x
}

set_names <- function(x, names) {
  names(x) <- names
  x
}
