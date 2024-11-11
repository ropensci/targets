as_data_frame <- function(...) {
  as.data.frame(..., stringsAsFactors = FALSE)
}

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

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

keyvalue_field <- function(x, pattern) {
  element <- grep(pattern = pattern, x = x, value = TRUE)
  gsub(pattern = pattern, replacement = "", x = element)
}

omit_na <- function(x) {
  x[!is.na(x)]
}

omit_null <- function(x) {
  x[!map_lgl(x, is.null)]
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

sort_chr <- function(x) {
  old_locale <- Sys.getlocale(category = "LC_COLLATE")
  on.exit(Sys.setlocale(category = "LC_COLLATE", locale = old_locale))
  Sys.setlocale(category = "LC_COLLATE", locale = "C")
  sort.int(
    x = as.character(x),
    decreasing = FALSE,
    na.last = NA,
    method = "radix",
    index.return = FALSE
  )
}

enclass <- function(x, class) {
  class(x) <- c(class, class(x))
  x
}

supported_args <- function(fun, args) {
  args <- omit_null(args)
  common <- intersect(names(formals(fun)), names(args))
  args[common]
}

tar_envir_base <- new.env(parent = baseenv())

tar_envir_empty <- new.env(parent = emptyenv())
