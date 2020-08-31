capitalize <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

string_sub_expression <- function(x) {
  x <- gsub("^expression\\(", "", x)
  x <- gsub("\\)$", "", x)
  x
}

produce_lines <- function(x) {
  lines <- unlist(strsplit(as.character(x), split = "\n"))
  trn(
    length(x),
    paste(lines, collapse = "\n    "),
    deparse_safe(x)
  )
}

paste_list <- function(x) {
  trn(
    length(x),
    paste0(names(x), ": ", x),
    list()
  )
}
