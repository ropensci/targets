format_init <- function(format = "rds") {
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  format_new(
    class = format[1],
    read = format_field(format, "^read="),
    write = format_field(format, "^write="),
    marshal = format_field(format, "^marshal="),
    unmarshal = format_field(format, "^unmarshal="),
    repository = keyvalue_field(format, "^repository=") %||% NULL
  )
}

format_new <- function(
  class = NULL,
  read = NULL,
  write = NULL,
  marshal = NULL,
  unmarshal = NULL,
  repository = NULL
) {
  force(class)
  force(read)
  force(write)
  force(marshal)
  force(unmarshal)
  force(repository)
  enclass(x = environment(), class = class)
}

format_field <- function(format, pattern) {
  out <- base64url::base64_urldecode(keyvalue_field(format, pattern))
  out %||% NULL
}

format_validate <- function(format) {
  tar_assert_correct_fields(format, format_new)
  class <- format$class
  tar_assert_inherits(format, class)
  tar_assert_chr(class)
  tar_assert_scalar(class)
  tar_assert_nzchar(class)
  for (field in setdiff(names(format), "class")) {
    value <- format[[field]]
    tar_assert_chr(value %|||% "x")
    tar_assert_scalar(value %|||% "x")
    tar_assert_nzchar(value %|||% "x")
  }
}
