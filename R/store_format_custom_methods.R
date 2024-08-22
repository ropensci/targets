store_format_custom_methods_init <- function(format) {
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  store_format_custom_methods_new(
    read = store_format_custom_field(
      format = format,
      pattern = "^read=",
      default = store_format_custom_default_read()
    ),
    write = store_format_custom_field(
      format = format,
      pattern = "^write=",
      default = store_format_custom_default_write()
    ),
    marshal = store_format_custom_field(
      format = format,
      pattern = "^marshal=",
      default = store_format_custom_default_marshal()
    ),
    unmarshal = store_format_custom_field(
      format = format,
      pattern = "^unmarshal=",
      default = store_format_custom_default_unmarshal()
    ),
    convert = store_format_custom_field(
      format = format,
      pattern = "^convert=",
      default = store_format_custom_default_convert()
    ),
    copy = store_format_custom_field(
      format = format,
      pattern = "^copy=",
      default = store_format_custom_default_copy()
    )
  )
}

store_format_custom_methods_new <- function(
  read = NULL,
  write = NULL,
  marshal = NULL,
  unmarshal = NULL,
  convert = NULL,
  copy = NULL
) {
  force(read)
  force(write)
  force(marshal)
  force(unmarshal)
  force(convert)
  force(copy)
  environment()
}

store_format_custom_methods_validate <- function(methods) {
  tar_assert_correct_fields(methods, store_format_custom_methods_new)
  for (field in c("read", "write", "marshal", "unmarshal", "convert")) {
    tar_assert_chr(methods[[field]])
    tar_assert_scalar(methods[[field]])
    tar_assert_nzchar(methods[[field]])
  }
}

store_format_custom_default_read <- function() {
  tar_deparse_safe(
    function(path) readRDS(path)
  )
}

store_format_custom_default_write <- function() {
  tar_deparse_safe(
    function(object, path) {
      saveRDS(object = object, file = path, version = 3L)
    }
  )
}

store_format_custom_default_marshal <- function() {
  tar_deparse_safe(
    function(object) object
  )
}

store_format_custom_default_unmarshal <- function() {
  tar_deparse_safe(
    function(object) object
  )
}

store_format_custom_default_convert <- function() {
  tar_deparse_safe(
    function(object) object
  )
}

store_format_custom_default_copy <- function() {
  tar_deparse_safe(
    function(object) object
  )
}
