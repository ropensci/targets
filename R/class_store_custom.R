#' @export
store_new.format_custom <- function(format, file = NULL, resources = NULL) {
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  store <- store_custom_new(
    file = file,
    resources = resources,
    read = store_custom_field(
      format = format,
      pattern = "^read=",
      default = store_custom_default_read()
    ),
    write = store_custom_field(
      format = format,
      pattern = "^write=",
      default = store_custom_default_write()
    ),
    marshal = store_custom_field(
      format = format,
      pattern = "^marshal=",
      default = store_custom_default_marshal()
    ),
    unmarshal = store_custom_field(
      format = format,
      pattern = "^unmarshal=",
      default = store_custom_default_unmarshal()
    ),
    convert = store_custom_field(
      format = format,
      pattern = "^convert=",
      default = store_custom_default_convert()
    ),
    copy = store_custom_field(
      format = format,
      pattern = "^copy=",
      default = store_custom_default_copy()
    )
  )
}

#' @export
store_class_format.format_custom <- function(format) {
  c("tar_store_custom", "tar_nonexportable", "tar_store")
}

store_custom_new <- function(
  file = NULL,
  resources = NULL,
  read = NULL,
  write = NULL,
  marshal = NULL,
  unmarshal = NULL,
  convert = NULL,
  copy = NULL
) {
  force(file)
  force(resources)
  force(read)
  force(write)
  force(marshal)
  force(unmarshal)
  force(convert)
  force(copy)
  enclass(
    environment(),
    store_class_format.format_custom(NULL)
  )
}

store_custom_field <- function(format, pattern, default) {
  out <- base64url::base64_urldecode(keyvalue_field(format, pattern))
  if ((length(out) < 1L) || !any(nzchar(out))) {
    out <- default
  }
  out
}

#' @export
store_assert_format_setting.format_custom <- function(format) {
}

#' @export
store_read_path.tar_store_custom <- function(store, path) {
  store_custom_call_method(
    store = store,
    text = store$read,
    args = list(path = path)
  )
}

#' @export
store_write_path.tar_store_custom <- function(store, object, path) {
  store_custom_call_method(
    store = store,
    text = store$write,
    args = list(object = object, path = path)
  )
}

#' @export
store_marshal_object.tar_store_custom <- function(store, object) {
  store_custom_call_method(
    store = store,
    text = store$marshal,
    args = list(object = object)
  )
}

#' @export
store_unmarshal_object.tar_store_custom <- function(store, object) {
  store_custom_call_method(
    store = store,
    text = store$unmarshal,
    args = list(object = object)
  )
}

#' @export
store_convert_object.tar_store_custom <- function(store, object) {
  store_custom_call_method(
    store = store,
    text = store$convert,
    args = list(object = object)
  )
}

#' @export
store_copy_object.tar_store_custom <- function(store, object) {
  store_custom_call_method(
    store = store,
    text = store$copy,
    args = list(object = object)
  )
}

store_custom_call_method <- function(store, text, args) {
  envvars <- store$resources$custom_format$envvars
  if (length(envvars)) {
    names <- names(envvars)
    previous <- Sys.getenv(names, names = TRUE)
    on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
    do.call(what = Sys.setenv, args = as.list(envvars))
  }
  envir <- new.env(parent = baseenv())
  what <- eval(parse(text = text), envir = envir)
  do.call(what = what, args = args, envir = envir)
}

#' @export
store_validate.tar_store_custom <- function(store) {
  tar_assert_correct_fields(store, store_custom_new)
  store_validate_packages(store)
  tar_assert_list(store$resources)
  for (field in c("read", "write", "marshal", "unmarshal", "convert")) {
    tar_assert_chr(store[[field]])
    tar_assert_scalar(store[[field]])
    tar_assert_nzchar(store[[field]])
  }
}

store_custom_old_repository <- function(format) {
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  value <- grep("^repository=", format, value = TRUE)
  value <- gsub("^repository=", "", value)
  value %||% "local"
}

store_custom_default_read <- function() {
  tar_deparse_safe(
    function(path) readRDS(path)
  )
}

store_custom_default_write <- function() {
  tar_deparse_safe(
    function(object, path) {
      saveRDS(object = object, file = path, version = 3L)
    }
  )
}

store_custom_default_marshal <- function() {
  tar_deparse_safe(
    function(object) object
  )
}

store_custom_default_unmarshal <- function() {
  tar_deparse_safe(
    function(object) object
  )
}

store_custom_default_convert <- function() {
  tar_deparse_safe(
    function(object) object
  )
}

store_custom_default_copy <- function() {
  tar_deparse_safe(
    function(object) object
  )
}
