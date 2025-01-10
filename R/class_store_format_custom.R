#' @export
store_class_format.format_custom <- function(format) {
  store_class_format_format_custom
}

store_class_format_format_custom <- c(
  "tar_store_format_custom",
  "tar_nonexportable",
  "tar_store"
)

store_format_custom_field <- function(format, pattern, default, prefix) {
  out <- base64url::base64_urldecode(keyvalue_field(format, pattern))
  if ((length(out) < 1L) || !any(nzchar(out))) {
    return(default)
  } else {
    return(paste0(prefix, out))
  }
}

#' @export
store_assert_format_setting.format_custom <- function(format) {
}

#' @export
store_read_path.tar_store_format_custom <- function(store, path) {
  store_format_custom_call_method(
    store = store,
    text = store$methods_format$read,
    args = list(path = path)
  )
}

#' @export
store_write_path.tar_store_format_custom <- function(store, object, path) {
  store_format_custom_call_method(
    store = store,
    text = store$methods_format$write,
    args = list(object = object, path = path)
  )
}

#' @export
store_marshal_object.tar_store_format_custom <- function(store, object) {
  store_format_custom_call_method(
    store = store,
    text = store$methods_format$marshal,
    args = list(object = object)
  )
}

#' @export
store_unmarshal_object.tar_store_format_custom <- function(store, object) {
  store_format_custom_call_method(
    store = store,
    text = store$methods_format$unmarshal,
    args = list(object = object)
  )
}

#' @export
store_convert_object.tar_store_format_custom <- function(store, object) {
  store_format_custom_call_method(
    store = store,
    text = store$methods_format$convert,
    args = list(object = object)
  )
}

#' @export
store_copy_object.tar_store_format_custom <- function(store, object) {
  store_format_custom_call_method(
    store = store,
    text = store$methods_format$copy,
    args = list(object = object)
  )
}

store_format_custom_call_method <- function(store, text, args) {
  envvars <- store$resources$custom_format$envvars
  if (length(envvars)) {
    names <- names(envvars)
    previous <- Sys.getenv(names, names = TRUE)
    on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
    do.call(what = Sys.setenv, args = as.list(envvars))
  }
  envir <- new.env(parent = baseenv())
  what <- eval(parse(text = text, keep.source = FALSE), envir = envir)
  do.call(what = what, args = args, envir = envir)
}

store_format_custom_old_repository <- function(format) {
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  value <- grep("^repository=", format, value = TRUE)
  value <- gsub("^repository=", "", value)
  value %||% "local"
}
