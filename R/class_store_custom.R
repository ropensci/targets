#' @export
store_new.format_custom <- function(format, file = NULL, resources = NULL) {
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  store <- store_custom_new(
    file = file,
    resources = resources,
    read = store_custom_field(format, "^read="),
    write = store_custom_field(format, "^write="),
    marshal = store_custom_field(format, "^marshal="),
    unmarshal = store_custom_field(format, "^unmarshal="),
    repository = keyvalue_field(format, "^repository=")
  )
  store_custom_enclass_repository(store)
}

store_custom_new <- function(
  file = NULL,
  resources = NULL,
  read = NULL,
  write = NULL,
  marshal = NULL,
  unmarshal = NULL,
  repository = NULL
) {
  force(file)
  force(resources)
  force(read)
  force(write)
  force(marshal)
  force(unmarshal)
  force(repository)
  enclass(
    environment(),
    c("tar_store_custom", "tar_nonexportable", "tar_store")
  )
}

store_custom_field <- function(format, pattern) {
  out <- base64url::base64_urldecode(keyvalue_field(format, pattern))
  out %||% NULL
}

store_custom_enclass_repository <- function(store) {
  switch(
    store$repository,
    default = store,
    aws = enclass(
      store,
      c("tar_aws_store_custom", "tar_aws", "tar_cloud", "tar_external")
    )
  )
}

#' @export
store_assert_format_setting.format_custom <- function(format) {
}

#' @export
store_read_path.tar_store_custom <- function(store, path) {
  store_custom_call_method(
    text = store$read,
    args = list(path = path)
  )
}

#' @export
store_write_path.tar_store_custom <- function(store, object, path) {
  store_custom_call_method(
    text = store$write,
    args = list(object = object, path = path)
  )
}

#' @export
store_marshal_object.tar_store_custom <- function(store, object) {
  store_custom_call_method(
    text = store$marshal,
    args = list(object = object)
  )
}

#' @export
store_unmarshal_object.tar_store_custom <- function(store, object) {
  store_custom_call_method(
    text = store$unmarshal,
    args = list(object = object)
  )
}

store_custom_call_method <- function(text, args) {
  envir <- new.env(parent = baseenv())
  what <- eval(parse(text = text), envir = envir)
  do.call(what = what, args = args, envir = envir)
}

#' @export
store_validate.tar_store_custom <- function(store) {
  tar_assert_correct_fields(store, store_custom_new)
  store_validate_packages(store)
  tar_assert_list(store$resources)
  for (field in c("read", "write", "marshal", "unmarshal")) {
    tar_assert_chr(store[[field]])
    tar_assert_scalar(store[[field]])
    tar_assert_nzchar(store[[field]])
  }
}
