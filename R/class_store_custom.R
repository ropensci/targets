#' @export
store_new.store_custom <- function(class, file = NULL, resources = NULL) {
  browser()
  store <- store_custom_new(file, resources)
  store_custom_enclass_repository(store)
}

store_custom_new <- function(file = NULL, resources = NULL) {
  force(file)
  force(resources)
  enclass(
    environment(),
    c("tar_external", "tar_store_custom", "tar_nonexportable", "tar_store")
  )
}

store_custom_enclass_repository <- function(store) {
  repository <- store$resources$store_custom$repository
  if (is.null(repository)) {
    return(store)
  }
  switch(
    repository,
    default = store,
    aws = enclass(store, c("tar_aws", "tar_cloud"))
  )
}

#' @export
store_assert_format_setting.store_custom <- function(class) {
}

#' @export
store_read_path.tar_store_custom <- function(store, path) {
  store_custom_encoded_call(
    fun = keyvalue_field(store$file$path, pattern = "^read="),
    args = list(path = path)
  )
}

#' @export
store_write_path.tar_store_custom <- function(store, object, path) {
  store_custom_encoded_call(
    fun = keyvalue_field(store$file$path, pattern = "^write="),
    args = list(object = object, path = path)
  )
}

#' @export
store_marshal_object.tar_store_custom <- function(store, object) {
  store_custom_encoded_call(
    fun = keyvalue_field(store$file$path, pattern = "^marshal="),
    args = list(object = object)
  )
}

#' @export
store_unmarshal_object.tar_store_custom <- function(store, object) {
  store_custom_encoded_call(
    fun = keyvalue_field(store$file$path, pattern = "^unmarshal="),
    args = list(object = object)
  )
}

store_custom_encoded_call <- function(fun, args) {
  envir <- new.env(parent= baseenv())
  text <- base64url::base64_urldecode(fun)
  what <- eval(parse(text = text), envir = envir)
  do.call(what = what, args = args, envir = envir)
}
