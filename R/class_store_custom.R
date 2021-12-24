#' @export
store_new.custom <- function(class, file = NULL, resources = NULL) {
  definition <- unlist(class)
  store <- store_custom_new(
    file = file,
    resources = resources,
    read = keyvalue_field(definition, "^read="),
    write = keyvalue_field(definition, "^write="),
    marshal = keyvalue_field(definition, "^marshal="),
    unmarshal = keyvalue_field(definition, "^unmarshal="),
    repository = keyvalue_field(definition, "^repository=")
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
store_assert_format_setting.custom <- function(class) {
}

#' @export
store_read_path.tar_store_custom <- function(store, path) {
  store_custom_call_method(
    code = store$read,
    args = list(path = path)
  )
}

#' @export
store_write_path.tar_store_custom <- function(store, object, path) {
  store_custom_call_method(
    code = store$write,
    args = list(object = object, path = path)
  )
}

#' @export
store_marshal_object.tar_store_custom <- function(store, object) {
  store_custom_call_method(
    code = store$marshal,
    args = list(object = object)
  )
}

#' @export
store_unmarshal_object.tar_store_custom <- function(store, object) {
  store_custom_call_method(
    code = store$unmarshal,
    args = list(object = object)
  )
}

store_custom_call_method <- function(code, args) {
  text <- base64url::base64_urldecode(code)
  envir <- new.env(parent= baseenv())
  what <- eval(parse(text = text), envir = envir)
  do.call(what = what, args = args, envir = envir)
}
