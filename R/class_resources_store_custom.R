resources_store_custom_init <- function(
  read = function(path) {
    readRDS(path) # nocov
  },
  write = function(object, path) {
    saveRDS(object = object, file = path, version = 3L) # nocov
  },
  marshal = function(object) {
    identity(object) # nocov
  },
  unmarshal = function(object) {
    identity # nocov
  },
  repository = "local"
) {
  resources_store_custom_new(
    read = base64url::base64_urlencode(tar_deparse_safe(read)),
    write = base64url::base64_urlencode(tar_deparse_safe(write)),
    marshal = base64url::base64_urlencode(tar_deparse_safe(marshal)),
    unmarshal = base64url::base64_urlencode(tar_deparse_safe(unmarshal)),
    repository = repository
  )
}

resources_store_custom_new <- function(
  read = NULL,
  write = NULL,
  marshal = NULL,
  unmarshal = NULL,
  repository = NULL
) {
  force(read)
  force(write)
  force(marshal)
  force(unmarshal)
  force(repository)
  enclass(environment(), c("tar_resources_store_custom", "tar_resources"))
}

#' @export
resources_validate.tar_resources_store_custom <- function(resources) {
  tar_assert_chr(resources$read)
  tar_assert_chr(resources$write)
  tar_assert_chr(resources$marshal)
  tar_assert_chr(resources$unmarshal)
  tar_assert_chr(resources$repository)
  tar_assert_scalar(resources$read)
  tar_assert_scalar(resources$write)
  tar_assert_scalar(resources$marshal)
  tar_assert_scalar(resources$unmarshal)
  tar_assert_scalar(resources$repository)
  read <- eval(parse(text = base64url::base64_urldecode(resources$read)))
  write <- eval(parse(text = base64url::base64_urldecode(resources$write)))
  marshal <- eval(parse(text = base64url::base64_urldecode(resources$marshal)))
  unmarshal <- eval(
    parse(text = base64url::base64_urldecode(resources$unmarshal))
  )
  tar_assert_function(read)
  tar_assert_function(write)
  tar_assert_function(marshal)
  tar_assert_function(unmarshal)
  tar_assert_function_arguments(read, "path")
  tar_assert_function_arguments(write, c("object", "path"))
  tar_assert_function_arguments(marshal, "object")
  tar_assert_function_arguments(unmarshal, "object")
}

#' @export
print.tar_resources_store_custom <- function(x, ...) {
  cat(
    "<tar_resources_store_custom>\n ",
    paste0(paste_list(as.list(x)), collapse = "\n  ")
  )
}
