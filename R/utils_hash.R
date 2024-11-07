hash_file <- function(path) {
  secretbase::siphash13(file = path)
}

hash_object <- function(object) {
  secretbase::siphash13(x = object)
}

hash_null <- hash_object(object = NULL)
