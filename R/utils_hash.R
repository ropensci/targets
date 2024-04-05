hash_character <- function(object, ...) {
  vdigest64(object, serialize = FALSE, file = FALSE, seed = 0L, ...)
}

hash_file <- function(object, ...) {
  vapply(
    X = object,
    FUN = vdigest64_file,
    serialize = FALSE,
    file = TRUE,
    seed = 0L,
    ...,
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}

hash_object <- function(object, ...) {
  vdigest64(
    object = list(object),
    serialize = TRUE,
    serializeVersion = 3L,
    file = FALSE,
    seed = 0L,
    ...
  )
}

vdigest64 <- digest::getVDigest(algo = "xxhash64")

vdigest64_file <- digest::getVDigest(algo = "xxhash64", errormode = "warn")

hash_null <- hash_object(NULL)
