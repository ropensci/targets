digest_chr32 <- function(object, ...) {
  vdigest32(object, serialize = FALSE, file = FALSE, seed = 0L, ...)
}

digest_chr64 <- function(object, ...) {
  vdigest64(object, serialize = FALSE, file = FALSE, seed = 0L, ...)
}

digest_file64 <- function(object, ...) {
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

digest_obj32 <- function(object, ...) {
  vdigest32(
    object = list(object),
    serialize = TRUE,
    serializeVersion = 3L,
    file = FALSE,
    seed = 0L,
    ...
  )
}

digest_obj64 <- function(object, ...) {
  vdigest64(
    object = list(object),
    serialize = TRUE,
    serializeVersion = 3L,
    file = FALSE,
    seed = 0L,
    ...
  )
}

vdigest32 <- digest::getVDigest(algo = "xxhash32")

vdigest64 <- digest::getVDigest(algo = "xxhash64")

vdigest64_file <- digest::getVDigest(algo = "xxhash64", errormode = "warn")

null64 <- digest_obj64(NULL)
