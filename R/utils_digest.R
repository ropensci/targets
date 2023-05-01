vdigest32 <- digest::getVDigest(algo = "xxhash32")

vdigest64 <- digest::getVDigest(algo = "xxhash64")

vdigest64_file <- digest::getVDigest(algo = "xxhash64", errormode = "warn")

digest_chr32 <- function(object, ...) {
  vdigest32(object, serialize = FALSE, ...)
}

digest_chr64 <- function(object, ...) {
  vdigest64(object, serialize = FALSE, ...)
}

digest_file64 <- function(object, ...) {
  vapply(
    X = object,
    FUN = vdigest64_file,
    serialize = FALSE,
    file = TRUE,
    ...,
    FUN.VALUE = character(1L),
    USE.NAMES = FALSE
  )
}

digest_obj32 <- function(object, ...) {
  vdigest32(list(object), serialize = TRUE, serializeVersion = 3L, ...)
}

digest_obj64 <- function(object, ...) {
  vdigest64(list(object), serialize = TRUE, serializeVersion = 3L, ...)
}

produce_seed <- function(scalar) {
  seed <- tar_options$get_seed()
  if_any(
    anyNA(seed),
    NA_integer_,
    digest::digest2int(as.character(scalar), seed = seed)
  )
}

null64 <- digest_obj64(NULL)
