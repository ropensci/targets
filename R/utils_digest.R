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
  unname(map_chr(object, vdigest64_file, serialize = FALSE, file = TRUE, ...))
}

digest_obj32 <- function(object, ...) {
  vdigest32(list(object), serialize = TRUE, serializeVersion = 3L, ...)
}

digest_obj64 <- function(object, ...) {
  vdigest64(list(object), serialize = TRUE, serializeVersion = 3L, ...)
}

produce_seed <- function(scalar) {
  if_any(
    anyNA(tar_option_get("seed")),
    NA_integer_,
    digest::digest2int(as.character(scalar), seed = tar_option_get("seed"))
  )
}

null64 <- digest_obj64(NULL)
