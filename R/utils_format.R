format_seconds <- function(x) {
  map_chr(x, format_seconds_scalar)
}

format_seconds_scalar <- function(x) {
  if (is.na(x)) {
    return("")
  }
  if (x > 3600 * 24 * 365) {
    return(paste(round(x / (3600 * 24 * 365), 3), "years"))
  }
  if (x > 3600 * 24 * 30) {
    return(paste(round(x / (3600 * 24 * 30), 3), "months"))
  }
  if (x > 3600 * 24) {
    return(paste(round(x / (3600 * 24), 3), "days"))
  }
  if (x > 3600) {
    return(paste(round(x / 3600, 3), "hours"))
  }
  if (x > 60) {
    return(paste(round(x / 60, 3), "minutes"))
  }
  paste(round(x, 3), "seconds")
}

format_bytes <- function(x) {
  map_chr(x, format_bytes_scalar)
}

format_bytes_scalar <- function(x) {
  if (is.na(x)) {
    return("")
  }
  if (x > 1e12) {
    return(paste(round(x / 1e12, 2), "terabytes"))
  }
  if (x > 1e9) {
    return(paste(round(x / 1e9, 3), "gigabytes"))
  }
  if (x > 1e6) {
    return(paste(round(x / 1e6, 3), "megabytes"))
  }
  if (x > 1e3) {
    return(paste(round(x / 1e3, 3), "kilobytes"))
  }
  paste(round(x, 3), "bytes")
}

format_branches <- function(x) {
  ifelse(
    is.na(x),
    "",
    paste(round(x), "branches")
  )
}
