time_stamp <- function(time = Sys.time()) {
  format(time, "%z UTC %Y-%m-%d %H:%M %OS2")
}

time_stamp_short <- function(time = Sys.time()) {
  format(time, "%H:%M %OS2")
}

time_seconds <- function() {
  if_any(
    tar_runtime$fun %in% c("tar_make_future", "tar_make_clustermq"),
    as.numeric(proc.time()["elapsed"]),
    time_seconds_local()
  )
}

# not possible to cover both cases
# nocov start
if (length(find.package("nanonext", quiet = TRUE)) > 0L) {
  time_seconds_local <- function() {
    nanonext::mclock() / 1e3
  }
} else {
  time_seconds_local <- function() {
    as.numeric(proc.time()["elapsed"])
  }
}
# nocov end
