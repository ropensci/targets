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

time_seconds_local <- function() {
  if (is.null(tar_runtime$nanonext)) {
    tar_runtime$nanonext <- rlang::is_installed("nanonext")
  }
  if_any(
    tar_runtime$nanonext,
    nanonext::mclock() / 1e3,
    as.numeric(proc.time()["elapsed"])
  )
}
