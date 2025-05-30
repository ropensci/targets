time_stamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%OS2", tz = "UTC")
}

time_stamp_cli <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%OS2")
}

time_stamp_pid <- function(pid = Sys.getpid()) {
  handle <- tryCatch(
    ps::ps_handle(pid = pid),
    error = function(condition) NULL
  )
  if (is.null(handle)) {
    return(NA_character_)
  }
  time <- ps::ps_create_time(p = handle)
  format(time, "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
}

time_seconds <- function() {
  if_any(
    tar_runtime$fun %in% c("tar_make_future", "tar_make_clustermq"),
    as.numeric(proc.time()["elapsed"]),
    time_seconds_local()
  )
}

time_seconds_local <- function() {
  if (package_installed("nanonext")) {
    nanonext::mclock() / 1e3
  } else {
    as.numeric(proc.time()["elapsed"]) # nocov
  }
}
