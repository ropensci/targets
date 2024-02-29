time_stamp <- function(time = Sys.time()) {
  format(time, "%Y-%m-%d %H:%M:%OS2", tz = "UTC")
}

time_stamp_cli <- function(time = Sys.time()) {
  paste(time_stamp(time = time), "UTC")
}

time_stamp_short <- function(time = Sys.time()) {
  format(time, "%H:%M %OS2")
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
  if (is.null(tar_runtime$nanonext)) {
    tar_runtime$nanonext <- rlang::is_installed("nanonext")
  }
  if_any(
    tar_runtime$nanonext,
    nanonext::mclock() / 1e3,
    as.numeric(proc.time()["elapsed"])
  )
}
