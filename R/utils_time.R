time_stamp <- function(time = Sys.time()) {
  format(time, "%z UTC %Y-%m-%d %H:%M %OS2")
}

time_stamp_short <- function(time = Sys.time()) {
  format(time, "%H:%M %OS2")
}

time_seconds <- function() {
  if_any(
    tar_runtime$get_fun() %in% c("tar_make_future", "tar_make_clustermq"),
    as.numeric(proc.time()["elapsed"]),
    time_seconds_local()
  )
}

time_seconds_local <- function() {
  nanonext::mclock() / 1e3
}
