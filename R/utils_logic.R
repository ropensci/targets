`%||%` <- function(x, y) {
  if (length(x) <= 0L) {
    y
  } else {
    x
  }
}

`%|||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

`%||NA%` <- function(x, y) {
  if (anyNA(x)) {
    y
  } else {
    x
  }
}

`%||nf%` <- function(x, y) {
  if (length(x) <= 0L || anyNA(x)) {
    y
  } else {
    x
  }
}

if_any <- function(condition, x, y) {
  if (any(condition)) {
    x
  } else {
    y
  }
}

retry_until_true <- function(
  fun,
  args = list(),
  seconds_interval = 0.1,
  seconds_timeout = 5,
  max_tries = Inf,
  message = character(0),
  envir = parent.frame(),
  catch_error = TRUE,
  verbose = TRUE
) {
  force(envir)
  fun <- rlang::as_function(fun)
  tar_assert_function(fun)
  tar_assert_list(args)
  if (length(args)) {
    tar_assert_named(args)
    tar_assert_equal_lengths(unique(names(args)), args)
  }
  tar_assert_dbl(seconds_interval)
  tar_assert_scalar(seconds_interval)
  tar_assert_none_na(seconds_interval)
  tar_assert_ge(seconds_interval, 0)
  tar_assert_dbl(seconds_timeout)
  tar_assert_scalar(seconds_timeout)
  tar_assert_none_na(seconds_timeout)
  tar_assert_ge(seconds_timeout, 0)
  tries <- 0L
  start <- time_seconds()
  while (!isTRUE(retry_attempt(fun, args, envir, catch_error, verbose))) {
    if ((time_seconds() - start) > seconds_timeout) {
      message <- paste(
        "timed out after retrying for",
        seconds_timeout,
        "seconds.",
        message
      )
      tar_throw_expire(message)
    }
    tries <- tries + 1L
    if (tries >= max_tries) {
      message <- paste(
        "giving up after",
        max_tries,
        "attempts.",
        message
      )
      tar_throw_expire(message)
    }
    Sys.sleep(seconds_interval)
  }
  invisible()
}

retry_attempt <- function(fun, args, envir, catch_error, verbose) {
  if_any(
    catch_error,
    tryCatch(
      all(do.call(what = fun, args = args)),
      error = function(condition) {
        if (verbose) {
          message(conditionMessage(condition))
        }
        FALSE
      }
    ),
    all(do.call(what = fun, args = args))
  )
}
