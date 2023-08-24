retry_until_success <- function(
  fun,
  args = list(),
  seconds_interval = 0.1,
  seconds_timeout = 5,
  max_tries = Inf,
  message = character(0),
  envir = parent.frame(),
  verbose = TRUE,
  classes_retry = character(0L)
) {
  force(envir)
  fun <- as_function(fun)
  tries <- 0L
  start <- time_seconds()
  while (TRUE) {
    tries <- tries + 1L
    result <- retry_attempt_success(fun, args, envir, verbose, classes_retry)
    if (result$success) {
      return(result$result)
    }
    retry_iteration(
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      max_tries = max_tries,
      message = message,
      tries = tries,
      start = start
    )
  }
  out
}

retry_attempt_success <- function(fun, args, envir, verbose, classes_retry) {
  tryCatch(
    list(
      result = do.call(what = fun, args = args, envir = envir),
      success = TRUE
    ),
    error = function(condition) {
      if (verbose) {
        message(conditionMessage(condition))
      }
      class <- class(condition)
      if (any(class %in% classes_retry)) {
        list(success = FALSE)
      } else {
        tar_throw_expire(conditionMessage(condition))
      }
    }
  )
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
  fun <- as_function(fun)
  tries <- 0L
  start <- time_seconds()
  while (!isTRUE(retry_attempt_true(fun, args, envir, catch_error, verbose))) {
    tries <- tries + 1L
    retry_iteration(
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      max_tries = max_tries,
      message = message,
      tries = tries,
      start = start
    )
  }
  invisible()
}

retry_attempt_true <- function(fun, args, envir, catch_error, verbose) {
  if_any(
    catch_error,
    tryCatch(
      all(do.call(what = fun, args = args, envir = envir)),
      error = function(condition) {
        if (verbose) {
          message(conditionMessage(condition))
        }
        FALSE
      }
    ),
    all(do.call(what = fun, args = args, envir = envir))
  )
}

retry_iteration <- function(
  seconds_interval,
  seconds_timeout,
  max_tries,
  message,
  tries,
  start
) {
  if ((time_seconds() - start) > seconds_timeout) {
    message <- paste(
      "timed out after retrying for",
      seconds_timeout,
      "seconds.",
      message
    )
    tar_throw_expire(message)
  }
  if (tries >= max_tries) {
    message <- paste(
      "giving up after",
      max_tries,
      "attempts.",
      message
    )
    tar_throw_expire(message)
  }
  jitter <- stats::runif(
    n = 1L,
    min = - seconds_interval / 3,
    max = seconds_interval / 3
  )
  Sys.sleep(seconds_interval + jitter)
}
