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
  fun <- rlang::as_function(fun)
  result <- new.env(parent = emptyenv())
  retry_until_true(
    fun = function(fun, args, classes_retry, message, result) {
      tryCatch(
        {
          result$result <- do.call(what = fun, args = args)
          TRUE
        },
        error = function(condition) {
          class <- class(condition)
          if_any(
            class %in% classes_retry,
            FALSE,
            tar_throw_expire(
              paste0(message, ". ", conditionMessage(condition))
            )
          )
        }
      )
    },
    args = list(
      fun = fun,
      args = args,
      classes_retry = classes_retry,
      message = message,
      result = result
    ),
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    max_tries = max_tries,
    message = message,
    envir = envir,
    catch_error = FALSE,
    verbose = verbose
  )
  result$result
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
    jitter <- stats::runif(
      n = 1L,
      min = - seconds_interval / 3,
      max = seconds_interval / 3
    )
    Sys.sleep(seconds_interval + jitter)
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
