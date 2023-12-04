tar_test("cue_record()", {
  local <- local_init(pipeline_init(list(target_init("x", quote(1)))))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  local <- local_init(pipeline_init(list(target_init("x", quote(1)))))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("object in environment with the same name as a target", {
  envir <- new.env(parent = baseenv())
  envir$x <- 1L
  envir$f <- function(x) {
    x + 1L
  }
  tar_option_set(envir = envir)
  x <- target_init("x", quote(f(1L)))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  value <- target_read_value(pipeline_get_target(pipeline, "x"))
  expect_equal(value$object, 2L)
  x <- target_init("x", quote(f(1L)))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("function in environment with the same name as a target", {
  envir <- new.env(parent = baseenv())
  envir$x <- identity
  envir$f <- function(x) {
    x + 1L
  }
  tar_option_set(envir = envir)
  x <- target_init("x", quote(f(1L)))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  value <- target_read_value(pipeline_get_target(pipeline, "x"))
  expect_equal(value$object, 2L)
  x <- target_init("x", quote(f(1L)))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("previous import conflicts with current target", {
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  x <- target_init("x", quote(1L))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  envir$x <- 1L
  y <- target_init("y", quote(1L))
  pipeline <- pipeline_init(list(y))
  local <- local_init(pipeline)
  local$run()
  x <- target_init("x", quote(1L))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  x <- target_init("x", quote(1L))
  pipeline <- pipeline_init(list(x))
  local <- local_init(pipeline)
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("cue_record() on a dynamic file", {
  envir <- new.env(parent = baseenv())
  tar_option_set(envir = envir)
  envir$save1 <- function() {
    file <- tempfile()
    saveRDS(1L, file)
    file
  }
  x <- target_init("x", quote(save1()), format = "file")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  x <- target_init("x", quote(save1()), format = "file")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("cue_always()", {
  for (index in seq_len(2)) {
    cue <- cue_init(mode = "always")
    target <- target_init("x", quote(1), cue = cue)
    local <- local_init(pipeline_init(list(target)))
    local$run()
    out <- counter_get_names(local$scheduler$progress$completed)
    expect_equal(out, "x")
  }
})

tar_test("cue_never()", {
  local <- local_init(pipeline_init(list(target_init("x", quote(1L)))))
  local$run()
  cue <- cue_init(mode = "never")
  target <- target_init("x", quote(2L), cue = cue)
  local <- local_init(pipeline_init(list(target)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("cue_command()", {
  local <- local_init(pipeline_init(list(target_init("x", quote(1L)))))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  local <- local_init(pipeline_init(list(target_init("x", quote(2L)))))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("cue_command() suppressed", {
  cue <- cue_init(command = FALSE)
  x <- target_init("x", quote(1L), cue = cue)
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  x <- target_init("x", quote(2L), cue = cue)
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("cue_depend()", {
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(y))
  local <- local_init(pipeline_init(list(x, y, z)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(sort(out), sort(c("x", "y", "z")))
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(y))
  local <- local_init(pipeline_init(list(x, y, z)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(sort(out), sort(c("x", "y", "z")))
  x <- target_init("x", quote(2L))
  y <- target_init("y", quote(x))
  z <- target_init("z", quote(y))
  local <- local_init(pipeline_init(list(x, y, z)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(sort(out), sort(c("x", "y", "z")))
})

tar_test("cue_depend() on a nested function", {
  envir <- new.env(parent = baseenv())
  envir$f <- function(x) g(x)
  envir$g <- function(x) x + 1L
  environment(envir$f) <- envir
  tar_option_set(envir = envir)
  x <- target_init("x", quote(f(1L)))
  y <- target_init("y", quote(x))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(sort(out), sort(c("x", "y")))
  x <- target_init("x", quote(f(1L)))
  y <- target_init("y", quote(x))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(sort(out), sort(c("x", "y")))
  envir$g <- function(x) x + 2L - 1L
  x <- target_init("x", quote(f(1L)))
  y <- target_init("y", quote(x))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("cue_depend() on a nested function with a pattern", {
  envir <- new.env(parent = baseenv())
  envir$f <- function(x) g(x)
  envir$g <- function(x) x + 1L
  environment(envir$f) <- envir
  tar_option_set(envir = envir)
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(f(x)), pattern = quote(map(x)))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_true("x" %in% out)
  expect_true(any(grepl("y_", out)))
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(f(x)), pattern = quote(map(x)))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, character(0))
  envir$g <- function(x) x + 2L - 1L
  x <- target_init("x", quote(1L))
  y <- target_init("y", quote(f(x)), pattern = quote(map(x)))
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_false("x" %in% out)
  expect_true(any(grepl("y_", out)))
})

tar_test("cue_depend() suppressed", {
  cue <- cue_init(depend = FALSE)
  x <- target_init("x", quote(1L), cue = cue)
  y <- target_init("y", quote(x), cue = cue)
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(sort(out), sort(c("x", "y")))
  x <- target_init("x", quote(2L), cue = cue)
  y <- target_init("y", quote(x), cue = cue)
  local <- local_init(pipeline_init(list(x, y)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("cue_format()", {
  skip_if_not_installed("qs")
  x <- target_init("x", quote(1L), format = "rds")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  x <- target_init("x", quote(1L), format = "qs")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("cue_format() suppressed", {
  skip_if_not_installed("qs")
  cue <- cue_init(format = FALSE)
  x <- target_init("x", quote(1L), format = "rds", cue = cue)
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  x <- target_init("x", quote(1L), format = "qs", cue = cue)
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("cue_repository()", {
  skip_if_not_installed("paws.storage")
  tar_script(tar_target(x, 1, repository = "local"))
  tar_make(callr_function = NULL)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_script(tar_target(x, 1, repository = "aws"))
  expect_equal(tar_outdated(callr_function = NULL), "x")
})

tar_test("cue_repository() suppressed", {
  skip_if_not_installed("paws.storage")
  tar_script(
    tar_target(x, 1, repository = "local", cue = tar_cue(repository = FALSE))
  )
  tar_make(callr_function = NULL)
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_script(
    tar_target(x, 1, repository = "aws", cue = tar_cue(repository = FALSE))
  )
  expect_equal(tar_outdated(callr_function = NULL), "x")
})

tar_test("cue_iteration()", {
  x <- target_init("x", quote(1L), iteration = "vector")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  x <- target_init("x", quote(1L), iteration = "list")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("cue_iteration() suppressed", {
  cue <- cue_init(iteration = FALSE)
  x <- target_init("x", quote(1L), iteration = "vector", cue = cue)
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  x <- target_init("x", quote(1L), iteration = "list", cue = cue)
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("cue_seed() responds to changed seed", {
  on.exit(tar_option_reset())
  tar_option_set(seed = 1L)
  x <- target_init("x", quote(1L))
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  x <- target_init("x", quote(1L))
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, character(0))
  tar_option_set(seed = 2L)
  x <- target_init("x", quote(1L))
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("cue_seed() always activates when seed is NA", {
  on.exit(tar_option_reset())
  tar_option_set(seed = NA)
  for (index in seq_len(3)) {
    x <- target_init("x", quote(1L))
    local <- local_init(pipeline_init(list(x)))
    local$run()
    out <- counter_get_names(local$scheduler$progress$completed)
    expect_equal(out, "x")
  }
})

tar_test("cue_seed() suppressed", {
  on.exit(tar_option_reset())
  tar_option_set(seed = NA)
  x <- target_init("x", quote(1L))
  local <- local_init(pipeline_init(list(x)))
  local$run()
  for (index in seq_len(3)) {
    x <- target_init("x", quote(1L), cue = tar_cue(seed = FALSE))
    local <- local_init(pipeline_init(list(x)))
    local$run()
    out <- counter_get_names(local$scheduler$progress$completed)
    expect_equal(out, character(0))
  }
})

tar_test("cue_file()", {
  x <- target_init("x", quote(1L))
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  saveRDS(2L, x$store$file$path)
  x <- target_init("x", quote(1L))
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("file cue activated if a file does not exist", {
  for (index in seq_len(2)) {
    x <- target_init(
      "name",
      quote({
        file.create("x")
        file.create("y")
        c("x", "y")
      }),
      format = "file",
      deployment = "main"
    )
    pipeline <- pipeline_init(list(x))
    local <- local_init(pipeline)
    local$run()
    out <- counter_get_names(local$scheduler$progress$completed)
    expect_equal(out, "name")
    unlink("y")
  }
})

tar_test("above, but with file cue suppressed", {
  file.create("x")
  cue <- cue_init(file = FALSE)
  for (index in seq_len(2)) {
    x <- target_init(
      "name",
      quote({
        file.create("x")
        file.create("y")
        c("x", "y")
      }),
      format = "file",
      deployment = "main",
      cue = cue
    )
    pipeline <- pipeline_init(list(x))
    local <- local_init(pipeline)
    local$run()
    unlink("y")
  }
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "name")
})

tar_test("cue_file() suppressed", {
  cue <- cue_init(file = FALSE)
  x <- target_init("x", quote(1L), cue = cue)
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  saveRDS(2L, x$store$file$path)
  x <- target_init("x", quote(1L), cue = cue)
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$skipped)
  expect_equal(out, "x")
})

tar_test("cue_file() on a dynamic file", {
  envir <- new.env(parent = baseenv())
  envir$save1 <- function() {
    file <- tempfile()
    saveRDS(1L, file)
    file
  }
  tar_option_set(envir = envir)
  x <- target_init("x", quote(save1()), format = "file")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  saveRDS(2L, x$store$file$path)
  x <- target_init("x", quote(save1()), format = "file")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("cue_file() on a fast dynamic file", {
  envir <- new.env(parent = baseenv())
  envir$save1 <- function() {
    file <- tempfile()
    saveRDS(1L, file)
    file
  }
  tar_option_set(envir = envir)
  x <- target_init("x", quote(save1()), format = "file_fast")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
  saveRDS(2L, x$store$file$path)
  x <- target_init("x", quote(save1()), format = "file_fast")
  local <- local_init(pipeline_init(list(x)))
  local$run()
  out <- counter_get_names(local$scheduler$progress$completed)
  expect_equal(out, "x")
})

tar_test("cue print", {
  out <- utils::capture.output(print(tar_cue()))
  expect_true(any(grepl("cue", out)))
})

tar_test("cue_validate()", {
  expect_silent(cue_validate(cue_init()))
})
