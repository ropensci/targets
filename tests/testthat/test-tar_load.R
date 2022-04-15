tar_test("tar_load() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1L)),
      target_init("y2", quote(2L)),
      target_init("z", quote(y1 + y2))
    )
  )
  local_init(pipeline = pipeline)$run()
  envir <- new.env(parent = emptyenv())
  expect_message(tar_load(missing), regexp = "no targets to load")
  tar_load(starts_with("y"), envir = envir)
  expect_equal(sort(names(envir)), sort(c("y1", "y2")))
  expect_equal(envir$y1, 1L)
  expect_equal(envir$y2, 2L)
  expect_false("z" %in% names(envir))
})

tar_test("tar_load_everything() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1L)),
      target_init("y2", quote(2L)),
      target_init("z", quote(y1 + y2))
    )
  )
  local_init(pipeline = pipeline)$run()
  envir <- new.env(parent = emptyenv())
  tar_load_everything(envir = envir)
  expect_equal(sort(names(envir)), sort(c("y1", "y2", "z")))
  expect_equal(envir$y1, 1L)
  expect_equal(envir$y2, 2L)
  expect_equal(envir$z, 3L)
})

tar_test("tar_load_raw() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1L)),
      target_init("y2", quote(2L)),
      target_init("z", quote(y1 + y2))
    )
  )
  local_init(pipeline = pipeline)$run()
  envir <- new.env(parent = emptyenv())
  expect_message(tar_load(missing), regexp = "no targets to load")
  tar_load_raw(c("y1", "y2"), envir = envir)
  expect_equal(sort(names(envir)), sort(c("y1", "y2")))
  expect_equal(envir$y1, 1L)
  expect_equal(envir$y2, 2L)
  expect_false("z" %in% names(envir))
})

tar_test("tar_read() on patterns with vector iteration", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(letters[seq_len(4L)])),
      target_init("y", quote(x), pattern = quote(map(x)), iteration = "vector")
    )
  )
  local_init(pipeline = pipeline)$run()
  expect_equal(unname(tar_read(y)), letters[seq_len(4L)])
  envir <- new.env(parent = emptyenv())
  tar_load(y, branches = c(2L, 3L), envir = envir)
  out <- base::get("y", envir = envir)
  expect_equal(unname(out), letters[c(2L, 3L)])
})

tar_test("custom script and store args", {
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script({
    list(
      tar_target(w, letters)
    )
  }, script = "example/script.R")
  tar_make(
    callr_function = NULL,
    script = "example/script.R",
    store = "example/store"
  )
  envir <- new.env(parent = emptyenv())
  tar_load(w, envir = envir, store = "example/store")
  expect_equal(envir$w, letters)
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/objects/w"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("tar_load() error in strict mode", {
  tar_script(
    list(
      tar_target(x, 1),
      tar_target(y, stop(x))
    )
  )
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
  envir <- new.env(parent = emptyenv())
  expect_error(
    suppressWarnings(
      tar_load(everything(), silent = TRUE, envir = envir)
    )
  )
})

tar_test("tar_load() error in strict mode", {
  tar_script(
    list(
      tar_target(x, "value"),
      tar_target(y, stop(x))
    )
  )
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
  envir <- new.env(parent = emptyenv())
  suppressWarnings(
    tar_load(everything(), strict = FALSE, silent = TRUE, envir = envir)
  )
  expect_equal(names(envir), "x")
  expect_equal(envir$x, "value")
})
