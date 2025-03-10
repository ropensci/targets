tar_test("tar_meta() works", {
  pipeline <- pipeline_init(list(target_init("x", quote(1))))
  local_init(pipeline = pipeline)$run()
  out <- tar_meta()
  expect_equal(nrow(out), 1L)
  expect_equal(out$name, "x")
  expect_equal(colnames(out), header_meta())
})

tar_test("tar_meta() with target selection", {
  tar_script({
    envir <- new.env(parent = baseenv())
    tar_option_set(envir = envir)
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  out <- tar_meta()
  expect_equal(nrow(out), 4L)
  out <- tar_meta(c("y", "x"))
  expect_equal(out$name, c("y", "x"))
  out <- tar_meta(c("x", "y"))
  expect_equal(out$name, c("x", "y"))
})

tar_test("tar_meta() with field selection", {
  tar_script({
    envir <- new.env(parent = baseenv())
    tar_option_set(envir = envir)
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x))
    )
  })
  tar_make(callr_function = NULL)
  out <- tar_meta(fields = "command")
  expect_equal(nrow(out), 4L)
  expect_equal(colnames(out), c("name", "command"))
})

tar_test("tar_meta() targets_only", {
  skip_cran()
  tar_script({
    envir <- new.env(parent = baseenv())
    tar_option_set(envir = envir)
    envir$f <- function() {
      "y"
    }
    list(tar_target(x, f()))
  })
  tar_make(callr_function = NULL)
  out <- tar_meta(targets_only = FALSE)
  expect_true("f" %in% out$name)
  out <- tar_meta(targets_only = TRUE)
  expect_false("f" %in% out$name)
})

tar_test("tar_meta() targets_only", {
  skip_cran()
  tar_script({
    list(
      tar_target(x, "x"),
      tar_target(y, stop(x))
    )
  })
  expect_error(tar_make(callr_function = NULL))
  out <- tar_meta(fields = error, complete_only = TRUE)
  expect_equal(dim(out), c(1L, 2L))
  expect_equal(out$name, "y")
  expect_true(grepl("x$", out$error))
})

tar_test("custom script and store args", {
  skip_cran()
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
  expect_true(is.data.frame(tar_meta(store = "example/store")))
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
