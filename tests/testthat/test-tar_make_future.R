tar_test("tar_make_future() works with callr_function = NULL", {
  skip_if_not_installed("future")
  tar_script(list(tar_target(x, "x")))
  tar_make_future(
    callr_function = NULL,
    reporter = "silent"
  )
  expect_equal(tar_read(x), "x")
})

tar_test("tar_make_future() works", {
  skip_cran()
  skip_if_not_installed("future")
  tar_script(list(tar_target(x, "x")))
  tar_make_future(
    callr_arguments = list(show = FALSE),
    reporter = "silent"
  )
  expect_equal(tar_read(x), "x")
})

tar_test("tar_make_future() can use tidyselect", {
  skip_cran()
  skip_if_not_installed("future")
  tar_script(
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  )
  tar_make_future(
    names = starts_with("y"),
    reporter = "silent",
    callr_arguments = list(show = FALSE)
  )
  out <- sort(list.files(file.path("_targets", "objects")))
  expect_equal(out, sort(c("y1", "y2")))
})

tar_test("nontrivial globals with global environment", {
  skip_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  tar_script({
    future::plan(future.callr::callr)
    f <- function(x) {
      g(x) + 1L
    }
    g <- function(x) {
      x + 1L
    }
    list(
      tar_target(x, 1),
      tar_target(y, f(x))
    )
  })
  tar_make_future(
    reporter = "silent",
    callr_arguments = list(spinner = FALSE)
  )
  expect_equal(tar_read(y), 3L)
})

tar_test("nontrivial globals with non-global environment", {
  skip_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("future.callr")
  tar_script({
    future::plan(future.callr::callr)
    envir <- new.env(parent = globalenv())
    evalq({
      f <- function(x) {
        g(x) + 1L
      }
      g <- function(x) {
        x + 1L
      }
    }, envir = envir)
    tar_option_set(envir = envir)
    list(
      tar_target(x, 1),
      tar_target(y, f(x))
    )
  })
  tar_make_future(
    reporter = "silent",
    callr_arguments = list(spinner = FALSE)
  )
  expect_equal(tar_read(y), 3L)
})

tar_test("custom script and store args", {
  skip_cran()
  skip_if_not_installed("future")
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    tar_target(x, TRUE),
    script = "example/script.R"
  )
  tar_make_future(
    script = "example/script.R",
    store = "example/store",
    callr_function = NULL
  )
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/objects/x"))
  expect_equal(readRDS("example/store/objects/x"), TRUE)
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("custom script and store args with callr function", {
  skip_cran()
  skip_if_not_installed("future")
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    tar_target(x, TRUE),
    script = "example/script.R"
  )
  tar_make_future(
    script = "example/script.R",
    store = "example/store",
    reporter = "silent"
  )
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_true(file.exists("example/store/meta/meta"))
  expect_true(file.exists("example/store/objects/x"))
  expect_equal(readRDS("example/store/objects/x"), TRUE)
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("bootstrap builder for shortcut", {
  skip_cran()
  skip_if_not_installed("future")
  tar_script({
    list(
      tar_target(w, 1L),
      tar_target(x, w),
      tar_target(y, 1L),
      tar_target(z, x + y)
    )
  })
  tar_make_future(callr_function = NULL)
  expect_equal(tar_read(z), 2L)
  tar_script({
    list(
      tar_target(w, 1L),
      tar_target(x, w),
      tar_target(y, 1L),
      tar_target(z, x + y + 1L)
    )
  })
  tar_make_future(names = "z", shortcut = TRUE, callr_function = NULL)
  expect_equal(tar_read(z), 3L)
  progress <- tar_progress()
  expect_equal(nrow(progress), 1L)
  expect_equal(progress$name, "z")
  expect_equal(progress$progress, "built")
})
