tar_test("tar_make() works", {
  tar_script(
    list(
      tar_target(y1, 1L + 1L),
      tar_target(y2, 1L + 1L),
      tar_target(z, y1 + y2)
    )
  )
  tar_make(
    reporter = "silent",
    callr_function = NULL
  )
  out <- tar_read(z)
  expect_equal(out, 4L)
})

tar_test("empty tar_make() works even with names", {
  skip_cran()
  tar_script(list())
  expect_silent(
    tar_make(
      names = x,
      reporter = "silent",
      callr_function = NULL
    )
  )
})

tar_test("tar_make() deduplicates metadata", {
  skip_cran()
  tar_script({
    tar_option_set(envir = new.env(parent = baseenv()))
    list(tar_target(x, 1L, cue = tar_cue(mode = "always")))
  })
  for (index in seq_len(3L)) {
    tar_make(callr_arguments = list(show = FALSE))
  }
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 1L)
})

tar_test("tar_make() can use tidyselect", {
  tar_script(
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  )
  tar_make(
    names = starts_with("y"),
    reporter = "silent",
    callr_arguments = list(show = FALSE)
  )
  out <- sort(list.files(file.path("_targets", "objects")))
  expect_equal(out, sort(c("y1", "y2")))
})

tar_test("tar_make() finds the correct environment", {
  skip_cran()
  tar_script({
    f <- function(x) {
      g(x) + 1L
    }
    g <- function(x) {
      x + 1L
    }
    a <- 1L
    list(tar_target(y, f(!!a), tidy_eval = TRUE))
  })
  tar_make(
    reporter = "silent",
    callr_arguments = list(show = FALSE)
  )
  out <- tar_read(y)
  expect_equal(out, 3L)
})

tar_test("tar_make() handles callr errors", {
  skip_cran()
  withr::local_envvar(list(TAR_TEST = "false")) # covers some lines
  tar_script({
    list(
      tar_target(x, "x"),
      tar_target(y, stop(x))
    )
  })
  # TODO: when https://github.com/r-lib/callr/issues/196 and
  # https://github.com/r-lib/callr/issues/197 are fixed,
  # go back to expecting forwarded errors of class "tar_condition_validate".
  try(
    tar_make(reporter = "silent", callr_arguments = list(show = FALSE)),
    silent = TRUE
  )
  expect_null(NULL)
})

tar_test("priorities apply to tar_make() (#437)", {
  skip_cran()
  tar_script(
    list(
      tar_target(x1, 1, priority = 0),
      tar_target(y1, 1, priority = 0.5),
      tar_target(z1, 1, priority = 1),
      tar_target(x2, x1, priority = 0),
      tar_target(y2, y1, priority = 0.5),
      tar_target(z2, z1, priority = 1)
    )
  )
  tar_make(callr_function = NULL)
  out <- tar_progress()$name
  exp <- c("z1", "z2", "y1", "y2", "x1", "x2")
  expect_equal(out, exp)
})

tar_test("custom script and store args", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    tar_target(x, TRUE),
    script = "example/script.R"
  )
  tar_make(
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
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    tar_target(x, TRUE),
    script = "example/script.R"
  )
  tar_make(
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

tar_test("runtime settings are forwarded, local process", {
  skip_cran()
  tar_script({
    writeLines(tar_store(), "store.txt")
    tar_target(x, 1)
  })
  tar_make(callr_function = NULL, store = "custom_store")
  expect_equal(readLines("store.txt"), "custom_store")
})

tar_test("runtime settings are forwarded, extrernal process", {
  skip_cran()
  tar_script({
    writeLines(tar_store(), "store.txt")
    tar_target(x, 1)
  })
  tar_make(
    store = "custom_store",
    reporter = "silent",
    callr_arguments = list(spinner = FALSE)
  )
  expect_equal(readLines("store.txt"), "custom_store")
})

tar_test("null environment", {
  skip_cran()
  tar_script(tar_target(x, "x"))
  tar_make(callr_function = NULL, envir = NULL)
  expect_equal(tar_read(x), "x")
})
