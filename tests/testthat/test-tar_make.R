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

tar_test("tar_make() works with crew", {
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("crew", minimum_version = "0.9.0")
  skip_if_not_installed("R.utils")
  should_skip <- identical(tolower(Sys.info()[["sysname"]]), "windows") &&
    isTRUE(as.logical(Sys.getenv("CI")))
  if (should_skip) {
    skip("skipping on Windows CI.")
  }
  tar_script({
    tar_option_set(
      controller = crew::crew_controller_local(
        host = "127.0.0.1",
        seconds_interval = 0.5
      ),
      backoff = tar_backoff(min = 0.5, max = 0.5)
    )
    tar_target(
      x,
      TRUE,
      memory = "transient",
      garbage_collection = TRUE
    )
  })
  on.exit({
    gc()
    crew_test_sleep()
  })
  expect_error(tar_crew(), class = "tar_condition_validate")
  R.utils::withTimeout(
    tar_make(
      reporter = "silent",
      callr_function = NULL
    ),
    timeout = 360
  )
  out <- tar_read(x)
  expect_true(out)
  expect_true(is.data.frame(tar_crew()))
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
  old <- Sys.getenv("TAR_TEST")
  on.exit(Sys.setenv(TAR_TEST = old))
  Sys.setenv(TAR_TEST = "false")
  tar_script({
    list(
      tar_target(x, "x"),
      tar_target(y, stop(x))
    )
  })
  expect_error(
    tar_make(reporter = "silent", callr_arguments = list(show = FALSE)),
    class = "tar_condition_run"
  )
})

tar_test("custom script, store args, and garbage collection", {
  skip_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(
    {
      tar_option_set(garbage_collection = 1L)
      tar_target(x, TRUE)
    },
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
  expect_true(readRDS("example/store/objects/x"))
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
  expect_true(readRDS("example/store/objects/x"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("runtime settings are forwarded, local process", {
  skip_cran()
  tar_script({
    writeLines(tar_path_store(), "store.txt")
    writeLines(tar_path_script(), "script.txt")
    tar_target(x, 1)
  }, script = "custom_script")
  tar_make(
    callr_function = NULL,
    store = "custom_store",
    script = "custom_script"
  )
  expect_equal(readLines("store.txt"), "custom_store")
  expect_equal(readLines("script.txt"), "custom_script")
})

tar_test("runtime settings are forwarded, extrernal process", {
  skip_cran()
  tar_script({
    writeLines(tar_path_store(), "store.txt")
    writeLines(tar_path_script(), "script.txt")
    tar_target(x, 1)
  }, script = "custom_script")
  tar_make(
    callr_arguments = list(spinner = FALSE),
    reporter = "silent",
    store = "custom_store",
    script = "custom_script"
  )
  expect_equal(readLines("store.txt"), "custom_store")
  expect_equal(readLines("script.txt"), "custom_script")
})

tar_test("null environment", {
  skip_cran()
  tar_script(tar_target(x, "x"))
  tar_make(callr_function = NULL, envir = NULL)
  expect_equal(tar_read(x), "x")
})

tar_test("working directory antipattern", {
  skip_cran()
  dir.create("x")
  old_dir <- getwd() # nolint
  on.exit(setwd(old_dir)) # nolint
  tar_script(
    list(
      tar_target(x, {
        setwd("x") # nolint
        1
      }),
      tar_target(y, x)
    )
  )
  expect_error(
    suppressWarnings(tar_make(callr_function = NULL)),
    class = "tar_condition_run"
  )
})

tar_test("tar_make_reporter()", {
  expect_equal(
    suppressWarnings(tar_make_reporter("summary")),
    "balanced"
  )
})
