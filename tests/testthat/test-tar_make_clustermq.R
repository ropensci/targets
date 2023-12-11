crew_test_sleep()

tar_test("tar_make_clustermq() works with callr_function = NULL", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  tar_script({
    options(clustermq.scheduler = "multicore")
    list(tar_target(x, "x"))
  })
  suppressWarnings(
    tar_make_clustermq(
      callr_function = NULL,
      reporter = "silent",
      garbage_collection = TRUE
    )
  )
  expect_equal(tar_read(x), "x")
})

tar_test("tar_make_clustermq() works", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  tar_script({
    options(clustermq.scheduler = "multicore")
    list(tar_target(x, "x"))
  })
  tar_make_clustermq(
    callr_arguments = list(show = FALSE),
    reporter = "silent"
  )
  expect_equal(tar_read(x), "x")
})

tar_test("tar_make_clustermq() can use tidyselect", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  require_clustermq()
  skip_on_covr()
  tar_script({
    options(clustermq.scheduler = "multicore")
    list(
      tar_target(y1, 1 + 1),
      tar_target(y2, 1 + 1),
      tar_target(z, y1 + y2)
    )
  })
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(
    tar_make_clustermq(
      names = starts_with("y"),
      reporter = "silent",
      callr_arguments = list(show = FALSE)
    )
  )
  out <- sort(list.files(file.path("_targets", "objects")))
  expect_equal(out, sort(c("y1", "y2")))
})

tar_test("custom script and store args", {
  skip_cran()
  require_clustermq()
  skip_on_covr()
  skip_on_os("solaris")
  skip_on_os("windows")
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  old_option <- getOption("clustermq.scheduler")
  on.exit(options(clustermq.scheduler = old_option))
  tar_script({
    tar_option_set(packages = character(0))
    options(clustermq.scheduler = "multicore")
    tar_target(x, TRUE)
  }, script = "example/script.R")
  tar_make_clustermq(
    script = "example/script.R",
    store = "example/store",
    callr_arguments = list(show = FALSE),
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

tar_test("custom script and store args with callr function", {
  skip_cran()
  require_clustermq()
  skip_on_covr()
  skip_on_os("solaris")
  skip_on_os("windows")
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script({
    options(clustermq.scheduler = "multicore")
    tar_target(x, TRUE)
  }, script = "example/script.R")
  tmp <- utils::capture.output(
    suppressMessages(
      tar_make_clustermq(
        script = "example/script.R",
        store = "example/store",
        reporter = "silent"
      )
    )
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
  skip_on_os("solaris")
  skip_on_os("windows")
  require_clustermq()
  skip_on_covr()
  tar_script({
    options(clustermq.scheduler = "multicore")
    list(
      tar_target(w, 1L),
      tar_target(x, w),
      tar_target(y, 1L),
      tar_target(z, x + y)
    )
  })
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(
    tar_make_clustermq(callr_function = NULL)
  )
  expect_equal(tar_read(z), 2L)
  tar_script({
    options(clustermq.scheduler = "multicore")
    list(
      tar_target(w, 1L),
      tar_target(x, w),
      tar_target(y, 1L),
      tar_target(z, x + y + 1L)
    )
  })
  # https://github.com/mschubert/clustermq/issues/269
  suppressWarnings(
    tar_make_clustermq(names = "z", shortcut = TRUE, callr_function = NULL)
  )
  expect_equal(tar_read(z), 3L)
  progress <- tar_progress()
  expect_equal(nrow(progress), 1L)
  expect_equal(progress$name, "z")
  expect_equal(progress$progress, "completed")
})

crew_test_sleep()
