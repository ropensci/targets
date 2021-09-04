tar_test("tar_config_set() inherits", {
  expect_false(file.exists("_targets.yaml"))
  expect_null(tar_config_get("inherits"))
  tar_config_set(inherits = "summary")
  expect_equal(tar_config_get("inherits"), "summary")
  expect_true(file.exists("_targets.yaml"))
  expect_true(any(grepl("inherits", readLines("_targets.yaml"))))
  tar_config_set()
  expect_equal(tar_config_get("inherits"), "summary")
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_null(tar_config_get("inherits"))
})

tar_test("tar_config_set() reporter_make", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("reporter_make"), "verbose")
  tar_config_set(reporter_make = "summary")
  expect_equal(tar_config_get("reporter_make"), "summary")
  expect_true(file.exists("_targets.yaml"))
  expect_true(any(grepl("reporter_make", readLines("_targets.yaml"))))
  tar_config_set()
  expect_equal(tar_config_get("reporter_make"), "summary")
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("reporter_make"), "verbose")
})

tar_test("tar_config_set() reporter_outdated", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("reporter_outdated"), "silent")
  tar_config_set(reporter_outdated = "forecast")
  expect_equal(tar_config_get("reporter_outdated"), "forecast")
  expect_true(file.exists("_targets.yaml"))
  expect_true(any(grepl("reporter_outdated", readLines("_targets.yaml"))))
  tar_config_set()
  expect_equal(tar_config_get("reporter_outdated"), "forecast")
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("reporter_outdated"), "silent")
})

tar_test("tar_config_set() shortcut", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("shortcut"), FALSE)
  tar_config_set(shortcut = TRUE)
  expect_equal(tar_config_get("shortcut"), TRUE)
  expect_true(file.exists("_targets.yaml"))
  expect_true(any(grepl("shortcut", readLines("_targets.yaml"))))
  tar_config_set()
  expect_equal(tar_config_get("shortcut"), TRUE)
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("shortcut"), FALSE)
})

tar_test("tar_config_set() with script", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  path <- tempfile()
  tar_config_set(script = path)
  expect_equal(tar_config_get("script"), path)
  expect_true(file.exists("_targets.yaml"))
  tar_config_set()
  expect_equal(tar_config_get("script"), path)
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("script"), path_script_default())
})

tar_test("tar_config_set() with script and different yaml file", {
  path <- tempfile()
  expect_false(file.exists(path))
  expect_equal(tar_config_get("script", config = path), path_script_default())
  path2 <- tempfile()
  tar_config_set(script = path2, config = path)
  expect_equal(tar_config_get("script", config = path), path2)
  expect_false(file.exists("_targets.yaml"))
  expect_true(file.exists(path))
  tar_config_set(config = path)
  expect_equal(tar_config_get("script", config = path), path2)
  expect_true(file.exists(path))
  expect_false(file.exists("_targets.yaml"))
  unlink(path)
  expect_equal(tar_config_get("script", config = path), path_script_default())
})

tar_test("tar_config_set() with store", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("store"), path_store_default())
  path <- tempfile()
  tar_config_set(store = path)
  expect_equal(tar_config_get("store"), path)
  expect_true(file.exists("_targets.yaml"))
  tar_config_set()
  expect_equal(tar_config_get("store"), path)
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("store"), path_store_default())
})

tar_test("tar_config_set() with store and different yaml file", {
  path <- tempfile()
  expect_false(file.exists(path))
  expect_equal(tar_config_get("store", config = path), path_store_default())
  path2 <- tempfile()
  tar_config_set(store = path2, config = path)
  expect_equal(tar_config_get("store", config = path), path2)
  expect_false(file.exists("_targets.yaml"))
  expect_true(file.exists(path))
  tar_config_set(config = path)
  expect_equal(tar_config_get("store", config = path), path2)
  expect_true(file.exists(path))
  expect_false(file.exists("_targets.yaml"))
  unlink(path)
  expect_equal(tar_config_get("store", config = path), path_store_default())
})

tar_test("tar_config_set() workers", {
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("workers"), 1L)
  tar_config_set(workers = 2L)
  expect_equal(tar_config_get("workers"), 2L)
  expect_true(file.exists("_targets.yaml"))
  expect_true(any(grepl("workers", readLines("_targets.yaml"))))
  tar_config_set()
  expect_equal(tar_config_get("workers"), 2L)
  expect_true(file.exists("_targets.yaml"))
  unlink("_targets.yaml")
  expect_equal(tar_config_get("workers"), 1L)
})

tar_test("_targets.yaml is locked during the pipeline then unlocked after", {
  tar_script({
    list(
      tar_target(a, c("main:", "  store: _targets2")),
      tar_target(x, writeLines(a, "_targets.yaml")),
      tar_target(y, x)
    )
  })
  tar_make(callr_function = NULL)
  expect_false(file.exists("_targets2"))
  expect_true(file.exists("_targets"))
  expect_equal(tar_config_get("store"), "_targets2")
  tar_config_set(store = "_targets")
  expect_equal(tar_config_get("store"), "_targets")
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL)
  expect_equal(unique(tar_progress()$progress), "skipped")
})

tar_test("same with external process", {
  tar_script({
    list(
      tar_target(a, c("main:", "  store: _targets2")),
      tar_target(x, writeLines(a, "_targets.yaml")),
      tar_target(y, x)
    )
  })
  tar_make(reporter = "silent")
  expect_false(file.exists("_targets2"))
  expect_true(file.exists("_targets"))
  expect_equal(tar_config_get("store"), "_targets2")
  tar_config_set(store = "_targets")
  expect_equal(tar_config_get("store"), "_targets")
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL)
  expect_equal(unique(tar_progress()$progress), "skipped")
})

tar_test("tar_config_set() can configure the script and the store", {
  skip_on_cran()
  tar_config_set(script = "example/script.R", store = "example/store")
  tar_script(tar_target(x, 1L))
  tar_make(reporter = "silent")
  expect_equal(tar_read(x), 1L)
  expect_true(file.exists("example/script.R"))
  expect_true(file.exists("example/store"))
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
})

tar_test("tar_config_set() TAR_CONFIG", {
  on.exit(Sys.unsetenv("TAR_CONFIG"))
  Sys.setenv(TAR_CONFIG = "custom.yaml")
  expect_false(file.exists("_targets.yaml"))
  expect_false(file.exists("custom.yaml"))
  tar_config_set(store = "custom")
  expect_false(file.exists("_targets.yaml"))
  expect_true(file.exists("custom.yaml"))
  expect_equal(tar_config_get("store"), "custom")
  tar_script()
  tar_make(callr_function = NULL)
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("custom"))
})

tar_test("single-project format still works", {
  writeLines("store: abc", "_targets.yaml")
  expect_warning(
    out <- tar_config_get("store"),
    class = "tar_condition_deprecate"
  )
  expect_equal(out, "abc")
})

tar_test("single-project converted to multi-project", {
  writeLines("store: abc", "_targets.yaml")
  expect_warning(
    tar_config_set(store = "x123"),
    class = "tar_condition_deprecate"
  )
  expect_equal(tar_config_get("store"), "x123")
  out <- yaml::read_yaml("_targets.yaml")
  expect_equal(out, list(main = list(store = "x123")))
})

tar_test("project switching/setting with project arg", {
  tar_config_set(store = "abc", project = "project1")
  tar_config_set(store = "xyz", project = "project2")
  expect_equal(tar_config_get("store", project = "project1"), "abc")
  expect_equal(tar_config_get("store", project = "project2"), "xyz")
})

tar_test("project switching/setting with TAR_PROJECT", {
  on.exit(Sys.unsetenv("TAR_PROJECT"))
  Sys.setenv(TAR_PROJECT = "project1")
  tar_config_set(store = "abc")
  Sys.setenv(TAR_PROJECT = "project2")
  tar_config_set(store = "xyz")
  Sys.setenv(TAR_PROJECT = "project1")
  expect_equal(tar_config_get("store"), "abc")
  Sys.setenv(TAR_PROJECT = "project2")
  expect_equal(tar_config_get("store"), "xyz")
  Sys.setenv(TAR_PROJECT = "project1")
})

tar_test("correct project inheritance (1 level)", {
  tar_config_set(store = "sa", project = "pa")
  tar_config_set(inherits = "pa", project = "pb")
  expect_equal(tar_config_get("store", project = "pb"), "sa")
  expect_equal(tar_config_get("script", project = "pb"), path_script_default())
})

tar_test("correct project inheritance more than 2 levels deep", {
  tar_config_set(store = "sa", project = "pa")
  tar_config_set(inherits = "pa", project = "pb")
  tar_config_set(inherits = "pb", project = "pc")
  tar_config_set(inherits = "pc", project = "pd")
  tar_config_set(inherits = "pd", project = "pe")
  expect_equal(tar_config_get("store", project = "pe"), "sa")
  expect_equal(tar_config_get("script", project = "pe"), path_script_default())
})

tar_test("inherit from nonexistent project", {
  tar_config_set(store = "sa", project = "pa")
  tar_config_set(inherits = "nope", project = "pb")
  expect_equal(tar_config_get("store", project = "pb"), path_store_default())
})

tar_test("nontrivial circular project inheritance", {
  tar_config_set(store = "sa", inherits = "pe", project = "pa")
  tar_config_set(inherits = "pa", project = "pb")
  tar_config_set(inherits = "pb", project = "pc")
  tar_config_set(inherits = "pc", project = "pd")
  tar_config_set(inherits = "pd", project = "pe")
  expect_error(
    tar_config_get("script", project = "pa"),
    class = "tar_condition_validate"
  )
})

tar_test("project inherits from itself", {
  tar_config_set(inherits = "pa", project = "pa")
  expect_error(
    tar_config_get("script", project = "pa"),
    class = "tar_condition_validate"
  )
})

tar_test("no conversion to multi-project if just a empty projects", {
  writeLines("pa:\npb:", "_targets.yaml")
  tar_config_set(store = "abc", project = "pc")
  yaml <- yaml::read_yaml("_targets.yaml")
  expect_equal(sort(names(yaml)), sort(c("pa", "pb", "pc")))
  expect_null(yaml$pa)
  expect_null(yaml$pb)
  expect_equal(yaml$pc, list(store = "abc"))
})
