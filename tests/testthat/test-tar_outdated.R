tar_test("tar_outdated() does not create a data store", {
  tar_script({
    f <- identity
    tar_option_set()
    list(tar_target(x, f(1L)))
  })
  out <- tar_outdated(callr_function = NULL)
  expect_false(file.exists("_targets"))
})

tar_test("tar_outdated() does not modify progress", {
  tar_script(
    list(
      tar_target(x, 1:2),
      tar_target(y, x, pattern = map(x))
    )
  )
  tar_make(callr_function = NULL)
  out1 <- tar_progress()
  tar_outdated(callr_function = NULL)
  out2 <- tar_progress()
  expect_equal(out1, out2)
})

tar_test("tar_outdated() without globals", {
  old <- Sys.getenv("TAR_WARN")
  Sys.setenv(TAR_WARN = "false")
  on.exit(Sys.setenv(TAR_WARN = old))
  tar_script({
    f <- identity
    envir <- environment()
    tar_option_set(envir = envir)
    list(tar_target(x, f(1L)))
  })
  out <- tar_outdated(
    targets_only = TRUE,
    callr_arguments = list(show = FALSE)
  )
  expect_equal(out, "x")
  tar_make(reporter = "silent", callr_function = NULL)
  out <- tar_outdated(
    targets_only = TRUE,
    callr_arguments = list(show = FALSE)
  )
  expect_equal(out, character(0))
  tar_script({
    f <- function(x) x + 1L
    envir <- environment()
    tar_option_set(envir = envir)
    list(tar_target(x, f(1L)))
  })
  out <- tar_outdated(
    targets_only = TRUE,
    callr_arguments = list(show = FALSE)
  )
  expect_equal(out, "x")
})

tar_test("tar_outdated() with globals", {
  old <- Sys.getenv("TAR_WARN")
  Sys.setenv(TAR_WARN = "false")
  on.exit(Sys.setenv(TAR_WARN = old))
  tar_script({
    f <- identity
    envir <- environment()
    tar_option_set(envir = envir)
    list(tar_target(x, f(1L)))
  })
  out <- tar_outdated(
    targets_only = FALSE,
    callr_arguments = list(show = FALSE)
  )
  expect_true(all(c("f", "x") %in% out))
  tar_make(reporter = "silent", callr_function = NULL)
  out <- tar_outdated(
    targets_only = FALSE,
    callr_arguments = list(show = FALSE)
  )
  expect_false(any(c("f", "x") %in% out))
  tar_script({
    f <- function(x) x + 1L
    envir <- environment()
    tar_option_set(envir = envir)
    list(tar_target(x, f(1L)))
  })
  out <- tar_outdated(
    targets_only = FALSE,
    callr_arguments = list(show = FALSE)
  )
  expect_true(all(c("f", "x") %in% out))
})

tar_test("tar_outdated() can include branches", {
  tar_script({
    list(
      tar_target(x, seq_len(2L)),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(reporter = "silent", callr_function = NULL)
  tar_script({
    list(
      tar_target(x, seq_len(2L)),
      tar_target(y, x + 1L, pattern = map(x))
    )
  })
  out <- tar_outdated(branches = TRUE, callr_arguments = list(show = FALSE))
  expect_equal(length(out), 3L)
  expect_true("y" %in% out)
  expect_equal(sum(grepl("^y_", out)), 2L)
})

tar_test("tar_outdated() can omit branches", {
  tar_script({
    list(
      tar_target(x, seq_len(2L)),
      tar_target(y, x, pattern = map(x))
    )
  })
  tar_make(reporter = "silent", callr_function = NULL)
  tar_script({
    list(
      tar_target(x, seq_len(2L)),
      tar_target(y, x + 1L, pattern = map(x))
    )
  })
  out <- tar_outdated(branches = FALSE, callr_arguments = list(show = FALSE))
  expect_equal(out, "y")
})

tar_test("tar_outdated() does not deduplicate metadata", {
  tar_script({
    tar_option_set(envir = new.env(parent = baseenv()))
    list(tar_target(x, 1L, cue = tar_cue(mode = "always")))
  })
  for (index in seq_len(2L)) {
    tar_make(callr_function = NULL)
  }
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 1L)
  meta_lines <- readLines(path_meta(path_store_default()))
  expect_equal(length(meta_lines), 2L)
  meta_lines <- c(meta_lines, meta_lines[2])
  writeLines(meta_lines, path_meta(path_store_default()))
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 2L)
  out <- tar_outdated(callr_arguments = list(show = FALSE))
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 2L)
  tar_make(callr_function = NULL)
  out <- meta_init()$database$read_data()
  expect_equal(nrow(out), 1L)
})

tar_test("tar_outdated() names arg works", {
   tar_script({
     envir <- new.env(parent = baseenv())
     tar_option_set(envir = envir)
     list(tar_target(x, 1L), tar_target(y, x))
   })
   out <- tar_outdated(callr_function = NULL)
   expect_equal(sort(out), sort(c("x", "y")))
   out <- tar_outdated(callr_function = NULL, names = "x")
   expect_equal(out, "x")
})

tar_test("custom script and store args", {
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_outdated(
    script = "example/script.R",
    store = "example/store",
    callr_function = NULL
  )
  expect_equal(out, "x")
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("custom script and store args with callr function", {
  skip_on_cran()
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  tar_script(tar_target(x, "y"), script = "example/script.R")
  out <- tar_outdated(
    script = "example/script.R",
    store = "example/store"
  )
  expect_equal(out, "x")
  expect_false(file.exists("_targets.yaml"))
  expect_equal(tar_config_get("script"), path_script_default())
  expect_equal(tar_config_get("store"), path_store_default())
  expect_false(file.exists(path_script_default()))
  expect_false(file.exists(path_store_default()))
  expect_true(file.exists("example/script.R"))
  expect_false(file.exists("example/store"))
  tar_config_set(script = "x")
  expect_equal(tar_config_get("script"), "x")
  expect_true(file.exists("_targets.yaml"))
})

tar_test("bootstrap builder for shortcut", {
  skip_on_cran()
  tar_script({
    list(
      tar_target(w, 1L),
      tar_target(x, w),
      tar_target(y, 1L),
      tar_target(z, x + y)
    )
  })
  tar_make(callr_function = NULL)
  tar_script({
    list(
      tar_target(w, 2L),
      tar_target(x, w),
      tar_target(y, 2L),
      tar_target(z, x + y + 1)
    )
  })
  out <- sort(tar_outdated(callr_function = NULL))
  exp <- sort(c("w", "x", "y", "z"))
  expect_equal(out, exp)
  out <- tar_outdated(
    callr_function = NULL,
    names = "z",
    shortcut = TRUE
  )
  expect_equal(out, "z")
})
