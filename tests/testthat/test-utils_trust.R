tar_test("reading custom format targets asks for trust", {
  skip_if(interactive())
  skip_cran()
  tar_script({
    format <- tar_format(
      read = function(path) readRDS(path),
      write = function(object, path) saveRDS(object, path)
    )
    list(
      tar_target(plain, 1L),
      tar_target(custom, 2L, format = format),
      tar_target(branched, c(3L, 4L), pattern = map(plain), format = format)
    )
  })
  tar_make(callr_function = NULL)
  restore <- mock_outside_check()
  on.exit(restore(), add = TRUE)
  Sys.setenv(TAR_ASK = "true")
  expect_equal(tar_read(plain), 1L)
  tar_load(plain)
  expect_equal(plain, 1L)
  expect_error(tar_read(custom), class = "tar_condition_run")
  expect_error(tar_read(branched), class = "tar_condition_run")
  expect_error(tar_load(custom), class = "tar_condition_run")
  expect_error(tar_load_everything(), class = "tar_condition_run")
  Sys.setenv(TAR_ASK = "false")
  expect_equal(tar_read(custom), 2L)
  tar_load(custom)
  expect_equal(custom, 2L)
})

tar_test("reading CAS targets asks for trust", {
  skip_if(interactive())
  skip_cran()
  tar_script({
    repository <- tar_repository_cas(
      upload = function(key, path) {
        dir.create("cas", showWarnings = FALSE)
        file.copy(path, file.path("cas", key))
      },
      download = function(key, path) {
        file.copy(file.path("cas", key), path)
      },
      exists = function(key) {
        file.exists(file.path("cas", key))
      }
    )
    list(tar_target(x, 1L, repository = repository))
  })
  tar_make(callr_function = NULL)
  restore <- mock_outside_check()
  on.exit(restore(), add = TRUE)
  Sys.setenv(TAR_ASK = "true")
  expect_error(tar_read(x), class = "tar_condition_run")
  Sys.setenv(TAR_ASK = "false")
  expect_equal(tar_read(x), 1L)
})

tar_test("the pipeline never asks for trust", {
  skip_if(interactive())
  skip_cran()
  tar_script({
    format <- tar_format(
      read = function(path) readRDS(path),
      write = function(object, path) saveRDS(object, path)
    )
    list(
      tar_target(x, 1L, format = format),
      tar_target(y, x + 1L),
      tar_target(
        z,
        {
          writeLines(as.character(targets::tar_read(x) + 2L), "z.txt")
          "z.txt"
        },
        format = "file"
      )
    )
  })
  restore <- mock_outside_check()
  on.exit(restore(), add = TRUE)
  Sys.setenv(TAR_ASK = "true")
  tar_make(callr_function = NULL)
  expect_equal(tar_progress(z)$progress, "completed")
  tar_invalidate(y)
  tar_make(names = y, shortcut = TRUE, callr_function = NULL)
  expect_equal(tar_progress(y)$progress, "completed")
  Sys.setenv(TAR_ASK = "false")
  expect_equal(readLines("z.txt"), "3")
  expect_equal(tar_read(y), 2L)
})

tar_test("tar_workspace() asks for trust with custom storage code", {
  skip_if(interactive())
  skip_cran()
  tar_script({
    format <- tar_format(
      read = function(path) readRDS(path),
      write = function(object, path) saveRDS(object, path)
    )
    tar_option_set(workspaces = c("y", "w"))
    list(
      tar_target(x, 1L, format = format),
      tar_target(y, x + 1L),
      tar_target(v, 1L),
      tar_target(w, v + 1L)
    )
  })
  tar_make(callr_function = NULL)
  restore <- mock_outside_check()
  on.exit(restore(), add = TRUE)
  Sys.setenv(TAR_ASK = "true")
  envir <- new.env(parent = globalenv())
  expect_error(
    tar_workspace(y, envir = envir, source = FALSE),
    class = "tar_condition_run"
  )
  tar_workspace(w, envir = envir, source = FALSE)
  expect_equal(envir$v, 1L)
  Sys.setenv(TAR_ASK = "false")
  tar_workspace(y, envir = envir, source = FALSE)
  expect_equal(envir$x, 1L)
})
