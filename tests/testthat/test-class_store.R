tar_test("store_wait_correct_hash()", {
  tmp <- tempfile()
  file <- file_init(path = tmp)
  writeLines("lines", tmp)
  store <- store_init()
  store$file <- file
  store$resources <- tar_resources(
    network = suppressWarnings(tar_resources_network(max_tries = 1L))
  )
  expect_error(
    store_wait_correct_hash(store),
    class = "tar_condition_expire"
  )
  file_update_hash(file)
  expect_silent(store_wait_correct_hash(store))
})

tar_test("default serialization/unserialization methods", {
  store <- store_init()
  expect_equal(store_marshal_object(store, "x"), "x")
  expect_equal(store_unmarshal_object(store, "x"), "x")
})

tar_test("store_validate()", {
  expect_silent(store_validate(store_new(resources = list())))
})

tar_test("store_file packages", {
  x <- tar_target(x, "x", format = "file")
  out <- store_get_packages(x$store)
  expect_equal(out, character(0))
})

tar_test("default delete and exists methods", {
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  path <- path_objects(path_store_default(), "x")
  meta <- tar_meta()
  meta <- meta[meta$name == "x",, drop = FALSE] # nolint
  record <- record_from_row(row = meta, path_store = path_store_default())
  store <- record_bootstrap_store(record)
  expect_true(file.exists(path))
  expect_true(store_exist_object(store))
  store_delete_object(store)
  expect_error(
    store_delete_objects(store, data_frame(name = "a"), 3, TRUE),
    class = "tar_condition_validate"
  )
  expect_false(file.exists(path))
  expect_false(store_exist_object(store))
})

tar_test("alternate storage with _targets.yaml", {
  path <- file.path(tempfile(), "custom", "store")
  tar_config_set(store = path)
  expect_equal(tar_config_get("store"), path)
  writeLines("x_line", "x_file.txt")
  tar_script({
    evalq({
      write_lines <- function(file) {
        file <- paste0(file, ".txt")
        writeLines("lines", file)
        file
      }
    }, envir = tar_option_get("envir"))
    list(
      tar_target(x, "x_file.txt", format = "file"),
      tar_target(y, readLines(x)),
      tar_target(z, c("a", "b", "c")),
      tar_target(z2, z, pattern = map(z)),
      tar_target(z3, write_lines(z2), pattern = map(z2), format = "file")
    )
  })
  # First run
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_false(file.exists(path_store_default()))
  expect_true(dir.exists(path))
  expect_true(dir.exists(file.path(path, "meta")))
  expect_true(file.exists(file.path(path, "meta", "process")))
  expect_true(file.exists(file.path(path, "meta", "progress")))
  expect_true(file.exists(file.path(path, "meta", "meta")))
  expect_true(dir.exists(file.path(path, "objects")))
  expect_true(file.exists(file.path(path, "objects", "y")))
  expect_true(file.exists(file.path(path, "objects", "z")))
  # Inspect values
  expect_equal(tar_read(x), "x_file.txt")
  expect_equal(tar_read(y), "x_line")
  expect_equal(tar_read(z), c("a", "b", "c"))
  expect_equal(unname(tar_read(z2)), c("a", "b", "c"))
  expect_equal(unname(tar_read(z3)), c("a.txt", "b.txt", "c.txt"))
  envir <- new.env(parent = emptyenv())
  expect_null(envir$z)
  tar_load(z, envir = envir)
  expect_equal(envir$z, c("a", "b", "c"))
  expect_null(envir$z3)
  tar_load(z3, envir = envir)
  expect_equal(unname(envir$z3), c("a.txt", "b.txt", "c.txt"))
  # Should be no invalidated targets
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_equal(unique(tar_progress()$progress), "skipped")
  # Moving data store should not invalidate targets.
  file.rename(path, path_store_default())
  unlink("_targets.yaml")
  expect_false(file.exists(path))
  expect_true(file.exists(path_store_default()))
  expect_equal(tar_outdated(callr_function = NULL), character(0))
  tar_make(callr_function = NULL, envir = tar_option_get("envir"))
  expect_equal(unique(tar_progress()$progress), "skipped")
})
