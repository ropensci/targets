tar_test("fst_tbl format", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    tibble::tibble(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "fst_tbl"
  )
  store_update_stage_early(x$store, x$file, "abc", path_store_default())
  builder_update_build(x, envir = envir)
  builder_update_paths(x, path_store_default())
  builder_update_object(x)
  exp <- envir$f()
  out <- tibble::as_tibble(fst::read_fst(x$file$path))
  expect_equal(out, exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("fst_tbl coercion", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.frame(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "fst_tbl"
  )
  store_update_stage_early(x$store, x$file, "abc", path_store_default())
  builder_update_build(x, envir)
  expect_true(inherits(x$value$object, "tbl_df"))
  builder_update_paths(x, path_store_default())
  builder_update_object(x)
  expect_true(inherits(target_read_value(x)$object, "tbl_df"))
})

tar_test("bad compression level throws error (unstructured resources)", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "fst_tbl",
        resources = list(compress = "bad")
      )
    )
  })
  expect_warning(
    tar_target(
      abc,
      data.frame(x = 1, y = 2),
      format = "fst_tbl",
      resources = list(compress = "bad")
    ),
    class = "tar_condition_deprecate"
  )
  suppressWarnings(
    expect_error(
      tar_make(callr_function = NULL),
      class = "tar_condition_run"
    )
  )
})

tar_test("fst_tbl packages", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  x <- tar_target(x, 1, format = "fst_tbl")
  out <- sort(store_get_packages(x$store))
  expect_equal(out, sort(c("fst", "tibble")))
})

tar_test("does not inherit from tar_external", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  store <- tar_target(x, "x_value", format = "fst_tbl")$store
  expect_false(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  target <- tar_target(x, "x_value", format = "fst_tbl")
  store <- target$store
  file <- target$file
  file$path <- "path"
  expect_equal(store_row_path(store, file), NA_character_)
})

tar_test("store_path_from_name()", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  store <- tar_target(x, "x_value", format = "fst_tbl")$store
  out <- store_path_from_name(
    store,
    format = store$format,
    name = "x",
    path = "path",
    path_store = path_store_default()
  )
  expect_equal(
    out,
    path_objects(path_store_default(), "x")
  )
})
