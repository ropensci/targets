tar_test("fst_dt format", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.table::data.table(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "fst_dt"
  )
  store_update_stage_early(x$store, x$file, "abc", path_store_default())
  builder_update_build(x, envir = envir)
  builder_update_paths(x, path_store_default())
  builder_update_object(x)
  exp <- envir$f()
  file <- x$file
  out <- fst::read_fst(file$path, as.data.table = TRUE)
  expect_equal(out, exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("bad compression level throws error (unstructured resources)", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "fst_dt",
        resources = list(compress = "bad")
      )
    )
  })
  expect_warning(
    tar_target(
      abc,
      data.frame(x = 1, y = 2),
      format = "fst_dt",
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

tar_test("fst_dt packages", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  x <- tar_target(x, 1, format = "fst_dt")
  out <- sort(store_get_packages(x$store))
  expect_equal(out, sort(c("fst", "data.table")))
})

tar_test("does not inherit from tar_external", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  store <- tar_target(x, "x_value", format = "fst_dt")$store
  expect_false(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  target <- tar_target(x, "x_value", format = "fst_dt")
  store <- target$store
  file <- target$file
  file$path <- "path"
  expect_equal(store_row_path(store, file), NA_character_)
})

tar_test("store_path_from_name()", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  store <- tar_target(x, "x_value", format = "fst_dt")$store
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

tar_test("store_copy_object() works for data tables", {
  skip_cran()
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  tar_script(
    list(
      tar_target(
        target_a,
        data.table::data.table(a = c(1, 2, 3)),
        format = "fst_dt"
      ),
      tar_target(
        target_b,
        rowSums(target_a[, b := c(1, 2, 3)]),
        format = "fst_dt"
      ),
      tar_target(
        target_c,
        colnames(target_a)
      )
    ),
    ask = FALSE
  )
  tar_make(callr_function = NULL)
  expect_equal(tar_read(target_c), "a")
})
