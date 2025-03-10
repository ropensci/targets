tar_test("parquet format", {
  skip_cran()
  skip_if_not_installed("arrow")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.frame(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "parquet"
  )
  store_update_stage_early(x$store, x$file, "abc", path_store_default())
  builder_update_build(x, envir)
  builder_update_paths(x, path_store_default())
  builder_update_object(x)
  exp <- envir$f()
  expect_equal(
    as.data.frame(
      eval(parse(text = "arrow::read_parquet"))(x$file$path)
    ),
    as.data.frame(exp)
  )
  expect_equal(
    as.data.frame(target_read_value(x)$object),
    as.data.frame(exp)
  )
  expect_silent(target_validate(x))
})

tar_test("bad compression level throws error (structured resources)", {
  skip_cran()
  skip_if_not_installed("arrow")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "parquet",
        resources = tar_resources(
          tar_resources_parquet(compression = "bad")
        )
      )
    )
  })
  expect_error(tar_make(callr_function = NULL))
})

tar_test("bad compression level throws error (unstructured resources)", {
  skip_cran()
  skip_if_not_installed("arrow")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "parquet",
        resources = list(compression = "bad")
      )
    )
  })
  expect_warning(
    tar_target(
      abc,
      data.frame(x = 1, y = 2),
      format = "parquet",
      resources = list(compression = "bad")
    ),
    class = "tar_condition_deprecate"
  )
  suppressWarnings(
    expect_error(tar_make(callr_function = NULL))
  )
})

tar_test("parquet packages", {
  skip_cran()
  skip_if_not_installed("arrow")
  x <- tar_target(x, 1, format = "parquet")
  out <- store_get_packages(x$store)
  expect_equal(out, "arrow")
})

tar_test("parquet format captures error messages", {
  skip_cran()
  skip_if_not_installed("arrow")
  tar_script(tar_target(x, stop("message123"), format = "parquet"))
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
  expect_true(grepl("message123$", tar_meta(x, error)$error))
})

tar_test("same with error = \"continue\"", {
  skip_cran()
  skip_if_not_installed("arrow")
  tar_script(
    tar_target(x, stop("message123"), format = "parquet", error = "continue")
  )
  tar_make(callr_function = NULL)
  expect_true(grepl("message123$", tar_meta(x, error)$error))
})

tar_test("parquet format cannot store non-data-frames", {
  skip_cran()
  skip_if_not_installed("arrow")
  tar_script(tar_target(x, 1:2, format = "parquet"))
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
})

tar_test("same with error = \"continue\"", {
  skip_cran()
  skip_if_not_installed("arrow")
  tar_script(tar_target(x, 1:2, format = "parquet", error = "continue"))
  tar_make(callr_function = NULL)
  expect_true(nchar(tar_meta(x, error)$error[1]) > 5)
})

tar_test("does not inherit from tar_external", {
  skip_cran()
  skip_if_not_installed("arrow")
  store <- tar_target(x, "x_value", format = "parquet")$store
  expect_false(inherits(store, "tar_external"))
})

tar_test("store_row_path()", {
  skip_cran()
  skip_if_not_installed("arrow")
  target <- tar_target(x, "x_value", format = "parquet")
  store <- target$store
  file <- target$file
  file$path <- "path"
  expect_equal(store_row_path(store, file), NA_character_)
})

tar_test("store_path_from_name()", {
  skip_cran()
  skip_if_not_installed("arrow")
  store <- tar_target(x, "x_value", format = "parquet")$store
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

tar_test("parquet and NULL", {
  skip_cran()
  skip_if_not_installed("arrow")
  tar_script(
    list(
      tar_target(x, NULL, format = "parquet", memory = "persistent"),
      tar_target(y, x, memory = "persistent")
    )
  )
  tar_make(callr_function = NULL)
  expect_equal(
    as.data.frame(tar_read(x)),
    as.data.frame(NULL)
  )
  expect_equal(
    as.data.frame(tar_read(y)),
    as.data.frame(NULL)
  )
})
