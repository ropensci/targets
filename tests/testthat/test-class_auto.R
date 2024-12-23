tar_test("auto format", {
  skip_cran()
  on.exit(unlink("file_y.txt"))
  tar_script({
    save_y <- function() {
      writeLines("contents", "file_y.txt")
      "file_y.txt"
    }
    list(
      tar_target(x, "no_file", format = "auto"),
      tar_target(y, save_y(), format = "auto")
    )
  })
  tar_make(callr_function = NULL)
  expect_equal(tar_read(x), "no_file")
  expect_equal(tar_read(y), "file_y.txt")
  expect_equal(readLines("file_y.txt"), "contents")
  expect_equal(tar_meta(tidyselect::any_of("x"))$format, "qs")
  expect_equal(tar_outdated(callr_function = NULL), character(0L))
  tar_make(callr_function = NULL)
  expect_equal(unique(tar_progress()$progress), "skipped")
})

tar_test("auto format with error null", {
  skip_cran()
  on.exit(unlink("file_y.txt"))
  tar_script({
    tar_option_set(error = "null")
    list(
      tar_target(x, stop("message_x"), format = "auto"),
      tar_target(y, stop("message_y"), format = "auto")
    )
  })
  suppressWarnings(tar_make(callr_function = NULL))
  expect_null(tar_read(x))
  expect_null(tar_read(y))
  expect_false(file.exists("file_y.txt"))
  expect_equal(tar_meta(tidyselect::any_of("x"))$format, "null")
  expect_equal(tar_meta(tidyselect::any_of("y"))$format, "null")
  expect_equal(sort(tar_outdated(callr_function = NULL)), sort(c("x", "y")))
})

tar_test("store_get_packages.tar_auto()", {
  expect_equal(store_get_packages.tar_auto(list()), "qs")
})

tar_test("store_reformat_auto()", {
  target <- list(settings = list(format = "qs"))
  expect_null(store_reformat_auto(target))
})

tar_test("store_path_from_name() file", {
  store <- tar_target(x, "x_value", format = "auto")$store
  out <- store_path_from_name(
    store,
    format = "file",
    name = "x",
    path = "path",
    path_store = path_store_default()
  )
  expect_equal(out, "path")
})

tar_test("store_path_from_name() file", {
  store <- tar_target(x, "x_value", format = "auto")$store
  out <- store_path_from_name(
    store,
    format = "qs",
    name = "x",
    path = "path",
    path_store = path_store_default()
  )
  expect_equal(out, path_objects(path_store_default(), "x"))
})
