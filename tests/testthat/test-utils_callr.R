tar_test("callr_args_default", {
  skip_cran()
  expect_message(out <- callr_args_default(callr::r))
  expect_equal(out, tar_callr_args_default(callr::r))
})

tar_test("custom error classes forwarded to top level", {
  skip_cran()
  tar_script(
    tar_target(x, rlang::abort("msg", class = c("my_class_1", "my_class_2")))
  )
  out <- tryCatch(
    tar_make(callr_function = NULL),
    error = function(condition) class(condition)
  )
  expect_true("my_class_1" %in% out)
  expect_true("my_class_2" %in% out)
})

tar_test("custom error classes forwarded to top level", {
  skip_cran()
  tar_script(
    tar_target(x, rlang::abort("msg", class = c("my_class_1", "my_class_2")))
  )
  out <- tryCatch(
    tar_make(callr_arguments = list(show = FALSE), reporter = "silent"),
    error = function(condition) class(condition)
  )
  expect_true("my_class_1" %in% out)
  expect_true("my_class_2" %in% out)
})

tar_test("objects_exist and objects_info are correct", {
  skip_cran()
  tar_script(
    list(
      tar_target(x, 1),
      tar_target(y, x),
      tar_target(
        z, {
          file.create("z")
          "z"
        },
        format = "file"
      )
    )
  )
  tar_make(callr_function = NULL, reporter = "silent")
  tar_script(
    list(
      tar_target(objects_info, tar_runtime_object()$objects_info),
      tar_target(
        objects_exist, {
          objects_info
          sort(names(tar_runtime_object()$objects_exist$envir))
        }
      )
    )
  )
  tar_make(callr_function = NULL, reporter = "silent")
  expect_equal(
    tar_read(objects_exist),
    sort(path_objects(path_store_default(), c("x", "y", "objects_info")))
  )
  info <- tar_read(objects_info)
  expect_true(is.data.frame(info))
  expect_true(all(c("size", "isdir", "mtime") %in% colnames(info)))
  expect_equal(
    sort(rownames(info)),
    sort(path_objects(path_store_default(), c("x", "y")))
  )
})
