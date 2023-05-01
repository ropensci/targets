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

tar_test("file path caches are correct", {
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
      tar_target(file_info, tar_runtime_object()$file_info),
      tar_target(
        file_exist, {
          file_info
          sort(names(tar_runtime_object()$file_exist$envir))
        }
      ),
      tar_target(
        file_info_exist, {
          file_exist
          sort(names(tar_runtime_object()$file_info_exist$envir))
        }
      )
    )
  )
  tar_make(callr_function = NULL, reporter = "silent")
  expect_null(tar_runtime$file_exist)
  expect_null(tar_runtime$file_info)
  expect_null(tar_runtime$file_info_exist)
  expect_equal(
    tar_read(file_exist),
    sort(
      c(
        path_objects(path_store_default(), c("x", "y", "file_info")),
        path_objects_dir(path_store_default()),
        path_scratch_dir(path_store_default())
      )
    )
  )
  expect_equal(
    tar_read(file_info_exist),
    sort(path_objects(path_store_default(), c("x", "y")))
  )
  info <- tar_read(file_info)
  expect_true(is.list(info))
  expect_true(all(c("size", "mtime_numeric") %in% names(info)))
  names <- sort(path_objects(path_store_default(), c("x", "y")))
  for (field in c("size", "mtime_numeric")) {
    expect_equal(sort(names(info[[field]])), names)
    expect_false(anyNA(info[[field]]))
  }
})
