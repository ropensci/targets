tar_test("tar_format() generates a format string", {
  format <- tar_format(
    read = function(path) {
      keras::load_model_hdf5(path)
    },
    write = function(object, path) {
      keras::save_model_hdf5(object = object, filepath = path)
    },
    marshal = function(object) {
      keras::serialize_model(object)
    },
    unmarshal = function(object) {
      keras::unserialize_model(object)
    },
  )
  expect_equal(length(format), 1)
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  expect_equal(format[1], "format_custom")
  expect_true(any(grepl("^read=+.", format)))
  expect_true(any(grepl("^write=+.", format)))
  expect_true(any(grepl("^marshal=+.", format)))
  expect_true(any(grepl("^unmarshal=+.", format)))
  expect_true(any(grepl("^repository=", format)))
})

tar_test("tar_format() default arguments are short", {
  format <- tar_format()
  expect_equal(length(format), 1)
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  expect_equal(format[1], "format_custom")
  expect_true(any(grepl("^read=$", format)))
  expect_true(any(grepl("^write=$", format)))
  expect_true(any(grepl("^marshal=$", format)))
  expect_true(any(grepl("^unmarshal=$", format)))
  expect_true(any(grepl("^repository=$", format)))
})

# nolint start
tar_test("Deprecated tar_format() repostory arg", {
  expect_error(
    tar_format(
      read = function(x) {
        keras::load_model_hdf5(x)
      }),
    class = "tar_condition_validate"
  )
})
# nolint end

tar_test("tar_format() generates a format string", {
  expect_warning(
    format <- tar_format(
      read = function(path) {
        keras::load_model_hdf5(path)
      },
      write = function(object, path) {
        keras::save_model_hdf5(object = object, filepath = path)
      },
      marshal = function(object) {
        keras::serialize_model(object)
      },
      unmarshal = function(object) {
        keras::unserialize_model(object)
      },
      repository = "aws"
    ),
    class = "tar_condition_deprecate"
  )
  expect_equal(length(format), 1)
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  expect_equal(format[1], "format_custom")
  expect_true(any(grepl("^read=+.", format)))
  expect_true(any(grepl("^write=+.", format)))
  expect_true(any(grepl("^marshal=+.", format)))
  expect_true(any(grepl("^unmarshal=+.", format)))
  expect_true(any(grepl("^repository=aws", format)))
})

tar_test("custom format is not allowed to create a directory", {
  skip_cran()
  tar_script({
    format <- tar_format(
      read = function(path) {
        readRDS(file.path(path, "x"))
      },
      write = function(object, path) {
        unlink(path, recursive = TRUE)
        dir.create(path)
        saveRDS(object, file.path(path, "x"))
      }
    )
    tar_target(x, 1, format = format)
  })
  expect_error(
    tar_make(callr_function = NULL),
    class = "tar_condition_run"
  )
})
