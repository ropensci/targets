test_that("tar_format() generates a format string", {
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
    repository = "default"
  )
  expect_equal(length(format), 1)
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  expect_equal(format[1], "format_custom")
  expect_true(any(grepl("^read=+.", format)))
  expect_true(any(grepl("^write=+.", format)))
  expect_true(any(grepl("^marshal=+.", format)))
  expect_true(any(grepl("^unmarshal=+.", format)))
  expect_true(any(grepl("^repository=default", format)))
})

test_that("tar_format() default arguments are acceptable", {
  format <- tar_format()
  expect_equal(length(format), 1)
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  expect_equal(format[1], "format_custom")
  expect_true(any(grepl("^read=+.", format)))
  expect_true(any(grepl("^write=+.", format)))
  expect_true(any(grepl("^marshal=+.", format)))
  expect_true(any(grepl("^unmarshal=+.", format)))
  expect_true(any(grepl("^repository=default", format)))
})

test_that("tar_format() bad function arguments", {
  expect_error(
    tar_format(
      read = function(x) {
        keras::load_model_hdf5(x)
      }),
    class = "tar_condition_validate"
  )
})
