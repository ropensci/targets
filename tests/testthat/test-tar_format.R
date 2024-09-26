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
  expect_length(format, 1L)
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  expect_equal(format[1L], "format_custom")
  expect_true(any(grepl("^read=+.", format)))
  expect_true(any(grepl("^write=+.", format)))
  expect_true(any(grepl("^marshal=+.", format)))
  expect_true(any(grepl("^unmarshal=+.", format)))
  expect_true(any(grepl("^repository=", format)))
})

tar_test("same with deprecated repository argument", {
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
  expect_length(format, 1L)
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  expect_equal(format[1], "format_custom")
  expect_true(any(grepl("^read=+.", format)))
  expect_true(any(grepl("^write=+.", format)))
  expect_true(any(grepl("^marshal=+.", format)))
  expect_true(any(grepl("^unmarshal=+.", format)))
  expect_true(any(grepl("^repository=aws", format)))
})

tar_test("tar_format() default arguments are short", {
  format <- tar_format()
  expect_length(format, 1L)
  format <- unlist(strsplit(format, split = "&", fixed = TRUE))
  expect_equal(format[1], "format_custom")
  expect_true(any(grepl("^read=$", format)))
  expect_true(any(grepl("^write=$", format)))
  expect_true(any(grepl("^marshal=$", format)))
  expect_true(any(grepl("^unmarshal=$", format)))
  expect_true(any(grepl("^repository=$", format)))
})

# nolint start
tar_test("Function assertions", {
  expect_error(
    tar_format(
      read = function(x) {
        keras::load_model_hdf5(x)
      }),
    class = "tar_condition_validate"
  )
})
# nolint end

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

tar_test("custom format envvar resources", {
  tar_script(
    tar_target(
      name = target_name,
      command = data.frame(x = 1L),
      format = tar_format(
        read = function(path) {
          readRDS(file = path)
        },
        write = function(object, path) {
          version <- as.integer(Sys.getenv("SERIALIZATION", unset = ""))
          saveRDS(object = object, file = path, version = version)
        }
      ),
      resources = tar_resources(
        custom_format = tar_resources_custom_format(
          envvars = c(SERIALIZATION = "3")
        )
      )
    )
  )
  tar_make(callr_function = NULL)
  expect_equal(tar_read(target_name), data.frame(x = 1L))
  expect_equal(Sys.getenv("SERIALIZATION", unset = ""), "")
})

tar_test("custom format insertions", {
  tar_script(
    tar_target(
      name = target_name,
      command = data.frame(x = 1L),
      format = tar_format(
        read = function(path) {
          readRDS(file = path)
        },
        write = function(object, path) {
          saveRDS(object = object, file = path, version = VERSION_VARIABLE)
        },
        substitute = list(VERSION_VARIABLE = 3)
      )
    )
  )
  tar_make(callr_function = NULL)
  expect_equal(tar_read(target_name), data.frame(x = 1L))
})

tar_test("patterns are marshaled correctly", {
  skip_cran()
  skip_on_os("windows")
  skip_on_os("solaris")
  skip_if_not_installed("crew", minimum_version = "0.9.0")
  skip_if_not_installed("torch")
  on.exit(crew_test_sleep())
  tar_script({
    tar_option_set(controller = crew::crew_controller_local())
    list(
      tar_target(x, c(1L, 2L)),
      tar_target(
        y,
        torch::torch_tensor(x),
        pattern = map(x),
        format = "torch",
        iteration = "list"
      ),
      tar_target(
        z,
        y[[1]] + y[[2]],
        format = "torch"
      )
    )
  })
  tar_make(callr_function = NULL)
  tar_load(z)
  expect_s3_class(z, "torch_tensor")
  expect_true(identical(as.integer(z), 3L))
})
