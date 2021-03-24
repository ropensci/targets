tar_test("fst format", {
  skip_if_not_installed("fst")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.frame(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "fst"
  )
  builder_update_build(x, envir = envir)
  builder_update_paths(x)
  builder_update_object(x)
  exp <- envir$f()
  expect_equal(fst::read_fst(x$store$file$path), exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("bad compression level throws error", {
  skip_if_not_installed("fst")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "fst",
        resources = list(compress = "bad")
      )
    )
  })
  expect_error(tar_make(callr_function = NULL), class = "condition_validate")
})

tar_test("fst packages", {
  x <- tar_target(x, 1, format = "fst")
  out <- store_get_packages(x$store)
  expect_equal(out, "fst")
})
