tar_test("fst format", {
  skip_if_not_installed("fst")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.frame(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "fst",
    envir = envir
  )
  builder_update_build(x)
  builder_update_path(x)
  builder_update_object(x)
  exp <- envir$f()
  expect_equal(fst::read_fst(x$store$file$path), exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})
