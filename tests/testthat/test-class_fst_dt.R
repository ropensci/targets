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
    format = "fst_dt",
    envir = envir
  )
  builder_update_build(x)
  builder_update_path(x)
  builder_update_object(x)
  exp <- envir$f()
  file <- x$store$file
  out <- fst::read_fst(file$path, as.data.table = TRUE)
  expect_equal(out, exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})
