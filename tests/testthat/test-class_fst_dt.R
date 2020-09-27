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
  builder_update_paths(x)
  builder_update_object(x)
  exp <- envir$f()
  file <- x$store$file
  out <- fst::read_fst(file$path, as.data.table = TRUE)
  expect_equal(out, exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("bad compression level throws error", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  tar_script({
    tar_pipeline(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "fst_dt",
        resources = list(compress = "bad")
      )
    )
  })
  expect_error(tar_make(callr_function = NULL), class = "condition_validate")
})
