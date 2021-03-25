tar_test("fst_tbl format", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    tibble::tibble(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "fst_tbl"
  )
  builder_update_build(x, envir = envir)
  builder_update_paths(x)
  builder_update_object(x)
  exp <- envir$f()
  out <- tibble::as_tibble(fst::read_fst(x$store$file$path))
  expect_equal(out, exp)
  expect_equal(target_read_value(x)$object, exp)
  expect_silent(target_validate(x))
})

tar_test("fst_tbl coercion", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  envir <- new.env(parent = baseenv())
  envir$f <- function() {
    data.frame(x = 1, y = 2)
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "fst_tbl"
  )
  builder_update_build(x, envir)
  expect_true(inherits(x$value$object, "tbl_df"))
  builder_update_paths(x)
  builder_update_object(x)
  expect_true(inherits(target_read_value(x)$object, "tbl_df"))
})

tar_test("bad compression level throws error", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  tar_script({
    list(
      tar_target(
        abc,
        data.frame(x = 1, y = 2),
        format = "fst_tbl",
        resources = list(compress = "bad")
      )
    )
  })
  expect_error(tar_make(callr_function = NULL), class = "condition_validate")
})

tar_test("fst_tbl packages", {
  x <- tar_target(x, 1, format = "fst_tbl")
  out <- sort(store_get_packages(x$store))
  expect_equal(out, sort(c("fst", "tibble")))
})
