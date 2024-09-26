tar_test("tar_sub_body()", {
  skip_cran()
  fun <- function(object, path) {
    saveRDS(object, path, version = version)
  }
  out <- tar_sub_body(fun, list(version = 3))
  expect_equal(names(formals(out)), c("object", "path"))
  expect_equal(
    setdiff(trimws(deparse(body(out))), c("{", "}")),
    "saveRDS(object, path, version = 3)"
  )
})
