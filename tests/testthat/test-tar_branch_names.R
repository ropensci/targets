tar_test("tar_branch_names()", {
  skip_cran()
  tar_script(
    {
      list(
        tar_target(w, 1),
        tar_target(x, seq_len(4)),
        tar_target(y, 2 * x, pattern = map(x)),
        tar_target(z, y, pattern = map(y))
      )
    },
    ask = FALSE
  )
  tar_make(callr_function = NULL)
  out <- tar_branch_names(z, c(2, 3))
  exp <- tar_meta(z, children)$children[[1]][c(2, 3)]
  expect_equal(out, exp)
})
