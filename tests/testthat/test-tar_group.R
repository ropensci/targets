tar_test("tar_group() works", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  out <- data_frame(
    x = seq_len(6),
    id = rep(letters[seq_len(3)], each = 2)
  )
  out <- dplyr::group_by(out, id)
  out <- tar_group(out)
  exp <- data_frame(
    x = seq_len(6),
    id = rep(letters[seq_len(3)], each = 2),
    tar_group = rep(seq_len(3), each = 2)
  )
  expect_equiv(out, exp)
})

tar_test("tar_group() needs group_by()", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  out <- data_frame(
    x = seq_len(6),
    id = rep(letters[seq_len(3)], each = 2)
  )
  expect_error(tar_group(out), class = "tar_condition_validate")
})
