tar_test("tar_throw_prelocal()", {
  expect_error(
    tar_throw_prelocal("x"),
    class = "tar_condition_prelocal"
  )
})
