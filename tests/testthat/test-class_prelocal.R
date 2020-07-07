tar_test("prelocal$validate()", {
  x <- algorithm_init("clustermq", pipeline_order())
  y <- x$produce_prelocal()
  expect_silent(y$validate())
})
