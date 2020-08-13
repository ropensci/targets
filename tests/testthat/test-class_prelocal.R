tar_test("prelocal$validate()", {
  x <- clustermq_init(pipeline_order())
  y <- x$produce_prelocal()
  expect_silent(y$validate())
})
