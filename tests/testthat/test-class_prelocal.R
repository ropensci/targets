tar_test("prelocal$validate() from clustermq", {
  x <- clustermq_init(pipeline_order())
  y <- x$produce_prelocal()
  expect_silent(y$validate())
})

tar_test("prelocal$validate() from crew", {
  x <- crew_init(pipeline_order())
  y <- x$produce_prelocal()
  expect_silent(y$validate())
})

tar_test("prelocal$tar_assert_deployment()", {
  x <- clustermq_init(pipeline_order())
  x$start()
  y <- x$produce_prelocal()
  t1 <- tar_target(x, 1, deployment = "main")
  t2 <- tar_target(x, 1, deployment = "worker")
  expect_silent(y$tar_assert_deployment(t1))
  expect_error(
    y$tar_assert_deployment(t2),
    class = "tar_condition_prelocal"
  )
})
