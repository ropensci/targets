tar_test("dynamic map method", {
  x <- data.frame(x = seq_len(2))
  methods <- dynamic_init()
  expect_equal(methods$map(x), x)
})

tar_test("dynamic cross method", {
  x <- data.frame(x = seq_len(2))
  y <- data.frame(y = seq_len(2))
  methods <- dynamic_init()
  out <- methods$cross(x, y)
  exp <- data.frame(
    x = rep(seq_len(2), each = 2),
    y = rep(seq_len(2), times = 2)
  )
  expect_equal(out, exp)
})

tar_test("dynamic head method", {
  x <- data.frame(x = seq_len(10))
  methods <- dynamic_init()
  expect_equal(methods$head(x, n = 2), head(x, n = 2))
})

tar_test("dynamic tail method", {
  x <- data.frame(x = seq_len(10))
  methods <- dynamic_init()
  expect_equal(methods$tail(x, n = 2), data.frame(x = c(9L, 10L)))
})

tar_test("dynamic slice method", {
  x <- data.frame(x = seq_len(10))
  methods <- dynamic_init()
  out <- methods$slice(x, index = c(3L, 4L))
  expect_true(is.data.frame(out))
  expect_equal(dim(out), c(2L, 1L))
  expect_equal(colnames(out), "x")
  expect_equal(out$x, c(3L, 4L))
})

tar_test("dynamic slice method with out of bounds index", {
  x <- data.frame(x = seq_len(10))
  methods <- dynamic_init()
  expect_error(
    methods$slice(x, index = c(3L, 400L)),
    class = "tar_condition_validate"
  )
})

tar_test("dynamic sample method", {
  x <- data.frame(x = seq_len(10))
  methods <- dynamic_init()
  out <- methods$sample(x, n = 2)
  expect_true(is.data.frame(out))
  expect_equal(dim(out), c(2L, 1L))
  expect_equal(colnames(out), "x")
  expect_true(all(out$x %in% seq_len(10)))
})

tar_test("dynamic sample method, n too low", {
  x <- data.frame(x = seq_len(10))
  methods <- dynamic_init()
  expect_error(
    methods$sample(x, n = 0),
    class = "tar_condition_validate"
  )
})

tar_test("dynamic sample method, n too high", {
  x <- data.frame(x = seq_len(10))
  methods <- dynamic_init()
  expect_error(
    methods$sample(x, n = 1000),
    class = "tar_condition_validate"
  )
})
