tar_test("parallel$validate() on a good queue", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = seq_len(3))
  expect_silent(q$validate())
})

tar_test("parallel$validate() with non-numeric ranks", {
  data <- letters[seq_len(3)]
  names(data) <- data
  q <- parallel_new(data)
  expect_error(q$validate(), class = "tar_condition_validate")
})

tar_test("parallel$is_nonempty()", {
  q <- parallel_init()
  expect_false(q$is_nonempty())
  expect_equal(length(q$data), q$n_data)
  expect_equal(q$n_data, 0L)
  expect_equal(length(q$ready$data), 0L)
  q$append(names = "abc", rank = 1L)
  expect_true(q$is_nonempty())
  expect_equal(length(q$data), q$n_data)
  expect_equal(q$n_data, 1L)
  q$increment_ranks(names = "abc", by = -1L)
  expect_equal(length(q$data), q$n_data)
  expect_equal(q$n_data, 0L)
  expect_true(q$is_nonempty())
  expect_equal(q$dequeue(), "abc")
  expect_equal(length(q$data), q$n_data)
  expect_equal(q$n_data, 0L)
  expect_false(q$is_nonempty())
})

tar_test("parallel$dequeue() and parallel$should_dequeue()", {
  q <- parallel_init(names = c("a", "b", "c", "d"), ranks = c(1, 0, 2, 0))
  expect_equal(as.list(q$data), list(a = 1, c = 2))
  expect_equal(q$n_data, 2L)
  expect_equal(q$ready$data, c("b", "d"))
  expect_true(q$should_dequeue())
  expect_equal(q$dequeue(), "b")
  expect_true(q$should_dequeue())
  expect_equal(q$dequeue(), "d")
  expect_false(q$should_dequeue())
  expect_null(q$dequeue())
  q$increment_ranks(names = c("a", "c"), by = c(-1L, -2L))
  expect_true(q$should_dequeue())
  expect_equal(q$dequeue(), "a")
  expect_true(q$should_dequeue())
  expect_equal(q$dequeue(), "c")
  expect_false(q$should_dequeue())
  expect_null(q$dequeue())
})

tar_test("parallel$append()", {
  q <- parallel_init(
    names = c("a", "b", "c", "d"), ranks = c(1, 0, 2, 0),
    step = 4L
  )
  q$append(names = c("x", "y"), ranks = c(0, 3))
  expect_equal(q$ready$data, c("b", "d", "x", rep(NA_character_, 3L)))
  expect_equal(length(as.list(q$data)), 3L)
  expect_equal(
    as.list(q$data)[sort(names(q$data))],
    list(a = 1, c = 2, y = 3)[sort(c("a", "c", "y"))]
  )
  q$append(names = c("w", "z"), ranks = c(0, 3))
  expect_equal(q$ready$data, c("b", "d", "x", "w", rep(NA_character_, 2L)))
  expect_equal(length(as.list(q$data)), 4L)
  expect_equal(
    as.list(q$data)[sort(names(q$data))],
    list(a = 1, c = 2, y = 3, z = 3)[sort(c("a", "c", "y", "z"))]
  )
})

tar_test("parallel$prepend()", {
  q <- parallel_init(
    names = c("a", "b", "c", "d"), ranks = c(1, 0, 2, 0),
    step = 4L
  )
  q$prepend(names = c("x", "y"), ranks = c(0, 3))
  expect_equal(q$ready$data, c("x", "b", "d"))
  expect_equal(length(as.list(q$data)), 3L)
  expect_equal(
    as.list(q$data)[sort(names(q$data))],
    list(a = 1, c = 2, y = 3)[sort(c("a", "c", "y"))]
  )
  q$prepend(names = c("w", "z"), ranks = c(0, 3))
  expect_equal(q$ready$data, c("w", "x", "b", "d"))
  expect_equal(length(as.list(q$data)), 4L)
  expect_equal(
    as.list(q$data)[sort(names(q$data))],
    list(a = 1, c = 2, y = 3, z = 3)[sort(c("a", "c", "y", "z"))]
  )
})

tar_test("parallel$increment_ranks() elementwise", {
  
})
