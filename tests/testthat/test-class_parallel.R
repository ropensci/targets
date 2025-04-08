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
  expect_equal(q$n_data, 4L)
  expect_equal(
    as.list(q$data)[sort(names(q$data))],
    list(a = 1, c = 2, y = 3, z = 3)[sort(c("a", "c", "y", "z"))]
  )
})

tar_test("parallel$increment_ranks()", {
  q <- parallel_init(
    names = c("a", "b", "c", "d"), ranks = c(1, 0, 2, 0),
    step = 4L
  )
  q$increment_ranks(names = c("b", "c", "x"), by = c(6, 7, 8))
  expect_equal(q$ready$data, c("b", "d"))
  expect_equal(length(q$data), 2L)
  expect_equal(q$n_data, 2L)
  expect_equal(as.list(q$data)[c("a", "c")], list(a = 1, c = 9))
  q$increment_ranks(names = c("a", "c"), by = -10)
  expect_equal(
    q$ready$data,
    c("b", "d", "a", "c", NA_character_, NA_character_)
  )
  expect_equal(length(q$data), 0L)
  expect_equal(q$n_data, 0L)
})

tar_test("parallel$reset()", {
  q <- parallel_init(
    names = letters,
    ranks = c(1, 2, rep(0, 24L)),
    step = 100L
  )
  expect_equal(q$dequeue(), "c")
  expect_equal(q$ready$data, letters[-c(1L, 2L)])
  expect_equal(q$ready$head, 2L)
  expect_equal(q$ready$tail, 24L)
  expect_equal(length(as.list(q$data)), 2L)
  expect_equal(
    as.list(q$data)[c("a", "b")],
    list(a = 1, b = 2)
  )
  expect_true(q$is_nonempty())
  q$reset()
  expect_equal(q$ready$data, character(0L))
  expect_equal(q$ready$head, 1L)
  expect_equal(q$ready$tail, 0L)
  expect_equal(as.list(q$data), list())
  expect_false(q$is_nonempty())
})
