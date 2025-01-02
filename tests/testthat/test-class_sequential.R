tar_test("sequential$dequeue()", {
  q <- queue_init("sequential", letters)
  expect_equal(q$dequeue(), "a")
  expect_equal(q$dequeue(), "b")
  expect_equal(q$data[q$head], "c")
  for (index in seq_len(length(letters) - 2L) + 2L) {
    expect_equal(q$dequeue(), letters[index])
  }
})

tar_test("sequential$prepend() with new queue", {
  q <- sequential_init(c("a", "b"))
  q$head <- 1L
  q$prepend(c("c", "d"))
  expect_true(q$is_nonempty())
  expect_equal(q$data, c("c", "d", "a", "b"))
  expect_equal(q$head, 1L)
  expect_equal(q$dequeue(), "c")
  expect_equal(q$dequeue(), "d")
  expect_equal(q$dequeue(), "a")
  expect_equal(q$dequeue(), "b")
  expect_false(q$is_nonempty())
})

tar_test("sequential$prepend() with active queue", {
  q <- sequential_init(c("a", "b"))
  q$head <- 2L
  q$prepend(c("c", "d"))
  expect_true(q$is_nonempty())
  expect_equal(q$data, c("a", "c", "d", "b"))
  expect_equal(q$head, 2L)
  expect_equal(q$dequeue(), "c")
  expect_equal(q$dequeue(), "d")
  expect_equal(q$dequeue(), "b")
  expect_false(q$is_nonempty())
})

tar_test("sequential$prepend() with finished queue", {
  q <- sequential_init(c("a", "b"))
  q$head <- 3L
  q$prepend(c("c", "d"))
  expect_true(q$is_nonempty())
  expect_equal(q$data, c("a", "b", "c", "d"))
  expect_equal(q$head, 3L)
  expect_equal(q$dequeue(), "c")
  expect_equal(q$dequeue(), "d")
  expect_false(q$is_nonempty())
})

tar_test("sequential$append()", {
  q <- sequential_init(c("a", "b"))
  q$append(c("c", "d"))
  expect_equal(q$data, c("a", "b", "c", "d"))
})

tar_test("sequential$increment_ranks()", {
  q <- sequential_init(c("a", "b"))
  q$increment_ranks("a", 1L)
  expect_equal(q$data, c("a", "b"))
})

tar_test("sequential$should_dequeue()", {
  q <- sequential_init(c("a"))
  expect_true(q$should_dequeue())
  q$dequeue()
  expect_false(q$should_dequeue())
})

tar_test("sequential$validate()", {
  q <- queue_init("sequential", letters)
  expect_silent(q$validate())
})
