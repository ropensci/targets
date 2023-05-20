tar_test("sequential$dequeue() and sequential$peek()", {
  q <- queue_init("sequential", letters)
  expect_equal(q$dequeue(), "a")
  expect_equal(q$dequeue(), "b")
  expect_equal(q$data, letters[c(-1L, -2L)])
})

tar_test("sequential$prepend()", {
  q <- sequential_init(c("a", "b"))
  q$prepend(c("c", "d"))
  expect_equal(q$data, c("c", "d", "a", "b"))
})

tar_test("sequential$append()", {
  q <- sequential_init(c("a", "b"))
  q$append(c("c", "d"))
  expect_equal(q$data, c("a", "b", "c", "d"))
})

tar_test("sequential$append0()", {
  q <- sequential_init(c("a", "b"))
  q$append0(name = "c")
  expect_equal(q$data, c("a", "b", "c"))
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
