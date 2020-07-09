test_that("sequential$dequeue()", {
  q <- queue_init("sequential", letters)
  expect_equal(q$dequeue(), "a")
  expect_equal(q$dequeue(), "b")
  expect_equal(q$data, letters[c(-1L, -2L)])
})

test_that("sequential$enqueue()", {
  q <- sequential_init(c("a", "b"))
  q$enqueue(c("c", "d"))
  expect_equal(q$data, c("c", "d", "a", "b"))
})

test_that("sequential$increment_ranks()", {
  q <- sequential_init(c("a", "b"))
  q$increment_ranks("a", 1L)
  expect_equal(q$data, c("a", "b"))
})

test_that("sequential$should_dequeue()", {
  q <- sequential_init(c("a"))
  expect_true(q$should_dequeue())
  q$dequeue()
  expect_false(q$should_dequeue())
})

test_that("sequential$validate()", {
  q <- queue_init("sequential", letters)
  expect_silent(q$validate())
})
