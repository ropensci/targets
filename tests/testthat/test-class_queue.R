tar_test("queue$data empty", {
  q <- queue_init()
  out <- q$data
  exp <- integer(0)
  names(exp) <- character(0)
  expect_identical(out, exp)
})

tar_test("queue$counter empty", {
  q <- queue_init()
  out <- q$counter
  expect_silent(counter_validate(out))
  expect_equal(out$count, 0L)
  expect_equal(counter_get_names(out), character(0))
})

tar_test("queue$get_names() empty", {
  q <- queue_init()
  expect_identical(q$get_names(), character(0))
})

tar_test("queue$get_names() empty", {
  q <- queue_init()
  expect_identical(q$get_ranks(), integer(0))
})

tar_test("queue$data nonempty", {
  q <- queue_init(names = letters[seq_len(3)], ranks = seq_len(3))
  out <- q$data
  exp <- seq_len(3)
  names(exp) <- letters[exp]
  expect_identical(out, exp)
})

tar_test("queue counter nonempty", {
  q <- queue_init(names = letters[seq_len(3)], ranks = seq_len(3))
  out <- q$counter
  expect_silent(counter_validate(out))
  expect_equal(out$count, 3L)
  expect_equal(sort(counter_get_names(out)), sort(letters[seq_len(3)]))
})

tar_test("queue$get_names() nonempty", {
  q <- queue_init(names = letters[seq_len(3)], ranks = seq_len(3))
  out <- q$get_names()
  exp <- letters[seq_len(3)]
  expect_identical(out, exp)
})

tar_test("queue$get_ranks() nonempty", {
  q <- queue_init(names = letters[seq_len(3)], ranks = seq_len(3))
  out <- q$get_ranks()
  exp <- seq_len(3)
  expect_identical(out, exp)
})

tar_test("queue$peek() on an empty queue", {
  q <- queue_init()
  data <- q$data
  expect_identical(q$peek(), character(0))
  expect_identical(q$data, data)
})

tar_test("queue$peek() with none ready", {
  q <- queue_init(names = letters[seq_len(3)], ranks = c(2L, 1L, 3L))
  data <- q$data
  expect_false(q$should_dequeue())
  expect_identical(q$peek(), "b")
  expect_identical(q$data, sort(data))
})

tar_test("queue$peek() with one ready", {
  q <- queue_init(names = letters[seq_len(3)], ranks = c(2L, 0L, 3L))
  data <- q$data
  expect_identical(q$peek(), "b")
  expect_identical(q$data, sort(data))
})

tar_test("queue$peek() with multiple ready", {
  q <- queue_init(names = letters[seq_len(3)], ranks = c(0L, 0L, 3L))
  data <- q$data
  expect_identical(length(q$peek()), 1L)
  expect_identical(q$data, sort(data))
})

tar_test("queue$dequeue() on an empty queue", {
  q <- queue_init()
  data <- q$data
  expect_identical(q$dequeue(), character(0))
  expect_identical(q$data, data)
})

tar_test("queue$dequeue() with none ready", {
  q <- queue_init(names = letters[seq_len(3)], ranks = c(2L, 1L, 3L))
  data <- q$data
  expect_false(q$should_dequeue())
  expect_identical(q$dequeue(), "b")
  expect_identical(q$data, sort(data)[-1])
})

tar_test("queue$dequeue() with one ready", {
  q <- queue_init(names = letters[seq_len(3)], ranks = c(2L, 0L, 3L))
  data <- q$data
  expect_identical(q$dequeue(), "b")
  expect_identical(q$data, sort(data)[-1])
  expect_equal(sort(counter_get_names(q$counter)), sort(c("a", "c")))
})

tar_test("queue$dequeue() with multiple ready", {
  q <- queue_init(names = letters[seq_len(3)], ranks = c(0L, 0L, 3L))
  data <- q$data
  expect_identical(length(out <- q$dequeue()), 1L)
  data <- sort(data[names(data) != out])
  expect_identical(q$data, data)
})

tar_test("queue$enqueue() nothing on an empty queue", {
  q <- queue_init()
  data <- q$data
  q$enqueue(names = character(0))
  expect_identical(q$data, data)
  expect_equal(counter_get_names(q$counter), character(0))
})

tar_test("queue$enqueue() something on an empty queue", {
  q <- queue_init()
  q$enqueue(names = c("a", "b"), ranks = c(1L, 2L))
  out <- q$data
  exp <- c(1L, 2L)
  names(exp) <- c("a", "b")
  expect_identical(out, exp)
})

tar_test("queue$enqueue() something on an empty queue", {
  q <- queue_init(names = c("x", "y"), ranks = c(0L, 3L))
  q$enqueue(names = c("a", "b"), ranks = c(1L, 2L))
  out <- q$data
  exp <- c(0L, 3L, 1L, 2L)
  names(exp) <- c("x", "y", "a", "b")
  expect_identical(out, exp)
})

tar_test("queue$enqueue() default ranks", {
  q <- queue_init(names = c("x", "y"), ranks = c(0L, 3L))
  q$enqueue(names = c("a", "b"))
  out <- q$data
  exp <- c(0L, 3L, 0L, 0L)
  names(exp) <- c("x", "y", "a", "b")
  expect_identical(out, exp)
})

tar_test("queue$set_ranks() elementwise", {
  q <- queue_init(names = c("x", "y", "z"), ranks = seq_len(3L))
  q$set_ranks(names = c("y", "z"), ranks = c(9L, 8L))
  expect_equal(q$get_ranks(), c(1L, 9L, 8L))
})

tar_test("queue$set_ranks() vectorized", {
  q <- queue_init(names = c("x", "y", "z"), ranks = seq_len(3L))
  q$set_ranks(names = c("y", "z"), ranks = 17L)
  expect_equal(q$get_ranks(), c(1L, 17L, 17L))
})

tar_test("queue$increment_ranks() elementwise", {
  q <- queue_init(names = c("x", "y", "z"), ranks = seq_len(3L))
  q$increment_ranks(names = c("y", "z"), by = c(-2L, 2L))
  expect_equal(q$get_ranks(), c(1L, 0L, 5L))
})

tar_test("queue$increment_ranks() vectorized", {
  q <- queue_init(names = c("x", "y", "z"), ranks = seq_len(3L))
  q$increment_ranks(names = c("y", "z"), by = 2L)
  expect_equal(q$get_ranks(), c(1L, 4L, 5L))
})

tar_test("queue$get_count() on an empty queue", {
  expect_equal(queue_init()$get_count(), 0L)
})

tar_test("queue$get_count() on a nonempty queue", {
  q <- queue_init(names = letters[seq_len(3)], ranks = seq_len(3))
  expect_equal(q$get_count(), 3L)
})

tar_test("queue$is_nonempty() on an empty queue", {
  expect_false(queue_init()$is_nonempty())
})

tar_test("queue$is_nonempty() on a nonempty queue", {
  q <- queue_init(names = letters[seq_len(3)], ranks = seq_len(3))
  expect_true(q$is_nonempty())
})

tar_test("queue$should_dequeue() on an empty queue", {
  expect_false(queue_init()$should_dequeue())
})

tar_test("queue$should_dequeue() with no zero rank element", {
  q <- queue_init(names = c("x", "y", "z"), ranks = seq_len(3))
  expect_false(queue_init()$should_dequeue())
})

tar_test("queue$should_dequeue() with a zero rank element", {
  q <- queue_init(names = c("x", "y", "z"), ranks = c(2L, 1L, 0L))
  expect_false(queue_init()$should_dequeue())
})

tar_test("q$exists_name() after enqueue", {
  q <- queue_init(names = c("x", "y"), ranks = c(0L, 3L))
  expect_false(q$exists_name("a"))
  expect_false(q$exists_name("b"))
  q$enqueue(names = c("a", "b"), ranks = c(1L, 2L))
  expect_true(q$exists_name("a"))
  expect_true(q$exists_name("b"))
})

tar_test("q$exists_name() after unwise dequeue", {
  q <- queue_init(names = c("x", "y"), ranks = c(1L, 3L))
  expect_true(q$exists_name("x"))
  expect_true(q$exists_name("y"))
  q$dequeue()
  expect_false(q$exists_name("x"))
  expect_true(q$exists_name("y"))
})

tar_test("q$exists_name() after prudent dequeue", {
  q <- queue_init(names = c("x", "y"), ranks = c(1L, 0L))
  expect_true(q$exists_name("x"))
  expect_true(q$exists_name("y"))
  q$dequeue()
  expect_true(q$exists_name("x"))
  expect_false(q$exists_name("y"))
})

tar_test("q$filter_exists()", {
  q <- queue_init(names = c("x", "y"), ranks = c(1L, 0L))
  out <- q$filter_exists(c("a", "x", "b", "y"))
  expect_equal(sort(out), sort(c("x", "y")))
  out <- q$filter_exists(c("a", "b", "y"))
  expect_equal(out, "y")
  out <- q$filter_exists(c("a"))
  expect_equal(out, character(0))
})

tar_test("queue$validate() on a good queue", {
  q <- queue_init(names = letters[seq_len(3)], ranks = seq_len(3))
  expect_silent(q$validate())
})

tar_test("queue$validate() with empty names", {
  q <- queue_init(names = NULL, ranks = seq_len(3))
  expect_error(q$validate(), class = "condition_validate")
})

tar_test("queue$validate() with duplicated names", {
  q <- queue_init(names = rep("a", 3), ranks = seq_len(3))
  expect_error(q$validate(), class = "condition_validate")
})

tar_test("queue$validate() with missing names", {
  q <- queue_init(names = c("a", NA_character_), ranks = c(1L, 2L))
  expect_error(q$validate(), class = "condition_validate")
})

tar_test("queue$validate() with non-integer ranks", {
  data <- letters[seq_len(3)]
  names(data) <- data
  q <- queue_new(data)
  expect_error(q$validate(), class = "condition_validate")
})

tar_test("queue$validate() with negative ranks", {
  q <- queue_init(names = letters[seq_len(3)], ranks = -seq_len(3))
  expect_error(q$validate(), class = "condition_validate")
})

tar_test("queue$validate() with missing ranks", {
  q <- queue_init(names = c("a", "b"), ranks = c(1L, NA_integer_))
  expect_error(q$validate(), class = "condition_validate")
})

tar_test("queue$validate() with bad counter", {
  q <- queue_init(names = c("a", "b"), ranks = c(1L, 2L))
  counter <- q$counter
  counter$count <- 0L
  expect_error(q$validate(), class = "condition_validate")
})
