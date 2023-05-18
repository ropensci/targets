tar_test("parallel$validate() on a good queue", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = seq_len(3))
  expect_silent(q$validate())
})

tar_test("parallel$validate() with empty names", {
  q <- parallel_init(names = NULL, ranks = seq_len(3))
  expect_error(q$validate(), class = "tar_condition_validate")
})

tar_test("parallel$validate() with duplicated names", {
  q <- parallel_init(names = rep("a", 3), ranks = seq_len(3))
  expect_error(q$validate(), class = "tar_condition_validate")
})

tar_test("parallel$validate() with missing names", {
  q <- parallel_init(names = c("a", NA_character_), ranks = c(1L, 2L))
  expect_error(q$validate(), class = "tar_condition_validate")
})

tar_test("parallel$validate() with non-integer ranks", {
  data <- letters[seq_len(3)]
  names(data) <- data
  q <- parallel_new(data)
  expect_error(q$validate(), class = "tar_condition_validate")
})

tar_test("parallel$validate() with negative ranks", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = -seq_len(3))
  expect_error(q$validate(), class = "tar_condition_validate")
})

tar_test("parallel$validate() with missing ranks", {
  q <- parallel_init(names = c("a", "b"), ranks = c(1L, NA_integer_))
  expect_error(q$validate(), class = "tar_condition_validate")
})

tar_test("parallel$validate() with bad counter", {
  q <- parallel_init(names = c("a", "b"), ranks = c(1L, 2L))
  counter <- q$counter
  counter$count <- 0L
  expect_error(q$validate(), class = "tar_condition_validate")
})

tar_test("parallel$data empty", {
  q <- parallel_init()
  out <- q$data
  exp <- integer(0)
  names(exp) <- character(0)
  expect_identical(out, exp)
})

tar_test("parallel$counter empty", {
  q <- parallel_init()
  out <- q$counter
  expect_silent(counter_validate(out))
  expect_equal(out$count, 0L)
  expect_equal(counter_get_names(out), character(0))
})

tar_test("parallel$get_names() empty", {
  q <- parallel_init()
  expect_identical(q$get_names(), character(0))
})

tar_test("parallel$get_names() empty", {
  q <- parallel_init()
  expect_identical(q$get_ranks(), integer(0))
})

tar_test("parallel$data nonempty", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = seq_len(3))
  out <- q$data
  exp <- seq_len(3)
  names(exp) <- letters[exp]
  expect_identical(out, exp)
})

tar_test("queue counter nonempty", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = seq_len(3))
  out <- q$counter
  expect_silent(counter_validate(out))
  expect_equal(out$count, 3L)
  expect_equal(sort(counter_get_names(out)), sort(letters[seq_len(3)]))
})

tar_test("parallel$get_names() nonempty", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = seq_len(3))
  out <- q$get_names()
  exp <- letters[seq_len(3)]
  expect_identical(out, exp)
})

tar_test("parallel$get_ranks() nonempty", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = seq_len(3))
  out <- q$get_ranks()
  exp <- seq_len(3)
  expect_identical(out, exp)
})

tar_test("parallel$dequeue() on an empty queue", {
  q <- parallel_init()
  data <- q$data
  expect_identical(q$dequeue(), character(0))
  expect_identical(q$data, data)
})

tar_test("parallel$peek() on an empty queue", {
  q <- parallel_init()
  data <- q$data
  expect_identical(q$peek(), character(0))
  expect_identical(q$data, data)
})

tar_test("parallel$dequeue() with none ready", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = c(2L, 1L, 3L))
  data <- q$data
  expect_false(q$should_dequeue())
  expect_identical(q$dequeue(), "b")
  expect_identical(q$data, sort(data)[-1])
})

tar_test("parallel$dequeue() with one ready", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = c(2L, 0L, 3L))
  data <- q$data
  expect_identical(q$dequeue(), "b")
  expect_identical(q$data, sort(data)[-1])
  expect_equal(sort(counter_get_names(q$counter)), sort(c("a", "c")))
})

tar_test("parallel$peek() with one ready", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = c(2L, 0L, 3L))
  data <- q$data
  expect_identical(q$peek(), "b")
  expect_identical(q$data, data)
  expect_equal(sort(counter_get_names(q$counter)), sort(c("a", "b", "c")))
})

tar_test("parallel$dequeue() with multiple ready", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = c(0L, 0L, 3L))
  data <- q$data
  expect_identical(length(out <- q$dequeue()), 1L)
  data <- sort(data[names(data) != out])
  expect_identical(q$data, data)
})

tar_test("parallel$prepend() nothing on an empty queue", {
  q <- parallel_init()
  data <- q$data
  q$prepend(names = character(0))
  expect_identical(q$data, data)
  expect_equal(counter_get_names(q$counter), character(0))
})

tar_test("parallel$prepend() something on an empty queue", {
  q <- parallel_init()
  q$prepend(names = c("a", "b"), ranks = c(1L, 2L))
  out <- q$data
  exp <- c(1L, 2L)
  names(exp) <- c("a", "b")
  expect_identical(out, exp)
})

tar_test("parallel$prepend() something on a nonempty queue", {
  q <- parallel_init(names = c("x", "y"), ranks = c(0L, 3L))
  q$prepend(names = c("a", "b"), ranks = c(1L, 2L))
  out <- q$data
  exp <- c(1L, 2L, 0L, 3L)
  names(exp) <- c("a", "b", "x", "y")
  expect_identical(out, exp)
})

tar_test("parallel$prepend() default ranks", {
  q <- parallel_init(names = c("x", "y"), ranks = c(0L, 3L))
  q$prepend(names = c("a", "b"))
  out <- q$data
  exp <- c(0L, 0L, 0L, 3L)
  names(exp) <- c("a", "b", "x", "y")
  expect_identical(out, exp)
})

tar_test("parallel$increment_ranks() elementwise", {
  q <- parallel_init(names = c("x", "y", "z"), ranks = seq_len(3L))
  q$increment_ranks(names = c("y", "z"), by = c(-2L, 2L))
  expect_equal(q$get_ranks(), c(1L, 0L, 5L))
})

tar_test("parallel$increment_ranks() vectorized", {
  q <- parallel_init(names = c("x", "y", "z"), ranks = seq_len(3L))
  q$increment_ranks(names = c("y", "z"), by = 2L)
  expect_equal(q$get_ranks(), c(1L, 4L, 5L))
})

tar_test("parallel$is_nonempty() on an empty queue", {
  expect_false(parallel_init()$is_nonempty())
})

tar_test("parallel$is_nonempty() on a nonempty queue", {
  q <- parallel_init(names = letters[seq_len(3)], ranks = seq_len(3))
  expect_true(q$is_nonempty())
})

tar_test("parallel$should_dequeue() on an empty queue", {
  expect_false(parallel_init()$should_dequeue())
})

tar_test("parallel$should_dequeue() with no zero rank element", {
  q <- parallel_init(names = c("x", "y", "z"), ranks = seq_len(3))
  expect_false(parallel_init()$should_dequeue())
  expect_false(q$should_dequeue())
})

tar_test("parallel$should_dequeue() with a zero rank element first", {
  q <- parallel_init(names = c("x", "y", "z"), ranks = c(0L, 1L, 2L))
  expect_false(parallel_init()$should_dequeue())
  expect_true(q$should_dequeue())
  expect_equal(q$dequeue(), "x")
})

tar_test("parallel$should_dequeue() with a zero rank element last", {
  q <- parallel_init(names = c("x", "y", "z"), ranks = c(2L, 1L, 0L))
  expect_false(parallel_init()$should_dequeue())
  expect_true(q$should_dequeue())
  expect_equal(q$dequeue(), "z")
})

tar_test("parallel$rotate() on empty queue", {
  q <- parallel_init(names = character(0L), ranks = integer(0L))
  first_data <- q$data
  first_count <- q$counter$count
  expect_silent(q$rotate())
  second_data <- q$data
  second_count <- q$counter$count
  expect_equal(first_data, second_data)
  expect_equal(first_count, second_count)
})

tar_test("parallel$rotate() on queue with all 0 ranks", {
  q <- parallel_init(names = letters, ranks = rep(0L, length(letters)))
  exp <- rep(0L, length(letters))
  names(exp) <- letters
  expect_identical(q$data, exp)
  q$rotate()
  exp <- rep(0L, length(letters))
  names(exp) <- c(letters[-1L], letters[1L])
  expect_identical(q$data, exp)
  q$rotate()
  exp <- rep(0L, length(letters))
  names(exp) <- c(letters[-c(1L, 2L)], letters[c(1L, 2L)])
  expect_identical(q$data, exp)
})

tar_test("parallel$rotate() on queue with various ranks", {
  q <- parallel_init(names = letters[seq_len(4L)], ranks = c(3L, 0L, 4L, 2L))
  exp <- c(3L, 0L, 4L, 2L)
  names(exp) <- letters[seq_len(4L)]
  expect_identical(q$data, exp)
  q$rotate()
  exp <- c(3L, 4L, 2L, 0L)
  names(exp) <- c("a", "c", "d", "b")
  expect_identical(q$data, exp)
})
