tar_test("empty queue", {
  q <- sequential_init()
  expect_equal(q$data, character(0L))
  expect_equal(q$head, 1L)
  expect_equal(q$tail, 0L)
  expect_silent(q$validate())
})

tar_test("sequential$is_nonempty() and sequential$should_dequeue()", {
  q <- sequential_init()
  expect_false(q$is_nonempty())
  expect_false(q$should_dequeue())
  q <- sequential_init(letters)
  expect_true(q$is_nonempty())
  expect_true(q$should_dequeue())
  while (q$is_nonempty()) {
    q$dequeue()
  }
  expect_false(q$is_nonempty())
  expect_false(q$should_dequeue())
  expect_equal(q$head, 27L)
  expect_equal(q$tail, 26L)
  expect_null(q$dequeue())
  q$data <- c(q$data, rep(NA_character_, 3L))
  expect_false(q$is_nonempty())
  expect_false(q$should_dequeue())
})

tar_test("sequential$clean()", {
  q <- sequential_init(step = 100L)
  for (index in seq_len(2L)) {
    q$clean()
    expect_equal(q$data, character(0L))
    expect_equal(q$head, 1L)
    expect_equal(q$tail, 0L)
  }
  q$append(letters)
  for (index in seq_len(4L)) {
    expect_equal(q$dequeue(), letters[index])
  }
  expect_equal(q$data, c(letters, rep(NA_character_, 74L)))
  expect_equal(q$head, 5L)
  expect_equal(q$data[q$head], "e")
  expect_equal(q$data[q$tail], "z")
  for (index in seq_len(2L)) {
    q$clean()
    expect_equal(q$head, 1L)
    expect_equal(q$tail, 22L)
    expect_equal(q$data[q$head], "e")
    expect_equal(q$data[q$tail], "z")
    expect_equal(q$data, c(letters[-seq_len(4L)], rep(NA_character_, 74L)))
  }
  while (q$is_nonempty()) {
    expect_true(nzchar(q$dequeue()))
  }
  expect_equal(q$data, c(letters[-seq_len(4L)], rep(NA_character_, 74L)))
  expect_equal(q$tail, 22L)
  expect_equal(q$data[q$tail], "z")
  expect_equal(q$head, 23L)
  expect_true(is.na(q$data[q$head]))
  expect_null(q$dequeue())
  for (index in seq_len(2L)) {
    q$clean()
    expect_equal(q$data, rep(NA_character_, 74L))
    expect_equal(q$head, 1L)
    expect_equal(q$tail, 0L)
  }
})

tar_test("sequential$extend()", {
  q <- sequential_init(step = 100L)
  q$extend(n = 50)
  expect_equal(q$data, rep(NA_character_, 100L))
  expect_equal(q$head, 1L)
  expect_equal(q$tail, 0L)
  q <- sequential_init(letters, step = 100L)
  expect_equal(q$dequeue(), "a")
  q$extend(n = 100L)
  expect_equal(q$data, c(letters[-1L], rep(NA_character_, 100L)))
  expect_equal(q$head, 1L)
  expect_equal(q$tail, 25L)
  q$extend(n = 100L)
  expect_equal(q$data, c(letters[-1L], rep(NA_character_, 200L)))
  expect_equal(q$head, 1L)
  expect_equal(q$tail, 25L)
})

tar_test("sequential$dequeue()", {
  q <- sequential_init(letters, step = 100L)
  expect_equal(q$dequeue(), "a")
  expect_equal(q$dequeue(), "b")
  expect_equal(q$data[q$head], "c")
  for (index in seq_len(length(letters) - 2L) + 2L) {
    expect_equal(q$dequeue(), letters[index])
  }
  expect_null(q$dequeue())
  q$extend(n = 100L)
  expect_null(q$dequeue())
  q$append(names = letters)
  for (index in seq_len(length(letters))) {
    expect_equal(q$dequeue(), letters[index])
  }
  expect_null(q$dequeue())
  expect_equal(q$data, c(letters, rep(NA_character_, 74L)))
  expect_equal(q$head, 27L)
  expect_equal(q$tail, 26L)
})

tar_test("sequential$append()", {
  q <- sequential_init(step = 100L)
  q$append(c("a", "b"))
  q$append(c("x", "y"))
  expect_equal(q$data, c("a", "b", "x", "y", rep(NA_character_, 96L)))
  expect_equal(q$head, 1L)
  expect_equal(q$tail, 4L)
  expect_equal(q$dequeue(), "a")
  expect_equal(q$data, c("a", "b", "x", "y", rep(NA_character_, 96L)))
  expect_equal(q$head, 2L)
  expect_equal(q$tail, 4L)
  q$append("z")
  expect_equal(q$data, c("a", "b", "x", "y", "z", rep(NA_character_, 95L)))
  expect_equal(q$head, 2L)
  expect_equal(q$tail, 5L)
  expect_equal(q$data[q$head], "b")
  expect_equal(q$data[q$tail], "z")
})

tar_test("sequential$prepend()", {
  q <- sequential_init(step = 100L)
  q$prepend(c("a", "b"))
  q$prepend(c("x", "y"))
  expect_equal(q$data, c("x", "y", "a", "b"))
  expect_equal(q$head, 1L)
  expect_equal(q$tail, 4L)
  expect_equal(q$dequeue(), "x")
  q$extend(n = 100L)
  q$prepend("z")
  q$prepend("c")
  expect_equal(q$data, c("c", "z", "y", "a", "b", rep(NA_character_, 100L)))
  expect_equal(q$head, 1L)
  expect_equal(q$tail, 5L)
})

tar_test("sequential$increment_ranks()", {
  q <- sequential_init(c("a", "b"))
  q$increment_ranks("a", 1L)
  expect_equal(q$data, c("a", "b"))
})

tar_test("sequential$reset()", {
  q <- sequential_init(names = letters, step = 100L)
  q$dequeue()
  expect_equal(q$data, letters)
  expect_equal(q$head, 2L)
  expect_equal(q$tail, 26L)
  expect_equal(q$data[q$head], "b")
  expect_equal(q$data[q$tail], "z")
  expect_true(q$is_nonempty())
  q$reset()
  expect_equal(q$data, character(0L))
  expect_equal(q$head, 1L)
  expect_equal(q$tail, 0L)
  expect_false(q$is_nonempty())
})
