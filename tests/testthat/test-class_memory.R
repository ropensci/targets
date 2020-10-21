tar_test("memory$envir", {
  envir <- new.env(parent = emptyenv())
  envir$a <- "123"
  out <- memory_init(envir = envir)
  expect_equal(out$envir$a, "123")
})

tar_test("memory$names", {
  envir <- new.env(parent = emptyenv())
  envir$a <- "123"
  out <- memory_init(envir = envir)
  expect_equal(out$names, "a")
})

tar_test("memory$count", {
  envir <- new.env(parent = emptyenv())
  envir$a <- "123"
  out <- memory_init(envir = envir)
  expect_equal(out$count, 1L)
})

tar_test("memory_get_object()", {
  envir <- new.env(parent = emptyenv())
  envir$a <- "123"
  out <- memory_init(envir = envir)
  expect_equal(memory_get_object(out, "a"), "123")
})

tar_test("memory_set_object()", {
  envir <- new.env(parent = emptyenv())
  envir$a <- "123"
  out <- memory_init(envir = envir)
  expect_equal(out$names, "a")
  expect_equal(out$count, 1L)
  memory_set_object(out, "b", "456")
  expect_equal(out$envir$b, "456")
  expect_equal(sort(out$names), sort(c("a", "b")))
  expect_equal(out$count, 2L)
})

tar_test("memory_exists_object()", {
  out <- memory_init()
  expect_false(memory_exists_object(out, "a"))
  memory_set_object(out, "a", "a")
  expect_true(memory_exists_object(out, "a"))
})

tar_test("memory_del_objects()", {
  out <- memory_init()
  memory_set_object(out, "a", "123")
  memory_set_object(out, "b", "456")
  memory_set_object(out, "c", "789")
  expect_equal(out$envir$a, "123")
  expect_equal(out$envir$b, "456")
  expect_equal(out$envir$c, "789")
  expect_equal(sort(out$names), sort(c("a", "b", "c")))
  expect_equal(out$count, 3L)
  memory_del_objects(out, c("a", "c"))
  expect_equal(out$envir$a, NULL)
  expect_equal(out$envir$b, "456")
  expect_equal(out$envir$c, NULL)
  expect_equal(out$names, "b")
  expect_equal(out$count, 1L)
})

tar_test("memory_validate() on a good memory object", {
  out <- memory_init()
  memory_set_object(out, "a", "123")
  memory_set_object(out, "b", "456")
  memory_set_object(out, "c", "789")
  expect_silent(memory_validate(out))
})

tar_test("memory_validate() on a memory object with no environment", {
  out <- memory_new(names = character(0))
  expect_error(memory_validate(out), class = "condition_validate")
})

tar_test("memory_validate() on a memory object with no names", {
  out <- memory_new(envir = new.env())
  expect_error(memory_validate(out), class = "condition_validate")
})

tar_test("memory_validate() with an extra field", {
  out <- memory_new(envir = new.env())
  out$bad <- 123
  expect_error(memory_validate(out), class = "condition_validate")
})

tar_test("memory_validate() with incorrect names", {
  out <- memory_init()
  memory_set_object(out, "a", "123")
  memory_set_object(out, "b", "456")
  memory_set_object(out, "c", "789")
  out$names <- c("a", "b")
  expect_error(memory_validate(out), class = "condition_validate")
})
