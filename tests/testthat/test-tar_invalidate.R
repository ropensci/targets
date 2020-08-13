tar_test("tar_invalidate() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1 + 1)),
      target_init("y2", quote(1 + 1)),
      target_init("z", quote(y1 + y2))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_invalidate(starts_with("y")) # Only invalidates y1 and y2
  data <- meta_init()$database$read_data()
  expect_true("z" %in% data$name)
  expect_false(any(c("y1", "y2") %in% data$name))
})


tar_test("tar_invalidate() works with patterns", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(x), pattern = quote(map(x))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_invalidate(starts_with("y")) # Only invalidates y1 and y2
  data <- meta_init()$database$read_data()
  names <- data$name
  expect_equal(sum(grepl("^x", names)), 1L)
  expect_equal(sum(grepl("^y", names)), 0L)
  expect_equal(sum(grepl("^z", names)), 3L)
})
