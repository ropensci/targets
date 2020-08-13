tar_test("tar_delete() works", {
  pipeline <- pipeline_init(
    list(
      target_init("y1", quote(1 + 1)),
      target_init("y2", quote(1 + 1)),
      target_init("z", quote(y1 + y2))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_delete(starts_with("y")) # Only deletes y1 and y2
  files <- list.files(file.path("_targets", "objects"))
  expect_equal(files, "z")
})

tar_test("tar_delete() works with patterns", {
  pipeline <- pipeline_init(
    list(
      target_init("x", quote(seq_len(2))),
      target_init("y", quote(x), pattern = quote(map(x))),
      target_init("z", quote(y), pattern = quote(map(y)))
    )
  )
  local_init(pipeline = pipeline)$run()
  tar_delete(starts_with("y")) # Only deletes y1 and y2
  data <- meta_init()$database$read_data()
  names <- list.files(file.path("_targets", "objects"))
  expect_equal(sum(grepl("^x", names)), 1L)
  expect_equal(sum(grepl("^y", names)), 0L)
  expect_equal(sum(grepl("^z", names)), 2L)
})

tar_test("tar_delete() does not delete dynamic files", {
  file.create("x")
  pipeline <- pipeline_init(
    list(target_init("x", quote("x"), format = "file"))
  )
  local_init(pipeline = pipeline)$run()
  tar_delete(x)
  expect_true(file.exists("x"))
})
