tar_test("dir_create_runtime() with no caches", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- NULL
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  expect_false(dir.exists(tmp))
  dir_create_runtime(tmp)
  expect_true(dir.exists(tmp))
})

tar_test("dir_create_runtime()", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- counter_init()
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  expect_false(dir.exists(tmp))
  expect_false(counter_exists_name(tar_runtime$file_exist, tmp))
  dir_create_runtime(tmp)
  expect_true(dir.exists(tmp))
  expect_true(counter_exists_name(tar_runtime$file_exist, tmp))
  unlink(tmp, recursive = TRUE)
  expect_false(dir.exists(tmp))
  expect_true(counter_exists_name(tar_runtime$file_exist, tmp))
  dir_create_runtime(tmp)
  expect_false(dir.exists(tmp))
  expect_true(counter_exists_name(tar_runtime$file_exist, tmp))
})

tar_test("file_exists_runtime() with no caches", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tar_runtime$file_exist <- NULL
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_equal(file_exists_runtime(c(tmp, "nope")), c(TRUE, FALSE))
})

tar_test("file_exists_runtime() all files registered", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  tmp3 <- tempfile()
  tar_runtime$file_exist <- counter_init(names = c(tmp1, tmp2, tmp3))
  expect_equal(file_exists_runtime(c(tmp1, tmp2, tmp3)), c(TRUE, TRUE, TRUE))
})

tar_test("file_exists_runtime() with some files registered", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  tmp3 <- tempfile()
  file.create(tmp1)
  on.exit(unlink(tmp1), add = TRUE)
  tar_runtime$file_exist <- counter_init(names = tmp2)
  expect_equal(file_exists_runtime(c(tmp1, tmp2, tmp3)), c(TRUE, TRUE, FALSE))
})

tar_test("file_exists_runtime() with no files registered", {
  old <- tar_runtime$file_exist
  on.exit(tar_runtime$file_exist <- old)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  tmp3 <- tempfile()
  file.create(tmp2)
  on.exit(unlink(tmp2), add = TRUE)
  tar_runtime$file_exist <- counter_init(names = character(0L))
  expect_equal(
    file_exists_runtime(c(tmp1, tmp2, tmp3)),
    c(FALSE, TRUE, FALSE)
  )
})

tar_test("file_info_runtime() with no files", {
  out <- file_info_runtime(character(0L))
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 0L)
})

tar_test("file_info_runtime() with no caches", {
  old_info <- tar_runtime$file_info
  on.exit(tar_runtime$file_info <- old_info)
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  files <- c(tmp1, tmp2)
  file.create(tmp1)
  file.create(tmp2)
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
  tar_runtime$file_info <- NULL
  out <- file_info_runtime(files)
  expect_true(is.list(out))
  fields <- c(
    "path",
    "size",
    "mtime_numeric",
    "trust_timestamps",
    "hit"
  )
  for (field in fields) {
    expect_equal(length(out[[field]]), 2L)
  }
  expect_false(any(out$hit))
})

tar_test("file_info_runtime() with all files registered", {
  old_info <- tar_runtime$file_info
  on.exit(tar_runtime$file_info <- old_info)
  store <- tempfile()
  on.exit(unlink(store, recursive = TRUE), add = TRUE)
  dir.create(path_objects_dir(store), recursive = TRUE)
  file.create(path_objects(store, "a"))
  file.create(path_objects(store, "b"))
  runtime_set_file_info(tar_runtime, store, c("a", "b"))
  out <- file_info_runtime(path_objects(store, "a"))
  expect_true(is.list(out))
  fields <- c(
    "path",
    "size",
    "mtime_numeric",
    "trust_timestamps",
    "hit"
  )
  for (field in fields) {
    expect_equal(length(out[[field]]), 1L)
  }
  expect_true(all(out$hit))
})

tar_test("file_info_runtime() with not all files registered", {
  old_info <- tar_runtime$file_info
  on.exit(tar_runtime$file_info <- old_info)
  store <- tempfile()
  on.exit(unlink(store, recursive = TRUE), add = TRUE)
  dir.create(path_objects_dir(store), recursive = TRUE)
  file.create(path_objects(store, "a"))
  runtime_set_file_info(tar_runtime, store, c("a"))
  file.create(path_objects(store, "b"))
  out_a <- file_info_runtime(path_objects(store, "a"))
  out_b <- file_info_runtime(path_objects(store, "b"))
  expect_true(out_a$hit)
  expect_false(out_b$hit)
})

tar_test("file_info_runtime() with no files registered", {
  old_info <- tar_runtime$file_info
  on.exit(tar_runtime$file_info <- old_info)
  store <- tempfile()
  on.exit(unlink(store, recursive = TRUE), add = TRUE)
  dir.create(path_objects_dir(store), recursive = TRUE)
  runtime_set_file_info(tar_runtime, store, character(0L))
  file.create(path_objects(store, "a"))
  file.create(path_objects(store, "b"))
  out_a <- file_info_runtime(path_objects(store, "a"))
  out_b <- file_info_runtime(path_objects(store, "b"))
  expect_false(out_a$hit)
  expect_false(out_b$hit)
})

tar_test("file_move() on files", {
  file.create("x")
  dir.create("z")
  file_move(from = "x", to = file.path("z", "y"))
  expect_true(file.exists(file.path("z", "y")))
  expect_false(file.exists("x"))
  writeLines("contents", "x")
  file_move(from = "x", to = file.path("z", "y"))
  expect_true(file.exists(file.path("z", "y")))
  expect_false(file.exists("x"))
  expect_equal(readLines(file.path("z", "y")), "contents")
})

tar_test("file_move() to empty directories", {
  dir.create("x")
  file.create(file.path("x", "x"))
  file_move(from = "x", to = file.path("z", "z2"))
  expect_true(file.exists(file.path("z", "z2", "x")))
  expect_false(file.exists("x"))
})

tar_test("file_move() to existing directories", {
  dir.create("x")
  file.create(file.path("x", "x"))
  dir.create(file.path("z", "z2"), recursive = TRUE)
  file.create(file.path("z", "z2", "z3"))
  file_move(from = "x", to = file.path("z", "z2"))
  expect_true(file.exists(file.path("z", "z2", "x")))
  expect_false(file.exists("x"))
})

tar_test("file_copy() on files", {
  file.create("x")
  dir.create("z")
  file_copy(from = "x", to = file.path("z", "y"))
  expect_true(file.exists(file.path("z", "y")))
  expect_true(file.exists("x"))
  writeLines("contents", "x")
  file_copy(from = "x", to = file.path("z", "y"))
  expect_true(file.exists(file.path("z", "y")))
  expect_true(file.exists("x"))
  expect_equal(readLines(file.path("z", "y")), "contents")
})

tar_test("file_copy() to empty directories", {
  dir.create("x")
  file.create(file.path("x", "x"))
  file_copy(from = "x", to = file.path("z", "z2"))
  expect_true(file.exists(file.path("z", "z2", "x")))
  expect_true(file.exists("x"))
})

tar_test("file_copy() to existing directories", {
  dir.create("x")
  file.create(file.path("x", "x"))
  dir.create(file.path("z", "z2"), recursive = TRUE)
  file.create(file.path("z", "z2", "z3"))
  file_copy(from = "x", to = file.path("z", "z2"))
  expect_true(file.exists(file.path("z", "z2", "x")))
  expect_true(file.exists("x"))
})
