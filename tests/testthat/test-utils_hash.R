tar_test("hash_object()", {
  for (object in list("x", 0L)) {
    out <- hash_object(object)
    expect_true(is.character(out))
    expect_true(length(out) == 1L)
    expect_false(anyNA(out))
    expect_true(nzchar(out))
  }
})

tar_test("hash_file()", {
  for (object in list("x", 0L)) {
    file <- tempfile()
    saveRDS(object, file)
    out <- hash_file(file)
    expect_true(is.character(out))
    expect_true(length(out) == 1L)
    expect_false(anyNA(out))
    expect_true(nzchar(out))
    expect_false(out == hash_object(file))
    unlink(file)
  }
})
