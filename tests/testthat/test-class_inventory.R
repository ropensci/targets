tar_test("inventory class is valid", {
  expect_silent(inventory_init()$validate())
})

tar_test("inventory abstract class basic methods", {
  x <- inventory_init()
  store <- store_init()
  expect_equal(x$list_cache(), character(0L))
  expect_equal(x$misses, 0L)
  expect_equal(x$downloads, 0L)
  for (index in seq_len(4L)) {
    out <- x$get_cache(store)
    expect_equal(x$misses, 1L)
    expect_equal(x$downloads, 1L)
    expect_equal(out, "example_hash")
    expect_equal(x$list_cache(), "example_bucket|example_key")
  }
  x$reset()
  expect_equal(x$list_cache(), character(0L))
})
