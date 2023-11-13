tar_test("inventory class is valid", {
  expect_silent(inventory_init()$validate())
})

tar_test("inventory abstract class basic methods", {
  x <- inventory_init()
  store <- store_init()
  expect_equal(x$list_cache(), character(0L))
  out <- x$get_cache(store)
  expect_equal(x$list_cache(), "example_bucket|example_key")
  expect_equal(out, "example_hash")
  x$reset_cache()
  expect_equal(x$list_cache(), character(0L))
})
