tar_test("settings$pattern", {
  x <- settings_init(pattern = NULL)
  expect_null(x$pattern)
  x <- settings_init(name = "x", pattern = quote(map(data, models)))
  expect_equal(x$pattern, expression(map(data, models)))
})

tar_test("settings$dimensions", {
  x <- settings_init(pattern = NULL)
  expect_equal(x$dimensions, character(0))
  x <- settings_init(pattern = quote(map(data, models)))
  expect_equal(sort(x$dimensions), sort(c("data", "models")))
})
tar_test("settings_validate()", {
  x <- settings_init(name = "abc")
  expect_silent(settings_validate(x))
})

tar_test("settings_validate() with resources", {
  x <- settings_init(name = "abc", resources = list(ncores = 2))
  expect_silent(settings_validate(x))
})

tar_test("settings_validate() with a bad name", {
  x <- settings_init(name = "")
  expect_error(settings_validate(x), class = "condition_validate")
  x <- settings_init(name = 123)
  expect_error(settings_validate(x), class = "condition_validate")
  x <- settings_init(name = "_abc")
  expect_error(settings_validate(x), class = "condition_validate")
  x <- settings_init(name = "abc.")
  expect_error(settings_validate(x), class = "condition_validate")
})

tar_test("settings_validate() with invalid pattern", {
  expect_error(
    settings_init(
      name = "abc",
      format = "rds",
      pattern = quote(cartwheel(a, b))
    ),
    class = "condition_validate"
  )
  expect_error(
    settings_init(
      name = "abc",
      format = "rds",
      pattern = quote(map())
    ),
    class = "condition_validate"
  )
})

tar_test("settings_validate() with bad field", {
  x <- settings_init(name = "abc")
  x$bad <- 123
  expect_error(settings_validate(x), class = "condition_validate")
})

tar_test("settings_validate() with bad memory", {
  x <- settings_init(name = "abc", memory = "fancy")
  expect_error(settings_validate(x), class = "condition_validate")
})

tar_test("settings_validate() with bad storage", {
  x <- settings_init(name = "abc", storage = "fancy")
  expect_error(settings_validate(x), class = "condition_validate")
})

tar_test("settings_validate() with bad retrieval", {
  x <- settings_init(name = "abc", retrieval = "fancy")
  expect_error(settings_validate(x), class = "condition_validate")
})
