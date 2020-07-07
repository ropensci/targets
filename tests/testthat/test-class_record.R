tar_test("record$produce_row()", {
  record <- record_init(
    name = "x_name",
    parent = "x_parent",
    type = "x_target",
    command = "x_command",
    depend = "x_depend",
    path = c("x_path", "y_path"),
    data = "x_data",
    bytes = 123,
    time = 456,
    format = "x_format",
    iteration = "x_iteration",
    children = c("x_branch", "y_branch"),
    error = c("stop", "msg")
  )
  row <- record_produce_row(record)
  expect_true(is.list(row))
  expect_equal(sort(names(row)), sort(header_meta()))
  expect_equal(row$name, "x_name")
  expect_equal(row$parent, "x_parent")
  expect_equal(row$type, "x_target")
  expect_equal(row$command, "x_command")
  expect_equal(row$depend, "x_depend")
  expect_equal(row$path, list(c("x_path", "y_path")))
  expect_equal(row$data, "x_data")
  expect_equal(row$bytes, 123)
  expect_equal(row$time, 456)
  expect_equal(row$format, "x_format")
  expect_equal(row$iteration, "x_iteration")
  expect_equal(row$children, list(c("x_branch", "y_branch")))
  expect_equal(row$error, record_encode_field(c("stop", "msg")))
})

tar_test("record_encode_field() empty", {
  expect_equal(record_encode_field(NULL), NA_character_)
})

tar_test("record_encode_field() nonempty", {
  out <- record_encode_field(c("stop", "msg"))
  expect_equal(out, "stop msg")
  expect_true(nzchar(out))
})

tar_test("record_encode_field() nonempty", {
  out <- record_encode_field(c("stop", "msg"))
  expect_equal(out, "stop msg")
  expect_true(nzchar(out))
})

tar_test("record_encode_field() with bad characters", {
  out <- record_encode_field(c("a\nb*|c\r", "b"))
  expect_equal(out, "a{NEWLINE}b*{PIPE}c{RETURN} b")
  expect_true(nzchar(out))
})

tar_test("record_validate(record) empty", {
  expect_silent(record_validate(record_init()))
})

tar_test("record_is_target()", {
  expect_true(record_is_target(record_init(type = "stem")))
  expect_true(record_is_target(record_init(type = "branch")))
  expect_true(record_is_target(record_init(type = "map")))
  expect_true(record_is_target(record_init(type = "cross")))
  expect_false(record_is_target(record_init(type = "object")))
  expect_false(record_is_target(record_init(type = "function")))
})

tar_test("record_validate() custom", {
  out <- record_init(
    name = "x_name",
    parent = "x_parent",
    type = "x_target",
    command = "x_command",
    depend = "x_depend",
    path = c("x_path", "y_path"),
    data = "x_data",
    bytes = 123,
    time = 456,
    format = "x_format",
    iteration = "x_iteration",
    children = c("x_branch", "y_branch")
  )
  expect_silent(record_validate(out))
})
