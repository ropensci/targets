tar_test("cli_start()", {
  skip_cran()
  expect_message(cli_start("x", time_stamp = TRUE))
})

tar_test("cli_built()", {
  skip_cran()
  expect_message(cli_built("x", time_stamp = TRUE))
})

tar_test("cli_skip()", {
  skip_cran()
  expect_message(cli_skip("x", time_stamp = TRUE))
})

tar_test("cli_error()", {
  skip_cran()
  expect_message(cli_error("x", time_stamp = TRUE))
})

tar_test("cli_cancel()", {
  skip_cran()
  expect_message(cli_cancel("x", time_stamp = TRUE))
})

tar_test("cli_uptodate()", {
  skip_cran()
  expect_message(cli_uptodate(seconds_elapsed = 1))
})

tar_test("cli_done()", {
  skip_cran()
  expect_message(cli_done(seconds_elapsed = 1))
})

tar_test("cli_empty()", {
  skip_cran()
  expect_message(cli_empty(seconds_elapsed = 1))
})

tar_test("cli_workspace()", {
  skip_cran()
  expect_message(cli_workspace("x", time_stamp = TRUE))
})

tar_test("cli_blue_bullet()", {
  skip_cran()
  expect_message(cli_blue_bullet("x"))
})

tar_test("cli_blue_play()", {
  skip_cran()
  expect_message(cli_blue_play("x"))
})

tar_test("cli_green_record()", {
  skip_cran()
  expect_message(cli_green_record("x"))
})

tar_test("cli_green_check()", {
  skip_cran()
  expect_message(cli_green_check("x"))
})

tar_test("cli_yellow_box()", {
  skip_cran()
  expect_message(cli_yellow_box("x"))
})

tar_test("cli_mark_info()", {
  skip_cran()
  expect_message(cli_mark_info("x"))
})

tar_test("cli_blank()", {
  skip_cran()
  expect_message(cli_blank("x"))
})

tar_test("cli_red_x()", {
  skip_cran()
  expect_message(cli_red_x("x"))
})

tar_test("cli_red_x()", {
  skip_cran()
  expect_message(cli_red_x("x"))
})

tar_test("cli_port()", {
  skip_cran()
  expect_message(cli_port("host", "port"))
})
