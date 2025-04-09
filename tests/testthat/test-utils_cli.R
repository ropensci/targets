tar_test("cli_pipeline_uptodate()", {
  skip_cran()
  expect_message(cli_pipeline_uptodate(seconds_elapsed = 1, skipped = 0))
})

tar_test("cli_pipeline_done()", {
  skip_cran()
  expect_message(
    cli_pipeline_done(
      seconds_elapsed = 1,
      completed = 2,
      skipped = 3
    )
  )
})

tar_test("cli_pipeline_empty()", {
  skip_cran()
  expect_message(cli_pipeline_empty(seconds_elapsed = 1))
})

tar_test("cli_pipeline_errored()", {
  skip_cran()
  expect_message(
    cli_pipeline_errored(seconds_elapsed = 1, completed = 2, skipped = 3)
  )
})

tar_test("cli_port()", {
  skip_cran()
  expect_message(cli_port("host", "port"))
})

tar_test("cli_short()", {
  expect_equal(cli_short("abcde", 100L), "abcde")
  expect_equal(cli_short("abcde", 5L), "abcde")
  expect_equal(cli_short("abcde", 4L), paste0("abc", cli::symbol$ellipsis))
  expect_equal(cli_short("abcde", 3L), paste0("ab", cli::symbol$ellipsis))
})
