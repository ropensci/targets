test_that("tar_debug_instructions()", {
  on.exit(tar_runtime$unset_target())
  for (target in list(tar_target(a, g(a)), tar_target(a, NULL))) {
    tar_runtime$set_target(target)
    target$command$expr <- as.expression(
      list(
        instructions = quote(targets::tar_debug_instructions()),
        browser = quote(browser()),
        expr = target$command$expr
      )
    )
    suppressMessages(expect_message(tar_debug_instructions()))
  }
})
