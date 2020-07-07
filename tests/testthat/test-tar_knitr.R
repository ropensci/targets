tar_test("tar_knitr() on a bad report", {
  writeLines(c("```{r}", "1 <-", "```"), "report.Rmd")
  expect_error(tar_knitr("report.Rmd"), class = "condition_validate")
})

tar_test("tar_knitr()", {
  path <- system.file("example_knitr_report.Rmd", package = "targets")
  file.copy(path, "report.Rmd")
  expr <- tar_knitr(path)
  out <- eval(expr, envir = list(analysis = 1, data = 1, data2 = 1))
  expect_equal(out, path)
  envir <- environment()
  x <- tar_target(x, !!tar_knitr("report.Rmd"))
  out <- x$command$deps
  exp <- c("analysis", "data", "data2")
  expect_true(all(exp %in% out))
  x <- tar_target(x, !!tar_knitr(path), tidy_eval = FALSE)
  out <- x$command$deps
  expect_false(any(exp %in% out))
})

tar_test("knitr_deps() finds function dependencies", {
  f <- function() tar_load(x)
  expect_equal(knitr_deps(f), "x")
})
