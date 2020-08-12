test_that("tar_sitrep() on an empty project", {
  tar_script(
    tar_pipeline(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x)),
      tar_target(z, 2 * y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  )
  out <- tar_sitrep(callr_function = NULL)
  out <- out[order(out$name), ]
  exp <- tibble::tibble(
    name = c("w", "x"),
    record = TRUE,
    always = FALSE,
    never = FALSE,
    command = NA,
    depend = NA,
    format = NA,
    iteration = NA,
    file = NA
  )
  expect_equivalent(out, exp)
})

test_that("tar_sitrep() on an empty project with callr process", {
  tar_script(
    tar_pipeline(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x)),
      tar_target(z, 2 * y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  )
  out <- tar_sitrep()
  out <- out[order(out$name), ]
  exp <- tibble::tibble(
    name = c("w", "x"),
    record = TRUE,
    always = FALSE,
    never = FALSE,
    command = NA,
    depend = NA,
    format = NA,
    iteration = NA,
    file = NA
  )
  expect_equivalent(out, exp)
})

test_that("tar_sitrep() name selection", {
  tar_script(
    tar_pipeline(
      tar_target(x2, seq_len(2)),
      tar_target(x1, seq_len(2)),
      tar_target(w, sum(y))
    )
  )
  out <- tar_sitrep(
    callr_function = NULL,
    fields = "record",
    names = starts_with("x")
  )
  out <- out[order(out$name), ]
  exp <- tibble::tibble(name = c("x1", "x2"), record = TRUE)
  exp <- exp[order(exp$name), ]
  expect_equivalent(out, exp)
})

test_that("tar_sitrep() name selection in reverse", {
  tar_script(
    tar_pipeline(
      tar_target(x2, seq_len(2)),
      tar_target(x1, seq_len(2)),
      tar_target(w, sum(y))
    )
  )
  out <- tar_sitrep(
    callr_function = NULL,
    fields = "record",
    names = c("x2", "x1")
  )
  exp <- tibble::tibble(name = c("x2", "x1"), record = TRUE)
  expect_equivalent(out, exp)
})

test_that("tar_sitrep() field selection", {
  tar_script(
    tar_pipeline(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x)),
      tar_target(z, 2 * y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  )
  out <- tar_sitrep(callr_function = NULL, fields = contains("always"))
  out <- out[order(out$name), ]
  exp <- tibble::tibble(name = c("w", "x"), always = FALSE)
  expect_equivalent(out, exp)
})
