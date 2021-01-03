tar_test("tar_sitrep() on an empty project", {
  tar_script(
    list(
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
  expect_equiv(out, exp)
})

tar_test("tar_sitrep() on an empty project with callr process", {
  tar_script(
    list(
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
  expect_equiv(out, exp)
})

tar_test("tar_sitrep() name selection", {
  tar_script(
    list(
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
  expect_equiv(out, exp)
})

tar_test("tar_sitrep() name selection in reverse", {
  tar_script(
    list(
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
  expect_equiv(out, exp)
})

tar_test("tar_sitrep() field selection", {
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x)),
      tar_target(z, 2 * y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  )
  out <- tar_sitrep(callr_function = NULL, fields = contains("always"))
  out <- out[order(out$name), ]
  exp <- tibble::tibble(name = c("w", "x"), always = FALSE)
  expect_equiv(out, exp)
})

tar_test("tar_sitrep() on a run project", {
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x)),
      tar_target(z, 2 * y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  )
  tar_make(callr_function = NULL)
  out <- tar_sitrep(callr_function = NULL)
  out <- out[order(out$name), ]
  children_y <- tar_meta(names = "y")$children[[1]]
  children_z <- tar_meta(names = "z")$children[[1]]
  exp <- tibble::tibble(
    name = sort(c("w", "x", children_y, children_z)),
    record = FALSE,
    always = FALSE,
    never = FALSE,
    command = FALSE,
    depend = FALSE,
    format = FALSE,
    iteration = FALSE,
    file = FALSE
  )
  expect_equiv(out, exp)
})

tar_test("tar_sitrep() on a project with a change", {
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x)),
      tar_target(z, 2 * y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  )
  tar_make(callr_function = NULL)
  children_y <- tar_meta(names = "y")$children[[1]]
  children_z <- tar_meta(names = "z")$children[[1]]
  unlink(path_objects(children_y[1]))
  out <- tar_sitrep(callr_function = NULL)
  out <- out[order(out$name), ]
  exp <- tibble::tibble(
    name = sort(c("w", "x", children_y, children_z)),
    record = FALSE,
    always = FALSE,
    never = FALSE,
    command = FALSE,
    depend = FALSE,
    format = FALSE,
    iteration = FALSE,
    file = FALSE
  )
  exp$file[exp$name == children_y[1]] <- TRUE
  expect_equiv(out, exp)
})

tar_test("tar_sitrep() invalidation due to aggregated pattern deps", {
  tar_script(
    list(
      tar_target(x, seq_len(2)),
      tar_target(y, 2 * x, pattern = map(x)),
      tar_target(z, 2 * y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  )
  tar_make(callr_function = NULL)
  tar_script(
    list(
      tar_target(x, c(1L, 3L)),
      tar_target(y, 2 * x, pattern = map(x)),
      tar_target(z, 2 * y, pattern = map(y)),
      tar_target(w, sum(y))
    )
  )
  tar_make(callr_function = NULL, names = c("x", "y", "z"))
  children_y <- tar_meta(names = "y")$children[[1]]
  children_z <- tar_meta(names = "z")$children[[1]]
  out <- tar_sitrep(callr_function = NULL)
  out <- out[order(out$name), ]
  exp <- tibble::tibble(
    name = sort(c("w", "x", children_y, children_z)),
    record = FALSE,
    always = FALSE,
    never = FALSE,
    command = FALSE,
    depend = FALSE,
    format = FALSE,
    iteration = FALSE,
    file = FALSE
  )
  exp$depend[exp$name == "w"] <- TRUE
  expect_equiv(out, exp)
})
