tar_test("tar_assert_package()", {
  expect_error(tar_assert_package("_illegal"), class = "tar_condition_targets")
})

tar_test("tar_assert_chr_no_delim()", {
  expect_error(tar_assert_chr_no_delim(0L), class = "tar_condition_validate")
  expect_error(
    tar_assert_chr_no_delim("a|b"),
    class = "tar_condition_validate"
  )
  expect_error(
    tar_assert_chr_no_delim("a*b"),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_target_dag() on a non-igraph", {
  expect_error(tar_assert_target_dag(123), class = "tar_condition_validate")
})

tar_test("tar_assert_target_dag() on a non-dag", {
  edges <- data_frame(from = c("a", "b"), to = c("b", "a"))
  igraph <- igraph::graph_from_data_frame(edges)
  expect_error(tar_assert_target_dag(igraph), class = "tar_condition_validate")
})

tar_test("tar_assert_target_dag() on a dag", {
  edges <- data_frame(from = c("a", "b"), to = c("b", "c"))
  igraph <- igraph::graph_from_data_frame(edges)
  expect_silent(tar_assert_target_dag(igraph))
})

tar_test("tar_assert_ge()", {
  expect_silent(tar_assert_ge(2L, 1L))
  expect_silent(tar_assert_ge(2L, 2L))
  expect_error(tar_assert_ge(1L, 2L), class = "tar_condition_validate")
})

tar_test("tar_assert_flag()", {
  expect_silent(tar_assert_flag("x", letters))
  expect_error(
    tar_assert_flag("xyz", letters),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_function()", {
  expect_error(tar_assert_function("not"), class = "tar_condition_validate")
})

tar_test("tar_assert_df()", {
  expect_silent(tar_assert_df(data_frame(x = 1)))
  expect_error(tar_assert_df(TRUE), class = "tar_condition_validate")
})

tar_test("tar_assert_in()", {
  expect_silent(tar_assert_in("x", letters))
  expect_error(tar_assert_in("xyz", letters), class = "tar_condition_validate")
})

tar_test("tar_assert_equal_lengths()", {
  expect_silent(tar_assert_equal_lengths(as.list(letters)))
  expect_error(
    tar_assert_equal_lengths(list(a = 1, b = letters)),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_not_in()", {
  expect_silent(tar_assert_not_in("xyz", letters))
  expect_error(
    tar_assert_not_in("x", letters),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_inherits()", {
  expect_silent(tar_assert_inherits(data_frame(x = 1), "data.frame"))
  expect_error(
    tar_assert_inherits(TRUE, "data.frame"),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_lang()", {
  expect_silent(tar_assert_lang(quote(x + 1)))
  expect_error(
    tar_assert_lang(TRUE),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_le()", {
  expect_silent(tar_assert_le(1L, 2L))
  expect_silent(tar_assert_le(2L, 2L))
  expect_error(tar_assert_le(2L, 1L), class = "tar_condition_validate")
})

tar_test("tar_assert_lgl()", {
  expect_silent(tar_assert_lgl(FALSE))
  expect_error(tar_assert_lgl("abc"), class = "tar_condition_validate")
})

tar_test("tar_assert_list()", {
  expect_silent(tar_assert_list(list("abc")))
  expect_error(tar_assert_list("abc"), class = "tar_condition_validate")
})

tar_test("tar_assert_name()", {
  expect_silent(tar_assert_name("abc"))
  expect_error(tar_assert_name("1 2"), class = "tar_condition_validate")
  expect_error(tar_assert_name(".a"), class = "tar_condition_validate")
  expect_error(tar_assert_name("a."), class = "tar_condition_validate")
})

tar_test("tar_assert_names()", {
  expect_silent(tar_assert_names("abc"))
  expect_error(tar_assert_names("1 2"), class = "tar_condition_validate")
})

tar_test("tar_assert_nonempty()", {
  expect_silent(tar_assert_nonempty("abc"))
  expect_silent(tar_assert_nonempty(NA_character_))
  expect_error(
    tar_assert_nonempty(NULL),
    class = "tar_condition_validate"
  )
  expect_error(
    tar_assert_nonempty(character(0)),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_none_na()", {
  expect_silent(tar_assert_none_na("abc"))
  expect_error(
    tar_assert_none_na(NA_character_),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_nonmissing()", {
  expect_silent(tar_assert_nonmissing("abc"))
  expect_error(
    tar_assert_nonmissing(substitute()),
    class = "tar_condition_validate"
  )
})

tar_test("assert_not_dirs()", {
  tmp <- tempfile()
  expect_silent(tar_assert_not_dirs(tmp))
  dir.create(tmp)
  expect_error(
    tar_assert_not_dirs(tmp),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_not_expr()", {
  expect_silent(tar_assert_not_expr(123))
  expect_error(
    tar_assert_not_expr(expression(x + 1)),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_nzchar()", {
  expect_silent(tar_assert_nzchar("abc"))
  expect_error(tar_assert_nzchar(c("a", "")), class = "tar_condition_validate")
})

tar_test("tar_assert_path()", {
  file.create("x")
  expect_error(tar_assert_path(c("x", "y")), class = "tar_condition_validate")
  file.create("y")
  expect_silent(tar_assert_path(c("x", "y")))
})

tar_test("tar_assert_file()", {
  expect_error(tar_assert_file(0), class = "tar_condition_validate")
  expect_error(tar_assert_file("x"), class = "tar_condition_validate")
  file.create("x")
  expect_error(tar_assert_file(c("x", "y")), class = "tar_condition_validate")
  expect_silent(tar_assert_file("x"))
})

tar_test("tar_assert_resources()", {
  expect_warning(
    tar_assert_resources(list(a = 1)),
    class = "tar_condition_deprecate"
  )
  expect_warning(
    tar_assert_resources(list(aws = 1)),
    class = "tar_condition_deprecate"
  )
  aws <- tar_resources_aws(bucket = "bucket")
  expect_silent(tar_assert_resources(tar_resources(aws = aws)))
})

tar_test("tar_assert_store()", {
  expect_error(tar_assert_store("_targets"), class = "tar_condition_validate")
  dir.create("_targets")
  expect_silent(tar_assert_store("_targets"))
})

tar_test("tar_assert_target", {
  expect_silent(tar_assert_target(tar_target(x, 1)))
  expect_error(tar_assert_target(1), class = "tar_condition_validate")
  expect_error(tar_assert_target(list()), class = "tar_condition_validate")
})

tar_test("tar_assert_target_list", {
  expect_silent(tar_assert_target_list(list(tar_target(x, 1))))
  expect_silent(tar_assert_target_list(list()))
  expect_error(
    tar_assert_target_list(tar_target(x, 1)),
    class = "tar_condition_validate"
  )
  expect_error(tar_assert_target_list(123), class = "tar_condition_validate")
})

tar_test("tar_assert_script()", {
  old <- Sys.getenv("TAR_WARN")
  on.exit(Sys.setenv(TAR_WARN = old))
  Sys.setenv(TAR_WARN = "true")
  expect_error(
    tar_assert_script("_targets.R"),
    class = "tar_condition_validate"
  )
  file.create("_targets.R")
  expect_silent(tar_assert_script("_targets.R"))
  writeLines("tar_make()", "_targets.R")
  expect_error(
    tar_assert_script("_targets.R"),
    class = "tar_condition_validate"
  )
  tar_script({
    library(targets)
    pkgload::load_all()
    list(tar_target(x, 1))
  })
  expect_warning(
    tar_assert_script("_targets.R"),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_true()", {
  expect_silent(tar_assert_true(TRUE, "x"))
  expect_error(tar_assert_true(FALSE, "x"), class = "tar_condition_validate")
})

tar_test("tar_assert_unique_targets()", {
  expect_silent(tar_assert_unique_targets(letters))
  expect_error(
    tar_assert_unique_targets(c("a", "a", "b", "b")),
    class = "tar_condition_validate"
  )
})
