tar_test("tar_assert_package()", {
  skip_cran()
  expect_error(tar_assert_package("_illegal"), class = "tar_condition_targets")
})

tar_test("tar_assert_chr_no_delim()", {
  skip_cran()
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
  skip_cran()
  expect_error(tar_assert_target_dag(123), class = "tar_condition_validate")
})

tar_test("tar_assert_target_dag() on a non-dag", {
  skip_cran()
  edges <- data_frame(from = c("a", "b"), to = c("b", "a"))
  igraph <- igraph::graph_from_data_frame(edges)
  expect_error(tar_assert_target_dag(igraph), class = "tar_condition_validate")
})

tar_test("tar_assert_target_dag() on a dag", {
  skip_cran()
  edges <- data_frame(from = c("a", "b"), to = c("b", "c"))
  igraph <- igraph::graph_from_data_frame(edges)
  expect_silent(tar_assert_target_dag(igraph))
})

tar_test("tar_assert_ge()", {
  skip_cran()
  expect_silent(tar_assert_ge(2L, 1L))
  expect_silent(tar_assert_ge(2L, 2L))
  expect_error(tar_assert_ge(1L, 2L), class = "tar_condition_validate")
})

tar_test("tar_assert_flag()", {
  skip_cran()
  expect_silent(tar_assert_flag("x", letters))
  expect_error(
    tar_assert_flag("xyz", letters),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_function()", {
  skip_cran()
  expect_silent(tar_assert_function(base::identity))
  expect_error(tar_assert_function("not"), class = "tar_condition_validate")
})

tar_test("tar_assert_function_arguments()", {
  skip_cran()
  f <- function(x, y) {
    x
  }
  expect_silent(tar_assert_function_arguments(f, c("x", "y")))
  expect_error(
    tar_assert_function_arguments(f, c("y", "x")),
    class = "tar_condition_validate"
  )
  expect_error(
    tar_assert_function_arguments(f, c("x", "a", "b")),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_df()", {
  skip_cran()
  expect_silent(tar_assert_df(data_frame(x = 1)))
  expect_error(tar_assert_df(TRUE), class = "tar_condition_validate")
})

tar_test("tar_assert_in()", {
  skip_cran()
  expect_silent(tar_assert_in("x", letters))
  expect_error(tar_assert_in("xyz", letters), class = "tar_condition_validate")
})

tar_test("tar_assert_equal_lengths()", {
  skip_cran()
  expect_silent(tar_assert_equal_lengths(as.list(letters)))
  expect_error(
    tar_assert_equal_lengths(list(a = 1, b = letters)),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_not_in()", {
  skip_cran()
  expect_silent(tar_assert_not_in("xyz", letters))
  expect_error(
    tar_assert_not_in("x", letters),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_inherits()", {
  skip_cran()
  expect_silent(tar_assert_inherits(data_frame(x = 1), "data.frame"))
  expect_error(
    tar_assert_inherits(TRUE, "data.frame"),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_lang()", {
  skip_cran()
  expect_silent(tar_assert_lang(quote(x + 1)))
  expect_error(
    tar_assert_lang(TRUE),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_le()", {
  skip_cran()
  expect_silent(tar_assert_le(1L, 2L))
  expect_silent(tar_assert_le(2L, 2L))
  expect_error(tar_assert_le(2L, 1L), class = "tar_condition_validate")
})

tar_test("tar_assert_lgl()", {
  skip_cran()
  expect_silent(tar_assert_lgl(FALSE))
  expect_error(tar_assert_lgl("abc"), class = "tar_condition_validate")
})

tar_test("tar_assert_list()", {
  skip_cran()
  expect_silent(tar_assert_list(list("abc")))
  expect_error(tar_assert_list("abc"), class = "tar_condition_validate")
})

tar_test("tar_assert_name()", {
  skip_cran()
  expect_silent(tar_assert_name("abc"))
  expect_error(tar_assert_name("1 2"), class = "tar_condition_validate")
  expect_error(tar_assert_name(".a"), class = "tar_condition_validate")
  expect_error(tar_assert_name("a."), class = "tar_condition_validate")
})

tar_test("tar_assert_names()", {
  skip_cran()
  expect_silent(tar_assert_names("abc"))
  expect_error(tar_assert_names("1 2"), class = "tar_condition_validate")
})

tar_test("tar_assert_nonempty()", {
  skip_cran()
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

tar_test("tar_assert_all_na()", {
  skip_cran()
  expect_silent(tar_assert_all_na(NA_character_))
  expect_error(
    tar_assert_all_na("abc"),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_none_na()", {
  skip_cran()
  expect_silent(tar_assert_none_na("abc"))
  expect_error(
    tar_assert_none_na(NA_character_),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_nonmissing()", {
  skip_cran()
  expect_silent(tar_assert_nonmissing("abc"))
  expect_error(
    tar_assert_nonmissing(substitute()),
    class = "tar_condition_validate"
  )
})

tar_test("assert_not_dirs()", {
  skip_cran()
  tmp <- tempfile()
  expect_silent(tar_assert_not_dirs(tmp))
  dir.create(tmp)
  expect_error(
    tar_assert_not_dirs(tmp),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_not_expr()", {
  skip_cran()
  expect_silent(tar_assert_not_expr(123))
  expect_error(
    tar_assert_not_expr(expression(x + 1)),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_nzchar()", {
  skip_cran()
  expect_silent(tar_assert_nzchar("abc"))
  expect_error(tar_assert_nzchar(c("a", "")), class = "tar_condition_validate")
})

tar_test("tar_assert_path()", {
  skip_cran()
  file.create("x")
  expect_error(tar_assert_path(c("x", "y")), class = "tar_condition_validate")
  file.create("y")
  expect_silent(tar_assert_path(c("x", "y")))
})

tar_test("tar_assert_file()", {
  skip_cran()
  expect_error(tar_assert_file(0), class = "tar_condition_validate")
  expect_error(tar_assert_file("x"), class = "tar_condition_validate")
  file.create("x")
  expect_error(tar_assert_file(c("x", "y")), class = "tar_condition_validate")
  expect_silent(tar_assert_file("x"))
})

tar_test("tar_assert_resources()", {
  skip_cran()
  expect_warning(
    tar_assert_resources(list(a = 1)),
    class = "tar_condition_deprecate"
  )
  skip_if_not_installed("paws.storage")
  expect_warning(
    tar_assert_resources(list(aws = 1)),
    class = "tar_condition_deprecate"
  )
  aws <- tar_resources_aws(bucket = "bucket")
  expect_silent(tar_assert_resources(tar_resources(aws = aws)))
})

tar_test("tar_assert_store()", {
  skip_cran()
  expect_error(tar_assert_store("_targets"), class = "tar_condition_validate")
  dir.create("_targets")
  expect_silent(tar_assert_store("_targets"))
})

tar_test("tar_assert_target", {
  skip_cran()
  expect_silent(tar_assert_target(tar_target(x, 1)))
  expect_error(tar_assert_target(1), class = "tar_condition_validate")
  expect_error(tar_assert_target(list()), class = "tar_condition_validate")
})

tar_test("tar_assert_target_list", {
  skip_cran()
  expect_silent(tar_assert_target_list(list(tar_target(x, 1))))
  expect_silent(tar_assert_target_list(list()))
  expect_error(
    tar_assert_target_list(tar_target(x, 1)),
    class = "tar_condition_validate"
  )
  expect_error(tar_assert_target_list(123), class = "tar_condition_validate")
})

tar_test("tar_assert_script()", {
  skip_cran()
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
  skip_cran()
  expect_silent(tar_assert_true(TRUE, "x"))
  expect_error(tar_assert_true(FALSE, "x"), class = "tar_condition_validate")
})

tar_test("tar_assert_unique_targets()", {
  skip_cran()
  expect_silent(tar_assert_unique_targets(letters))
  expect_error(
    tar_assert_unique_targets(c("a", "a", "b", "b")),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_finite()", {
  skip_cran()
  expect_silent(tar_assert_finite(1))
  expect_silent(tar_assert_finite(c(1, 2)))
  expect_error(tar_assert_finite(c(1, Inf)), class = "tar_condition_validate")
})

tar_test("tar_assert_format()", {
  skip_cran()
  expect_silent(tar_assert_format("rds"))
  expect_error(tar_assert_format("invalid"), class = "tar_condition_validate")
})

tar_test("tar_assert_repository()", {
  skip_cran()
  expect_silent(tar_assert_repository("local"))
  expect_error(
    tar_assert_repository("invalid"),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_named()", {
  skip_cran()
  expect_silent(tar_assert_named(NULL))
  expect_silent(tar_assert_named(list()))
  expect_silent(tar_assert_named(list(a = 1, b = 2)))
  expect_error(
    tar_assert_named(list(a = 1, a = 2)),
    class = "tar_condition_validate"
  )
  expect_error(
    tar_assert_named(list(a = 1, 2)),
    class = "tar_condition_validate"
  )
  expect_error(
    tar_assert_named(list(1, 2)),
    class = "tar_condition_validate"
  )
})

tar_test("tar_assert_internet()", {
  skip_cran()
  expect_silent(
    tryCatch(
      tar_assert_internet(),
      tar_condition_run = function(condition) NULL
    )
  )
})

tar_test("tar_assert_objects_files()", {
  skip_cran()
  tar_script(tar_target(x, 1))
  tar_make(callr_function = NULL)
  expect_silent(tar_assert_objects_files(path_store_default()))
  path <- file.path(path_objects_dir(path_store_default()), "y")
  dir.create(path)
  expect_error(
    tar_assert_objects_files(path_store_default()),
    class = "tar_condition_run"
  )
})
