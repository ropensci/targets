tar_test("tar_unversion()", {
  skip_cran()
  lines <- c(
    paste0(
      "name|type|data|command|depend|seed|path|time|size|bytes|format|",
      "repository|iteration|parent|children|seconds|warnings|error"
    ),
    "f|function|42ed6c6cf429ec42",
    "resources2|object|c7fa586ec71716f7",
    "resources|object|40cf4ff97d03671e",
    paste0(
      "z2|stem|164f6b22e6f7cb6a|0e2890e1b1d6be9c|ef46db3751d8e999|-2091466813",
      "|bucket=targets-test-versioned*region=NULL*key=_targets/objects/z2*",
      "endpoint=TlVMTA*version=4MeJDr09__xWul7SY4p40bB30UpV_sfT|t19648.",
      "5414026703s||50|rds|aws|vector|||0.001||"
    ),
    paste0(
      "x|stem|164f6b22e6f7cb6a|0e2890e1b1d6be9c|ef46db3751d8e999",
      "|-1032428690||t19648.5414097547s|ded833868582137a|50",
      "|rds|local|vector|||0||"
    ),
    paste0(
      "y|stem|164f6b22e6f7cb6a|0e2890e1b1d6be9c|ef46db3751d8e999|-1963496355|",
      "bucket=targets-test-unversioned*region=NULL*key=_targets/objects/y",
      "*endpoint=TlVMTA*version=|t19648.5414098716s||50|rds|aws|vector|||0||"
    ),
    paste0(
      "z|stem|164f6b22e6f7cb6a|0e2890e1b1d6be9c|ef46db3751d8e999|-499386612|",
      "bucket=targets-test-unversioned*region=NULL*key=_targets/objects/z*",
      "endpoint=TlVMTA*version=|t19648.5414154381s||50|rds|aws|vector|||0||"
    ),
    paste0(
      "y2|stem|164f6b22e6f7cb6a|0e2890e1b1d6be9c|ef46db3751d8e999|-62670671|",
      "bucket=targets-test-versioned*region=NULL*key=_targets/objects/y2*",
      "endpoint=TlVMTA*version=P5eyZdO.JwR__kS5fdxWcBgJLH4BmFIP|",
      "t19648.5414172763s||50|rds|aws|vector|||0.001||"
    )
  )
  dir_create(path_meta_dir(path_store_default()))
  writeLines(lines, path_meta(path_store_default()))
  before <- tar_meta()
  tar_unversion(names = tidyselect::any_of(c("x", "z2")))
  after <- tar_meta()
  expect_equal(before$name, after$name)
  names <- c(
    c(
      "f",
      "resources",
      "resources2",
      "x",
      "y",
      "y2",
      "z"
    )
  )
  for (name in names) {
    index <- which(before$name == name)
    expect_equal(before$path[[index]], after$path[[index]])
  }
  before <- before$path[[which(before$name == "z2")]]
  after <- after$path[[which(after$name == "z2")]]
  for (index in which(!grepl("^version=", before))) {
    expect_equal(before[index], after[index])
  }
  index <- which(grepl("^version=", before))
  expect_false(before[index] == after[index])
  expect_equal(after[index], "version=")
  expect_gt(nchar(before[index]), nchar(after[index]))
  expect_equal(nchar(store_aws_version(before)), 32L)
  expect_null(store_aws_version(after))
})
