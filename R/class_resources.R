resources_init <- function(
  aws = resources_aws_init(),
  feather = resources_feather_init(),
  fst = resources_fst_init(),
  future = resources_future_init(),
  parquet = resources_parquet_init(),
  qs = resources_qs_init(),
  url = resources_url_init()
) {
  resources_new(
    aws = aws,
    feather = feather,
    fst = fst,
    future = future,
    parquet = parquet,
    qs = qs,
    url = url
  )
}

resources_new <- function(
  aws = NULL,
  feather = NULL,
  fst = NULL,
  future = NULL,
  parquet = NULL,
  qs = NULL,
  url = NULL
) {
  force(aws)
  force(feather)
  force(fst)
  force(future)
  force(parquet)
  force(qs)
  force(url)
  enclass(environment(), "tar_resources")
}

resources_validate <- function(resources) {
  UseMethod("resources_validate")
}

#' @export
resources_validate.tar_resources <- function(resources) {
  resources_aws_validate(resources$aws)
  resources_feather_validate(resources$feather)
  resources_fst_validate(resources$fst)
  resources_future_validate(resources$future)
  resources_parquet_validate(resources$parquet)
  resources_qs_validate(resources$qs)
  resources_url_validate(resources$url)
}

#' @export
print.tar_resources <- function(x, ...) {
  cat(
    "<tar_resources>\n",
    " <tar_resources_aws>\n   ",
    paste0(paste_list(as.list(x$aws)), collapse = "\n    "),
    " <tar_resources_feather>\n   ",
    paste0(paste_list(as.list(x$feather)), collapse = "\n    "),
    " <tar_resources_fst>\n   ",
    paste0(paste_list(as.list(x$fst)), collapse = "\n    "),
    " <tar_resources_future>\n   ",
    paste0(paste_list(as.list(x$future)), collapse = "\n    "),
    " <tar_resources_parquet>\n   ",
    paste0(paste_list(as.list(x$parquet)), collapse = "\n    "),
    " <tar_resources_qs>\n   ",
    paste0(paste_list(as.list(x$qs)), collapse = "\n    "),
    " <tar_resources_url>\n   ",
    paste0(paste_list(as.list(x$url)), collapse = "\n    ")
  )
}
