resources_init <- function(
  aws = resources_aws_init(),
  clustermq = resources_clustermq_init(),
  feather = resources_feather_init(),
  fst = resources_fst_init(),
  future = resources_future_init(),
  parquet = resources_parquet_init(),
  qs = resources_qs_init(),
  url = resources_url_init()
) {
  resources_new(
    aws = aws,
    clustermq = clustermq,
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
  clustermq = NULL,
  feather = NULL,
  fst = NULL,
  future = NULL,
  parquet = NULL,
  qs = NULL,
  url = NULL
) {
  force(aws)
  force(clustermq)
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
  resources_validate(resources$aws)
  resources_validate(resources$clustermq)
  resources_validate(resources$feather)
  resources_validate(resources$fst)
  resources_validate(resources$future)
  resources_validate(resources$parquet)
  resources_validate(resources$qs)
  resources_validate(resources$url)
}

#' @export
print.tar_resources <- function(x, ...) {
  lines <- map_chr(names(x), tar_resources_lines, values = x)
  cat(paste(c("<tar_resources>", lines), collapse = "\n"))
}

tar_resources_lines <- function(name, values) {
  header <- sprintf("  <tar_resources_%s>", name)
  lines <- paste0(paste_list(as.list(values[[name]])), collapse = "\n    ")
  if_any(
    any(nzchar(lines)),
    paste(header, paste("   ", lines), sep = "\n"),
    header
  )
}
