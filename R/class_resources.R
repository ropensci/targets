resources_validate <- function(resources) {
  UseMethod("resources_validate")
}

#' @export
resources_validate.default <- function(resources) {
  tar_warn_deprecate(
    "Unstructured resource lists are deprecated in targets ",
    "version 0.5.0.9000 and above (2021-06-06). Use helpers ",
    "like tar_resources_clustermq() and tar_resources_feather() ",
    "to supply components of the resources arguments of tar_target() ",
    "and tar_option_set()."
  )
}
