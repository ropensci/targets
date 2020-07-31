tar_option_reset <- function() {
  remove(list = names(tar_envir_options), envir = tar_envir_options)
}
