switch_store <- function(store) {
  assert_store(store)
  tar_config$unset_lock()
  old_store <- tar_config$get_store()
  tar_config$assign_store(store)
  tar_config$set_lock()
  old_store
}

restore_store <- function(store) {
  tar_config$unset_lock()
  tar_config$assign_store(store)
}
