#' @export
store_serialize_value.tar_nonexportable <- function(store, target) {
  object <- store_serialize_object(target$store, target$value$object)
  target$value <- value_init(object, iteration = target$settings$iteration)
}

#' @export
store_unserialize_value.tar_nonexportable <- function(store, target) {
  object <- store_unserialize_object(target$store, target$value$object)
  target$value <- value_init(object, iteration = target$settings$iteration)
}
