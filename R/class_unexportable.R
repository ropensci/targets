#' @export
store_marshal_value.tar_nonexportable <- function(store, target) {
  object <- store_marshal_object(target$store, target$value$object)
  target$value <- value_init(object, iteration = target$settings$iteration)
}

#' @export
store_unmarshal_value.tar_nonexportable <- function(store, target) {
  object <- store_unmarshal_object(target$store, target$value$object)
  target$value <- value_init(object, iteration = target$settings$iteration)
}
