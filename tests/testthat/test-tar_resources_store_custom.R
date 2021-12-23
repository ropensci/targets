test_that("tar_resources_store_custom() on nontrivial settings", {
  out <- tar_resources_store_custom(
    read = function(path) {
      keras::load_model_hdf5(path)
    },
    write = function(object, path) {
      keras::save_model_hdf5(object = object, filepath = path)
    },
    marshal = function(object) {
      keras::serialize_model(object)
    },
    unmarshal = function(object) {
      keras::unserialize_model(object)
    },
    repository = "default"
  )
  expect_silent(resources_validate(out))
})

test_that("tar_resources_store_custom() on default settings", {
  expect_silent(resources_validate(tar_resources_store_custom()))
})

test_that("tar_resources_store_custom() bad function arguments", {
  expect_error(
    tar_resources_store_custom(
    read = function(x) {
      keras::load_model_hdf5(x)
    }),
    class = "tar_condition_validate"
  )
})
