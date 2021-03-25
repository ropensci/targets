# Requires Python Keras, which is a pain to work into CI.
library(testthat)

tar_test("keras format", {
  skip_if_not_installed("keras")
  envir <- new.env(parent = globalenv())
  envir$f <- function() {
    model <- keras::keras_model_sequential() %>%
      keras::layer_conv_2d(
        filters = 32,
        kernel_size = c(3, 3),
        activation = "relu",
        input_shape = c(28, 28, 1)
      ) %>%
      keras::layer_conv_2d(
        filters = 64,
        kernel_size = c(3, 3),
        activation = "relu"
      ) %>%
      keras::layer_max_pooling_2d(pool_size = c(2, 2)) %>%
      keras::layer_dropout(rate = 0.25) %>%
      keras::layer_flatten() %>%
      keras::layer_dense(units = 128, activation = "relu") %>%
      keras::layer_dropout(rate = 0.5) %>%
      keras::layer_dense(units = 10, activation = "softmax")
    keras::compile(
      model,
      loss = "categorical_crossentropy",
      optimizer = keras::optimizer_adadelta(),
      metrics = "accuracy"
    )
    model
  }
  x <- target_init(
    name = "abc",
    expr = quote(f()),
    format = "keras"
  )
  expect_silent(target_validate(x))
  builder_update_build(x, envir)
  expect_true(
    inherits(x$value$object, "keras.engine.training.Model")
  )
  builder_update_paths(x)
  builder_update_object(x)
  expect_true(
    inherits(target_read_value(x)$object, "keras.engine.training.Model")
  )
})
