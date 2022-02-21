#' Trains a feedforward DL model on the MNIST dataset.


# Prerequisites -----------------------------------------------------------

library(keras)


# Data Prep ---------------------------------------------------------------

# Import data

load("data/twoVariableDataset.RData")

################################################
# Get model one variable

# training data
X_train <- X_trainTwo
y_train <- y_trainTwo

# testing data
X_test <- X_testTwo
y_test <- y_testTwo

# validation data
X_valid <- X_validTwo
y_valid <- y_validTwo


# Hyperparameter flags ---------------------------------------------------

FLAGS <- flags(
  # nodes
  flag_numeric("nodes1", 64),
  flag_numeric("nodes2", 32),
  # learning paramaters
  flag_string("optimizer", "rmsprop"),
  flag_numeric("lr_annealing", 0.1)
)

# Define Model --------------------------------------------------------------

# initialize our model
model <- keras_model_sequential() %>% 
  layer_dense(input_shape = dim(X_train)[2:3], units = FLAGS$nodes1, activation = 'tanh') %>%
  layer_simple_rnn(units = FLAGS$nodes2, activation = 'tanh') %>%
  layer_dense(units = 1, activation = 'sigmoid') %>% # output
  compile(
    loss = 'binary_crossentropy',
    metrics = c('accuracy'),
    optimizer = FLAGS$optimizer
  ) %>% fit(
    x = X_train,
    y = y_train,
    epochs = 35,
    batch_size = 50,
    validation_data = list(X_valid, y_valid),
    callbacks = list(
      callback_early_stopping(patience = 5),
      callback_reduce_lr_on_plateau(factor = FLAGS$lr_annealing)
    ),
    verbose = FALSE
  )
