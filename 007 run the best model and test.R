# get 2017 data

pa_window2017 %>% head()


dataXOne2017 <- as.matrix(pa_window2017[, c("frac_minus2", "frac_minus1", "frac_current")])
X_testOne2017 <- array(dataXOne2017, dim = c(nrow(dataXOne2017), 3, 1))

dataXTwo2017 <- as.matrix(pa_window2017[, c("frac_minus2", "frac_minus1", "frac_current", "fracLag_minus2", "fracLag_minus1", "fracLag_current")])
X_testTwo2017 <- array(dataXTwo2017, dim = c(nrow(dataXTwo2017), 3, 2))


dataY2017 <- as.matrix(pa_window2017[, c("clust_plus1")])


# One variable:

load("data/oneVariableDataset.RData")

################################################
# Get model one variable

# training data
X_train <- X_trainOne
y_train <- y_trainOne

# testing data
X_test <- X_testOne
y_test <- y_testOne

# validation data
X_valid <- X_validOne
y_valid <- y_validOne


# initialize our model
model1 <- keras_model_sequential() %>% 
  layer_simple_rnn(input_shape = c(dim(X_train)[2:3]), 
                   units = 128, 
                   return_sequences = T, 
                   dropout = 0.2) %>%
  layer_simple_rnn(units = 64, return_sequences = T, dropout = 0.3) %>%
  layer_simple_rnn(units = 16, return_sequences = F, dropout = 0.3) %>%
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  compile(
    loss = 'binary_crossentropy',
    metrics = c('accuracy'),
    optimizer = "rmsprop")

# Actually train our model
trained_model1 <- model1 %>% fit(
  x = X_train,
  y = y_train,
  epochs = 50,
  batch_size = 50,
  validation_data = list(X_valid, y_valid),
  callbacks = list(
    callback_early_stopping(patience = 5),
    callback_reduce_lr_on_plateau(factor = 0.1)),)


trained_model1
summary(model1)
summary(trained_model1)
plot(trained_model1)



results1 <- evaluate(model1, X_test, y_test, batch_size=50)
results1

results1a <- evaluate(model1, X_testOne2017, dataY2017, batch_size=50)
results1a


# Predict the results for the test data
prediction1 <- model1 %>% predict(X_test, batch_size = 50)
prediction1a <- model1 %>% predict(X_testOne2017, batch_size = 50)

plot(prediction1, y_test)
plot(prediction1a, dataY2017)
trained_model1



################################################

# clear keras session
k_clear_session()

################################################
# Two variables:

load("data/twoVariableDataset.RData")

################################################
# Get model two variable

# training data
X_train <- X_trainTwo
y_train <- y_trainTwo

# testing data
X_test <- X_testTwo
y_test <- y_testTwo

# validation data
X_valid <- X_validTwo
y_valid <- y_validTwo


# initialize our model
model2 <- keras_model_sequential() %>% 
  layer_simple_rnn(input_shape = c(dim(X_train)[2:3]), 
                   units = 32, 
                   return_sequences = T, 
                   dropout = 0.2) %>%
  layer_simple_rnn(units = 16, return_sequences = T, dropout = 0.2) %>%
  layer_simple_rnn(units = 16, return_sequences = F, dropout = 0.3) %>%
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  compile(
    loss = 'binary_crossentropy',
    metrics = c('accuracy'),
    optimizer = "rmsprop"
  ) 

# Actually train our model
trained_model2 <- model2 %>% fit(
  x = X_train,
  y = y_train,
  epochs = 50,
  batch_size = 50,
  validation_data = list(X_valid, y_valid),
  callbacks = list(
    callback_early_stopping(patience = 5),
    callback_reduce_lr_on_plateau(factor = 0.05)),)


trained_model2
summary(trained_model2)
plot(trained_model2)


results2 <- evaluate(model2, X_test, y_test, batch_size=50)
results2

results2a <- evaluate(model2, X_testTwo2017, dataY2017, batch_size=50)
results2a

# Predict the results for the test data
prediction2 <- model2 %>% predict(X_test, batch_size = 50)
prediction2a <- model2 %>% predict(X_testTwo2017, batch_size = 50)

plot(prediction2, y_test)
trained_model2






############# testing and initial plotting

play <- pop.grid.waw.sf
play$totalCount <- pop.df.waw.firmsAcc.totals$totCount
play$frac10 <- pop.df.waw.firmsAcc.totals$frac10
play$lag3_frac10 <- pop.df.waw.firmsAcc.totals$lag3_frac10
play$count2018 <- pop.df.waw.firmsAcc.totals$Year2018
play$predictOne2017full <- prediction1a[,1]
play$predictTwo2017full <- prediction2a[,1]
play$true2017 <- dataY2017[,1]


library(viridis)
p1 <- ggplot() +
  geom_sf(play, mapping = aes(geometry = geometry, fill = predictOne2017full))+
  labs(title = "One-variable model", fill = "prediction") +
  scale_fill_viridis() + theme_bw() +  theme(legend.position="bottom")

p2 <- ggplot() +
  geom_sf(play, mapping = aes(geometry = geometry, fill = predictTwo2017full))+
  labs(title = "Two-variables model", fill = "prediction") +
  scale_fill_viridis() + theme_bw() +  theme(legend.position="bottom")

p3 <- ggplot() +
  geom_sf(play, mapping = aes(geometry = geometry, fill = true2017))+
  labs(title = "True clusters in T+1=2018", fill = "cluster") +
  scale_fill_viridis() + theme_bw() +  theme(legend.position="bottom")


ggplot() +
  geom_sf(play, mapping = aes(geometry = geometry, fill = true2017))#+

library(cowplot)
plot_grid(p1, p2, p3, ncol = 3)


