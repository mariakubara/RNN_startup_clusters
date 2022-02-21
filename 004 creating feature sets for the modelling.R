##############################
# Setting the parameters

message('Set parameters')

random_seed = 999 

pct_train = .70 #0.7
pct_test = .50 * (1 - pct_train)
pct_valid = pct_test

look_back = 3
look_forward = 1
min_length = look_back + look_forward
max_length = 20
above_pct = 0.50


###############################
# Divide train/test/validation

# Training/testing/validation split on cell IDs so as to avoid data leakage between test/validation and training sets.
message('Training/testing/validation split on cell IDs')
set.seed(random_seed)


## Vector of all cells

cells_vec <- unique(pa_window$ID_grid)
ncells <- length(cells_vec)
ntrain <- floor(pct_train * ncells)

## Training set
cellsIDs_train <- sample(x=cells_vec, size=ntrain, replace=F)

## Testing and validations sets
cellsIDs_testvalid <- cells_vec[cells_vec %in% cellsIDs_train == F]
ntest <- floor(0.5 * length(cellsIDs_testvalid)) 
cellsIDs_valid <- sample(cellsIDs_testvalid, size=ntest, replace=F)
cellsIDs_test <- cellsIDs_testvalid[cellsIDs_testvalid %in% cellsIDs_valid == F]


## All cells accounted for?
all_accounted_for <- length(cellsIDs_train) + length(cellsIDs_test) + length(cellsIDs_valid) == ncells
cat('All cells accounted for?', all_accounted_for, '\n')


# Add train/test/validation split labels to the filtering dataframe

pa_filtered <- pa_window %>%
  mutate(data_split = case_when(ID_grid %in% cellsIDs_train ~ 'train',
                                ID_grid %in% cellsIDs_test ~ 'test',
                                ID_grid %in% cellsIDs_valid ~ 'valid',
                                TRUE ~ 'other'),
         min_year = yearID - 2,
         max_year = yearID + 1) 


cells_raw <- pa_filtered %>% # base table
  select(-c(min_year, max_year)) %>% 
  arrange(ID_grid, yearID) %>%
  replace(is.na(.), 0) %>% # replace missing values with zeroes
  mutate(idx = as.numeric(as.factor(ID_grid))) %>%
  select_at(c('ID_grid', 'idx', colnames(.)[colnames(.) %in% c('ID_grid', 'idx') == F])) %>%
  distinct() 




#Export to CSV
message('Export to CSV')
cells_raw %>%write_csv('data/cells_data_modeling_cluster_no2017.csv')


###############################################################################

# initial dataset with all cells, divided into train test valid
# all variables - frac and frac_lag
# 2 years back and now, predict future year
# span 2012 - 2017 (2 years back for data imputing, one year to know the future)

cells_raw %>% head()

data_split_vec <- cells_raw$data_split
data_split_train <- data_split_vec == "train"
data_split_test <- data_split_vec == "test"
data_split_valid <- data_split_vec == "valid"

dataXOne <- as.matrix(cells_raw[, c("frac_minus2", "frac_minus1", "frac_current")])

dataXTwo <- as.matrix(cells_raw[, c("frac_minus2", "frac_minus1", "frac_current", "fracLag_minus2", "fracLag_minus1", "fracLag_current")])

dataY <- as.matrix(cells_raw[, c("clust_plus1")])



################################################
# set some parameters for our model
howManyYears <- 5 #2012 - 2016 (2017 for testing)
max_len <- 3 # the number of previous examples we'll look at
max_len_lag <- 6 # the number of previous examples we'll look at including lag
batch_size <- 50 # number of sequences to look at at one time during training
total_epochs <- 30 # how many times we'll look @ the whole dataset while training our model
normalize <- TRUE # if we add normalization to the model

# set a random seed for reproducability
set.seed(100)



################################################
# Model with one variable only - feature sets

# training data
X_trainOne <- array(dataXOne[data_split_train,], dim = c(length(which(data_split_train == TRUE)), max_len, 1))
y_trainOne <- dataY[data_split_train,]

# testing data
X_testOne <- array(dataXOne[data_split_test,], dim = c(length(which(data_split_test == TRUE)), max_len, 1))
y_testOne <- dataY[data_split_test,]


# validation data
X_validOne <- array(dataXOne[data_split_valid,], dim = c(length(which(data_split_valid == TRUE)), max_len, 1))
y_validOne <- dataY[data_split_valid,]

save(X_trainOne, y_trainOne, X_testOne, y_testOne, X_validOne,  y_validOne, file = "data/oneVariableDataset.RData")
#load("data/oneVariableDataset.RData")


################################################
# Model with two variables - feature sets

# training data

#shape in dim -> samples, timesteps, features

X_trainTwo <- array(dataXTwo[data_split_train,], dim = c(length(which(data_split_train == TRUE)), max_len, 2))
y_trainTwo <- dataY[data_split_train,]

# testing data
X_testTwo <- array(dataXTwo[data_split_test,], dim = c(length(which(data_split_test == TRUE)), max_len, 2))
y_testTwo <- dataY[data_split_test,]


# validation data
X_validTwo <- array(dataXTwo[data_split_valid,], dim = c(length(which(data_split_valid == TRUE)), max_len, 2))
y_validTwo <- dataY[data_split_valid,]

save(X_trainTwo, y_trainTwo, X_testTwo, y_testTwo, X_validTwo,  y_validTwo, file = "data/twoVariableDataset.RData")
#load("data/twoVariableDataset.RData")



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


################################################
# Get model two variables

# training data
X_train <- X_trainTwo
y_train <- y_trainTwo

# testing data
X_test <- X_testTwo
y_test <- y_testTwo

# validation data
X_valid <- X_validTwo
y_valid <- y_validTwo


################################################
# Create some initial model - check if all works

# initialize our model
model <- keras_model_sequential()


# our input layer
model %>%
  layer_simple_rnn(input_shape = c(dim(X_train)[2:3]), units = 64, return_sequences = T, 
                   dropout = 0.2) %>%
  layer_simple_rnn(units = 32, return_sequences = T, dropout = 0.2) %>%
  layer_simple_rnn(units = 16, return_sequences = F, dropout = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid') # output


# look at our model architecture
summary(model)

model %>% compile(loss = 'binary_crossentropy', 
                  optimizer = optimizer_rmsprop(lr = 0.01), 
                  metrics = c('accuracy'))


# set a random seed for reproducability
set.seed(100)

# Actually train our model! This step will take a while
trained_model <- model %>% fit(
  x = X_train, # sequence we're using for prediction 
  y = y_train, # sequence we're predicting
  batch_size = batch_size, # how many samples to pass to our model at a time
  epochs = total_epochs, # how many times we'll look @ the whole dataset
  validation_data = list(X_valid, y_valid),
  callbacks = callback_reduce_lr_on_plateau(factor = 0.2, patience = 5),) # how much data to hold out for testing as we go along


plot(trained_model)
trained_model

model %>% evaluate(X_test, y_test, batch_size = batch_size)


# Predict the results for the test data
prediction <- model %>% predict(X_test, batch_size = batch_size)

# clear keras session
k_clear_session()


