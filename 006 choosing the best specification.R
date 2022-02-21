# Loading the resulting files from the hyperparameter search

load("results/oneTraining3RNNeven.R") #runsOne3RNNeven
load("results/twoTraining3RNNeven.R") #runsTwo3RNNeven

load("results/oneTraining2GRUDense.R") #runsOne2GRUDense
load("results/twoTraining2GRUDense.R") #runsTwo2GRUDense

load("results/oneTraining1GRU1Dense.R") #runsOne1GRUDense
load("results/twoTraining1GRU1Dense.R") #runsTwo1GRUDense

load("results/oneTrainingDenseGRUDense.R") #runsOneDenseGRUDense
load("results/twoTrainingDenseGRUDense.R") #runsTwoDenseGRUDense

library(tidyverse)

runsOne3RNNeven %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsOne3RNNeven %>% 
  filter(metric_val_accuracy == max(metric_val_accuracy)) %>% 
  glimpse()

runsTwo3RNNeven %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsTwo3RNNeven %>% 
  filter(metric_val_accuracy == max(metric_val_accuracy)) %>% 
  glimpse()


