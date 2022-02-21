library(tfruns)

getwd()

start_time <- Sys.time()

# Run various combinations of dropout1 and dropout2
runs <- tuning_run("parametrization_script_one.R", 
                   flags = list(
                     nodes1 = c(128, 64, 32),
                     nodes2 = c(64, 32, 16),
                     nodes3 = c(32, 16, 8),
                     dropout1 = c(0.2, 0.3, 0.4),
                     dropout2 = c(0.2, 0.3, 0.4),
                     dropout3 = c(0.2, 0.3, 0.4),
                     optimizer = c("rmsprop", "adam"),
                     lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

end_time <- Sys.time()
end_time - start_time

runs %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsOne3RNNeven <- runs

save(runsOne3RNNeven, file = "results/oneTraining3RNNeven.R")

########################################################################


library(tfruns)

start_time <- Sys.time()

# Run various combinations of dropout1 and dropout2
runs <- tuning_run("parametrization_script_two.R", 
                   flags = list(
                     nodes1 = c(128, 64, 32),
                     nodes2 = c(64, 32, 16),
                     nodes3 = c(32, 16, 8),
                     dropout1 = c(0.2, 0.3, 0.4),
                     dropout2 = c(0.2, 0.3, 0.4),
                     dropout3 = c(0.2, 0.3, 0.4),
                     optimizer = c("rmsprop", "adam"),
                     lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

end_time <- Sys.time()
end_time - start_time

runs %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()


runsTwo3RNNeven <- runs

save(runsTwo3RNNeven, file = "results/twoTraining3RNNeven.R")


########################################################################


library(tfruns)

start_time <- Sys.time()

# Run various combinations of dropout1 and dropout2
runs <- tuning_run("parametrization_script_gru_one.R", 
                   flags = list(
                     nodes1 = c(128, 64, 32),
                     nodes2 = c(64, 32, 16),
                     nodes3 = c(32, 16, 8),
                     dropout1 = c(0.2, 0.3, 0.4),
                     dropout2 = c(0.2, 0.3, 0.4),
                     dropout3 = c(0.2, 0.3, 0.4),
                     dropout4 = c(0.2, 0.3, 0.4),
                     optimizer = c("rmsprop", "adam"),
                     lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

end_time <- Sys.time()
end_time - start_time

runs %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsOne2GRUDense <- runs

save(runsOne2GRUDense, file = "results/oneTraining2GRUDense.R")


########################################################################


library(tfruns)

start_time <- Sys.time()

# Run various combinations of dropout1 and dropout2
runs <- tuning_run("parametrization_script_gru_two.R", 
                   flags = list(
                     nodes1 = c(128, 64, 32),
                     nodes2 = c(64, 32, 16),
                     nodes3 = c(32, 16, 8),
                     dropout1 = c(0.2, 0.3, 0.4),
                     dropout2 = c(0.2, 0.3, 0.4),
                     dropout3 = c(0.2, 0.3, 0.4),
                     dropout4 = c(0.2, 0.3, 0.4),
                     optimizer = c("rmsprop", "adam"),
                     lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

end_time <- Sys.time()
end_time - start_time

runs %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsTwo2GRUDense <- runs

save(runsTwo2GRUDense, file = "results/twoTraining2GRUDense.R")


########################################################################


library(tfruns)

start_time <- Sys.time()

# Run various combinations of dropout1 and dropout2
runs <- tuning_run("parametrization_script_gruDense_one.R", 
                   flags = list(
                     nodes1 = c(128, 64, 32, 16),
                     dropout1 = c(0.2, 0.3, 0.4),
                     dropout2 = c(0.2, 0.3, 0.4),
                     optimizer = c("rmsprop", "adam"),
                     lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

end_time <- Sys.time()
end_time - start_time

runs %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsOne1GRUDense <- runs

save(runsOne1GRUDense, file = "results/oneTraining1GRU1Dense.R")


########################################################################

library(tfruns)

start_time <- Sys.time()

# Run various combinations of dropout1 and dropout2
runs <- tuning_run("parametrization_script_gruDense_two.R", 
                   flags = list(
                     nodes1 = c(128, 64, 32, 16),
                     dropout1 = c(0.2, 0.3, 0.4),
                     dropout2 = c(0.2, 0.3, 0.4),
                     optimizer = c("rmsprop", "adam"),
                     lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

end_time <- Sys.time()
end_time - start_time

runs %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsTwo1GRUDense <- runs

save(runsTwo1GRUDense, file = "results/twoTraining1GRU1Dense.R")


########################################################################


library(tfruns)

start_time <- Sys.time()

# Run various combinations of dropout1 and dropout2
runs <- tuning_run("parametrization_script_DenseGRUDense_one.R", 
                   flags = list(
                     nodes1 = c(128, 64, 32, 16),
                     nodes2 = c(128, 64, 32, 16),
                     optimizer = c("rmsprop", "adam"),
                     lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

end_time <- Sys.time()
end_time - start_time

runs %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsOneDenseGRUDense <- runs

save(runsOneDenseGRUDense, file = "results/oneTrainingDenseGRUDense.R")


########################################################################

library(tfruns)

start_time <- Sys.time()

# Run various combinations of dropout1 and dropout2
runs <- tuning_run("parametrization_script_DenseGRUDense_two.R", 
                   flags = list(
                     nodes1 = c(128, 64, 32, 16),
                     nodes2 = c(128, 64, 32, 16),
                     optimizer = c("rmsprop", "adam"),
                     lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

end_time <- Sys.time()
end_time - start_time

runs %>% 
  filter(metric_val_loss == min(metric_val_loss)) %>% 
  glimpse()

runsTwoDenseGRUDense <- runs

save(runsTwoDenseGRUDense, file = "results/twoTrainingDenseGRUDense.R")


