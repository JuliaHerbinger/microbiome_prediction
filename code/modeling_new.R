library(mlr3verse)
library(mlr3learners)
library(mlr3extralearners)
library(R6)
library(mlr3)
library(mlr3tuning)
library(paradox)
library(data.table)


# Define tuning and search spaces
search_space_knn <- ps(
  k = p_int(2, 12) 
)

search_space_gp <- ps(
  sigma = p_dbl(lower = -3, upper = 0, trafo = function(x) 10^x),  # ParamDbl for sigma
  C = p_dbl(lower = -2, upper = 2, trafo = function(x) 10^x) 
  
)



set.seed(123)

# Inner resampling for tuning
inner_resampling <- rsmp("cv", folds = 3)

# Outer resampling for evaluation
outer_resampling <- rsmp("loo")

# Define AutoTuner for KNN
knn_tuner <- AutoTuner$new(
  learner = custom_knn$new(),
  resampling = inner_resampling,
  measure = msr("regr.rmse"),
  search_space = search_space_knn,
  terminator = trm("none"),
  tuner = tnr("grid_search",resolution = 6),
  store_tuning_instance = TRUE
)

# Define AutoTuner for GP
gp_tuner <- AutoTuner$new(
  learner = custom_gp$new(),
  resampling = inner_resampling,
  measure = msr("regr.rmse"),
  search_space = search_space_gp,
  terminator = trm("evals", n_evals = 40),
  tuner = tnr("random_search"),
  store_tuning_instance = TRUE
)


# define additional custom knn learners without tuning 
# to compare agains average distance
lrn_knn2 = custom_knn$new()
lrn_knn2$param_set$values$k = 2
lrn_knn5 = custom_knn$new()
lrn_knn5$param_set$values$k = 5
lrn_knn8 = custom_knn$new()
lrn_knn8$param_set$values$k = 8
lrn_knn11 = custom_knn$new()
lrn_knn11$param_set$values$k = 11




library(future.apply)
options(future.globals.maxSize = 1e9)
#plan("future::multisession") ## => parallelize on your local computer
t1 = timestamp()
plan(multisession, workers = 10)

bmr <- future_lapply(Y, function(y) {
  # Define task
  data = cbind(X, "y" = y)
  task <- as_task_regr(data, target = "y")
  
  # Define benchmark design (which includes learners, task, resampling strategy, and performance measures)
  design <- benchmark_grid(
    learners = list(knn_tuner, gp_tuner, lrn_knn2, lrn_knn5, lrn_knn8, lrn_knn11),
    tasks = list(task),
    resamplings = list(outer_resampling)
  )
  
  # Benchmark both models using nested resampling and autotuning
  benchmark_result <- benchmark(design)
  return(benchmark_result)
}, future.seed=123)

t2 = timestamp()

save(bmr, file = paste0("results/bmr_", format(Sys.time(), "%d%m%Y"), ".RData"))
