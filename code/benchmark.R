library(mlr3verse)
library(mlr3learners)
library(mlr3extralearners)
library(R6)
library(mlr3)
library(mlr3tuning)
library(paradox)
library(data.table)
library(kernlab)
library(bestNormalize)



#---------------------------------------------------------------------
# Define search spaces for tuning
search_space_knn <- ps(
   k = p_int(1, 15) 
 )

search_space_gp <- ps(
  sigma = p_dbl(lower = 0.01, upper = 1)  # ParamDbl for sigma
)

search_space_svm <- ps(
   C = p_dbl(0.01, 10),  
   sigma = p_dbl(0.01, 1) 
 )


#---------------------------------------------------------------------
# Define nested resampling strategy

# Inner resampling for tuning
inner_resampling <- rsmp("cv", folds = 5)

# Outer resampling for evaluation
outer_resampling <- rsmp("loo")


#---------------------------------------------------------------------
# Define AutoTuner for KNN
knn_tuner <- AutoTuner$new(
  learner = custom_knn$new(),
  resampling = inner_resampling,
  measure = mlr3::msr("regr.rmse"),
  search_space = search_space_knn,
  terminator = trm("none"),
  tuner = tnr("grid_search",resolution = 8),
  store_tuning_instance = TRUE
)


gp_tuner <- AutoTuner$new(
  learner = custom_gp$new(),
  resampling = inner_resampling,
  measure = mlr3::msr("regr.rmse"),
  search_space = search_space_gp,
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("grid_search"),
  store_tuning_instance = TRUE
)



svm_tuner <- AutoTuner$new(
  learner = custom_svm$new(),
  resampling = inner_resampling,
  measure = mlr3::msr("regr.rmse"),
  search_space = search_space_svm,
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("random_search"),
  store_tuning_instance = TRUE
)

#---------------------------------------------------------------------
# Run benchmark with parallelization

set.seed(123)

library(future.apply)
options(future.globals.maxSize = 1e9)

t1 = timestamp()
plan(multisession, workers = 10)

bmr <- future_lapply(Y, function(y) {
  # Define task
  data = cbind(X, "y" = y)
  task <- as_task_regr(data, target = "y")
  
  # Define benchmark design (which includes learners, task, resampling strategy, and performance measures)
  design <- benchmark_grid(
    learners = list(knn_tuner, gp_tuner, svm_tuner),
    tasks = list(task),
    resamplings = list(outer_resampling)
  )
  
  # Benchmark both models using nested resampling and autotuning
  benchmark_result <- benchmark(design)
  print(benchmark_result)
  return(benchmark_result)
}, future.seed=123)

t2 = timestamp()

save(bmr, file = paste0("results/bmr_", format(Sys.time(), "%d%m%Y"), ".RData"))

