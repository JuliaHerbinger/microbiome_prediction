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


# Define tuning and search spaces
search_space_knn <- ps(
   k = p_int(1, 15) 
 )

# search_space_gp <- ps(
#   sigma = p_dbl(lower = -2, upper = 0, trafo = function(x) 10^x)  # ParamDbl for sigma
# )

search_space_gp2 <- ps(
  sigma = p_dbl(lower = 0.01, upper = 1)  # ParamDbl for sigma
)

 search_space_svm <- ps(
   C = p_dbl(0.01, 10),  
   sigma = p_dbl(0.01, 1) 
 )








#set.seed(123)

# Inner resampling for tuning
inner_resampling <- rsmp("cv", folds = 5)

# Outer resampling for evaluation
outer_resampling <- rsmp("loo")

# Define AutoTuner for KNN
knn_tuner <- AutoTuner$new(
  learner = custom_knn$new(),
  resampling = inner_resampling,
  measure = msr("regr.rmse"),
  search_space = search_space_knn,
  terminator = trm("none"),
  tuner = tnr("grid_search",resolution = 8),
  store_tuning_instance = TRUE
)

# Define AutoTuner for GP
# gp_tuner <- AutoTuner$new(
#   learner = custom_gp$new(),
#   resampling = inner_resampling,
#   measure = msr("regr.rmse"),
#   search_space = search_space_gp,
#   terminator = trm("evals", n_evals = 20),
#   tuner = tnr("random_search"),
#   store_tuning_instance = TRUE
# )


# library(mlr3mbo)
# library(DiceKriging)
# library(rgenoud)
# gp_tuner_auto <- AutoTuner$new(
#   learner = custom_gp$new(),
#   resampling = inner_resampling,
#   measure = msr("regr.rmse"),
#   search_space = search_space_gp,
#   terminator = trm("evals", n_evals = 30),
#   tuner = tnr("mbo"),
#   store_tuning_instance = TRUE
# )
#gp_tuner_auto$encapsulate(method = "evaluate", fallback = lrn("regr.featureless"))

gp_tuner_kernel2 <- AutoTuner$new(
  learner = custom_gp2$new(),
  resampling = inner_resampling,
  measure = msr("regr.rmse"),
  search_space = search_space_gp2,
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("grid_search"),
  store_tuning_instance = TRUE
)

lrn_gp_kernel3 = custom_gp2$new()
lrn_gp_kernel3$param_set$values$sigma = 1


# Define AutoTuner for SVM
#options(mlr3.encapsulate = "evaluate")  # ensures errors do not crash benchmarking

svm_tuner <- AutoTuner$new(
  learner = custom_svm$new(),
  resampling = inner_resampling,
  measure = msr("regr.rmse"),
  search_space = search_space_svm,
  terminator = trm("evals", n_evals = 10),
  tuner = tnr("random_search"),
  store_tuning_instance = TRUE
)
# 
# 
# # define additional custom knn learners without tuning 
# # to compare agains average distance
# lrn_knn2 = custom_knn$new()
# lrn_knn2$param_set$values$k = 2
# lrn_knn5 = custom_knn$new()
# lrn_knn5$param_set$values$k = 5
# lrn_knn8 = custom_knn$new()
# lrn_knn8$param_set$values$k = 8
# lrn_knn11 = custom_knn$new()
# lrn_knn11$param_set$values$k = 11
# 

set.seed(123)
#rn = sample(1:ncol(Y), 10)
#Ysub = Y[,rn]


library(future.apply)
options(future.globals.maxSize = 1e9)
#plan("future::multisession") ## => parallelize on your local computer
t1 = timestamp()
plan(multisession, workers = 10)


bmr <- future_lapply(Y, function(y) {
  # Define task
  #options(mlr3.encapsulate = "evaluate")
  
  data = cbind(X, "y" = y)
  task <- as_task_regr(data, target = "y")
  
  # Define benchmark design (which includes learners, task, resampling strategy, and performance measures)
  design <- benchmark_grid(
    learners = list(knn_tuner, gp_tuner_kernel2, svm_tuner),
    #learners = list(knn_tuner, gp_tuner, lrn_knn2, lrn_knn5, lrn_knn8, lrn_knn11),
    tasks = list(task),
    resamplings = list(outer_resampling)
  )
  
  # Benchmark both models using nested resampling and autotuning
  #set.seed(123)
  benchmark_result <- benchmark(design)
  print(benchmark_result)
  return(benchmark_result)
}, future.seed=123)

t2 = timestamp()

save(bmr, file = paste0("results/bmr_", format(Sys.time(), "%d%m%Y"), ".RData"))

aggr = benchmark_result$aggregate()
aggr = bmr[[1]]$aggregate()
rr = aggr$resample_result[[1]]
tab = as.data.table(rr$prediction())
