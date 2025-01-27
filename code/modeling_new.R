library(mlr3verse)
library(mlr3learners)
library(mlr3extralearners)
library(R6)
library(mlr3)
library(mlr3tuning)
library(paradox)
library(data.table)



custom_gp <- R6::R6Class(
  "CustomGPRegr",
  inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.custom_gp",
        feature_types = c("numeric", "integer"),
        predict_types = c("response"),
        param_set = ps(
          sigma = p_dbl(0, 2, default = 0.1),  # Adjusted range
          C = p_dbl(0, 100, default = 1)
        )
      )
    }
  ),
  private = list(
    .train = function(task) {
      train_data <- task$data(cols = task$feature_names)
      train_labels <- task$truth()
      
      # Box-Cox transformation for target
      lambda <- 0.5  # Example: Square root transformation
      transformed_labels <- (train_labels^lambda - 1) / lambda
      
      self$state <- list(
        train_data = train_data,
        train_labels = transformed_labels,
        inverse_transform = function(x) (lambda * x + 1)^(1 / lambda)  # Inverse
      )
      invisible(self)
    },
    
    .predict = function(task) {
      train_data <- self$state$train_data
      train_labels <- self$state$train_labels
      test_data <- task$data(cols = task$feature_names)
      sigma <- self$param_set$values$sigma
      
      hamming_kernel <- function(x, y, sigma) {
        exp(-sum(x != y) / sigma)
      }
      
      predictions <- apply(test_data, 1, function(instance) {
        similarities <- apply(train_data, 1, function(train_instance) {
          hamming_kernel(train_instance, instance, sigma)
        })
        if (sum(similarities) == 0) {
          return(mean(train_labels))  # Default prediction if no neighbors found
        }
        sum(similarities * train_labels) / sum(similarities)
      })
      
      # Apply the inverse transformation to predictions
      predictions <- self$state$inverse_transform(predictions)
      list(response = predictions)
    }
  )
)

# Custom KNN Learner 

# Custom Prediction Class
# CustomPredictionRegr <- R6::R6Class(
#   "CustomPredictionRegr",
#   inherit = PredictionRegr,
#   public = list(
#     initialize = function(task, row_ids, response, mean_distance, sd_distance) {
#       super$initialize(task = task, row_ids = row_ids, response = response)
#       self$mean_distance <- mean_distance
#       self$sd_distance <- sd_distance
#     },
#     mean_distance = NULL,
#     sd_distance = NULL
#   )
# )


custom_knn <- R6::R6Class(
  "CustomKNNRegr",
  inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.custom_knn",
        feature_types = c("numeric", "integer"),
        predict_types = c("response"),
        param_set = ps(
          k = p_int(lower = 1, upper = 12)
        )
      )
    }
  ),
  private = list(
    .train = function(task) {
      
      # Store training data and labels in self$state
      self$state <- list(  # Store only the necessary training data
        train_data = task$data(cols = task$feature_names),
        train_labels = task$truth()
      )
      invisible(self)
    },
    
    .predict = function(task) {
      
      # Ensure the training data exists
      if (is.null(self$state$train_data) || is.null(self$state$train_labels)) {
        stop("Training data is not available. Please train the model first.")
      }
      # Extract test data
      test_data <- task$data(cols = task$feature_names)
      k <- self$param_set$values$k
      
      # Function to compute Hamming distances in a vectorized way
      # Function to calculate Hamming distance element-wise between train and test instances
      # compute_hamming_distance <- function(train_instance, test_instance) {
      #   sum(train_instance != test_instance) / ncol(test_instance) # Hamming distance (element-wise comparison)
      # }
      
      # Apply Hamming distance calculation and prediction in one go
      # dist_matrix <- outer(1:nrow(test_data), 1:nrow(self$state$train_data), 
      #                      Vectorize(function(i, j) sum(test_data[i, ] != self$state$train_data[j, ])))
      # Calculate Hamming distance row-wise
      dist_matrix <- t(apply(test_data, 1, function(test_instance) {
        apply(self$state$train_data, 1, function(train_instance) {
          sum(train_instance != test_instance)
        })
      }))
      
      
      
      # Use apply to get the nearest neighbors and average their labels
      predictions <- apply(dist_matrix, 1, function(distances) {
        nearest_indices <- order(distances)[1:k]
        mean(self$state$train_labels[nearest_indices])
      })
      
      list(response = predictions)
      
      # # Predictions and distances
      # predictions <- numeric(nrow(test_data))
      # 
      # # For storing mean distance and standard deviation separately
      # mean_distances <- numeric(nrow(test_data))
      # sd_distances <- numeric(nrow(test_data))
      # 
      # # Predict for each test instance
      # # Calculate predictions and distances
      # for (i in seq_len(nrow(test_data))) {
      #   #instance <- test_data[i, , drop = FALSE]
      #   # Compute Hamming distances for each row in the training data
      #   # distances <- apply(self$state$train_data, 1, function(train_instance) {
      #   #   # Hamming distance: sum of mismatches
      #   #   sum(train_instance != instance) / length(instance) 
      #   # })
      #   # Calculate the distances to all training instances
      #   test_instance <- test_data[i, , drop = FALSE]
      #   distances <- apply(self$state$train_data, 1, function(train_instance) {
      #     compute_hamming_distance(train_instance, test_instance) 
      #   })
      #   
      #   k_actual <- min(k, nrow(self$state$train_data))
      #   nearest <- order(distances)[1:k_actual]
      #   
      #   # Store distances for additional statistics
      #   nearest_distances <- distances[nearest]
      #   mean_distances[i] <- mean(nearest_distances)
      #   #sd_distances[i] <- sd(nearest_distances)
      #   
      #   # Prediction is the mean of nearest neighbors' labels
      #   predictions[i] <- mean(self$state$train_labels[nearest])
      # }
      # 
      # # Store the additional statistics in the model's state
      # self$state$mean_distances <- mean_distances
      # #self$state$sd_distances <- sd_distances
      # 
      # # Return only the standard prediction output
      # return(PredictionRegr$new(task = task, response = predictions))
      
      
      # predictions <- sapply(seq_len(nrow(test_data)), function(i) {
      # 
      #   instance <- test_data[i, , drop = FALSE]
      #   # Compute Hamming distances for each row in the training data
      #   distances <- apply(self$state$train_data, 1, function(train_instance) {
      #     # Hamming distance: sum of mismatches
      #     sum(train_instance != instance) / length(instance) 
      #   })
      #   
      #   # Handle cases where there are fewer training instances than k
      #   k_actual <- min(k, nrow(self$state$train_data))
      #   
      #   # Find the k-nearest neighbors
      #   nearest <- order(distances)[1:k_actual]
      #   
      #   # Calculate the average distance of the k-nearest neighbors
      #   mean_distance <- mean(distances[nearest])
      #   sd_distance <- sd(distances[nearest])
      #   
      #   # Return the mean of their responses
      #   #prediction <- mean(self$state$train_labels[nearest])
      #   
      #   
      #   nearest_distances <- distances[nearest]
      #   
      #   # Avoid division by zero by adding a small constant
      #   weights <- 1 / (nearest_distances + 1e-5)
      #   
      #   # Calculate the weighted average of the responses
      #   sum(weights * self$state$train_labels[nearest]) / sum(weights)
        
        
        
        #list(prediction = weighted_avg, mean_distance = mean_distance, sd_distance = sd_distance)
      #})
      
      # list(
      #   response = sapply(predictions, function(x) x$prediction),
      #   mean_distance = sapply(predictions, function(x) x$mean_distance),
      #   sd_distance = sapply(predictions, function(x) x$sd_distance)
      # )
      # CustomPredictionRegr$new(
      #   task = task,
      #   row_ids = seq_len(nrow(test_data)),
      #   response = predictions,
      #   mean_distance = mean_distances,
      #   sd_distance = sd_distances
      # )
    }
  )
)




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
    learners = list(knn_tuner, gp_tuner),
    tasks = list(task),
    resamplings = list(outer_resampling)
  )
  
  # Benchmark both models using nested resampling and autotuning
  benchmark_result <- benchmark(design)
  return(benchmark_result)
}, future.seed=123)

t2 = timestamp()

save(bmr, file = "bmr_1701.RData")
