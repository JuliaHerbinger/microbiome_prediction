library(philentropy)
library(tidyr)
library(tidyverse)

##### modeling #################################################################
library(R6)
library(mlr3)
library(mlr3learners)
library(kernlab)
library(bestNormalize)

# custom_gp <- R6Class("CustomGPRegr",
#                     inherit = LearnerRegr,
#                     public = list(
#                       initialize = function() {
#                         super$initialize(
#                           id = "regr.custom_gp",
#                           feature_types = c("integer", "numeric", "factor", "ordered"),
#                           predict_types = c("response"),
#                           param_set = ps(
#                             sigma = p_dbl(lower = -3, upper = 1, trafo = function(x) 10^x)
#                           )
#                         )
#                       }
#                     ),
#                     
#                     private = list(
#                       .train = function(task) {
#                         # Convert categorical features to numeric (integer representation)
#                         train_data <- task$data(cols = task$feature_names)
#                         #train_data_numeric <- as.data.frame(lapply(train_data, as.integer))
#                         train_labels <- task$truth()
#                         #browser()
#                         # Apply Yeo-Johnson transformation to target
#                         if (all(train_labels == 0)) {
#                           # Skip Yeo-Johnson transformation if all values are zero
#                           transformed_labels <- train_labels
#                         } else {
#                           self$state$yj_trans <- bestNormalize::yeojohnson(train_labels)
#                           transformed_labels <- self$state$yj_trans$x.t
#                         }
#                         
#                         # Center around zero
#                         mean_transformed_labels <- mean(transformed_labels)
#                         transformed_labels <- transformed_labels - mean_transformed_labels
#                         
#                         # Define custom Hamming kernel
#                         #hamming_kernel <- function(x, y) {
#                         #  exp(-sum(x != y) / self$param_set$values$sigma)
#                         #}
#                         
#                         # Convert to kernel matrix
#                         #kernel_matrix <- as.kernelMatrix(kernelMatrix(hamming_kernel, as.matrix(train_data)))
#                         hamming_kernel <- function(sigma = 1) {
#                           rval <- function(x, y) {
#                             if (!is.vector(x) || !is.vector(y)) stop("x and y must be vectors!")
#                             if (length(x) != length(y)) stop("x and y must have the same length!")
#                             
#                             # Compute Hamming distance
#                             dist <- sum(x != y)
#                             
#                             # Convert to similarity using an RBF-like formulation
#                             similarity <- exp(-dist / sigma)
#                             return(similarity)  # Must return a single scalar value
#                           }
#                           
#                           class(rval) <- "kernel"  # Assign the "kernel" class
#                           return(rval)
#                         }
#                         
#                         # Create Hamming kernel with hyperparameter sigma
#                         custom_hamming_kernel <- hamming_kernel(sigma = self$param_set$values$sigma)
#                         
#                         # Train GP model using the custom kernel
#                         self$state$gp_model <- kernlab::gausspr(
#                           x = as.matrix(train_data),
#                           y = transformed_labels,
#                           kernel = custom_hamming_kernel,
#                           scaled = FALSE
#                         )
#                         
#                         # Save the mean of the transformed labels to use during prediction
#                         self$state$mean_transformed_labels <- mean_transformed_labels
#                         
#                         invisible(self)
#                       },
#                       
#                       .predict = function(task) {
#                         # Convert test data to numeric
#                         test_data <- task$data(cols = task$feature_names)
#                         #test_data_numeric <- as.data.frame(lapply(test_data, as.integer))
#                         
#                         # Predict with trained GP model
#                         # Check if the model is available
#                         if (is.null(self$state$gp_model)) {
#                           message("Warning: No trained GP model available, using fallback mean prediction.")
#                           # Use the fallback model if the GP model is not available
#                           predictions <- rep(0, nrow(test_data))
#                         } else {
#                           # Make predictions using the trained GP model
#                           predictions <- predict(self$state$gp_model, newdata = as.matrix(test_data))
#                         }
#                         #predictions <- predict(self$state$gp_model, newdata = as.matrix(test_data))
#                         
#                         # Add back the mean of transformed labels to the predictions
#                         predictions <- predictions + self$state$mean_transformed_labels
#                         
#                         # Apply inverse Yeo-Johnson transformation manually (not using predict)
#                         # If the transformed labels were all zero, reverse the transformation manually (or skip if needed)
#                         if (all(task$truth() == 0)) {
#                           inverse_predictions <- predictions  # No transformation needed for all-zero case
#                         } else {
#                           # Apply inverse Yeo-Johnson transformation manually
#                           inverse_predictions <- predict(self$state$yj_trans, newdata = predictions, inverse = TRUE)
#                           #inverse_predictions <- yeojohnson.inv(predictions, self$state$yj_trans$lambda)
#                         }
#                         #inverse_predictions <- yeojohnson.inv(predictions, self$state$yj_trans$lambda)
#                         
#                         list(response = inverse_predictions)
#                       }
#                     )
# )

custom_gp2 <- R6Class("CustomGPRegr",
                      inherit = LearnerRegr,
                      public = list(
                        initialize = function() {
                          super$initialize(
                            id = "regr.custom_gp",
                            feature_types = c("integer", "numeric", "factor", "ordered"),
                            predict_types = c("response"),
                            param_set = ps(
                              sigma = p_dbl(lower = -3, upper = 3, trafo = function(x) 10^x)
                            )
                          )
                        }
                      ),
                      
                      private = list(
                        .train = function(task) {
                          # Convert categorical features to numeric (integer representation)
                          train_data <- task$data(cols = task$feature_names)
                          #train_data_numeric <- as.data.frame(lapply(train_data, as.integer))
                          train_labels <- task$truth()
                          #browser()
                          # Apply Yeo-Johnson transformation to target
                          # if (all(train_labels == 0)) {
                          #   # Skip Yeo-Johnson transformation if all values are zero
                          #   transformed_labels <- train_labels
                          # } else {
                          #   self$state$yj_trans <- bestNormalize::yeojohnson(train_labels)
                          #   transformed_labels <- self$state$yj_trans$x.t
                          # }
                          
                          # Choose a small constant to avoid log(0)
                          epsilon <- 1e-3  
                          transformed_labels <- log(train_labels + epsilon)
                          
                          # Center around zero
                          # Store the mean for later inversion
                          self$state$mean_train_labels <- mean(transformed_labels)
                          
                          # Center around zero
                          transformed_labels <- transformed_labels - self$state$mean_train_labels
                          #transformed_labels <- transformed_labels - mean_transformed_labels
                          
                          # Define custom Hamming kernel
                          #hamming_kernel <- function(x, y) {
                          #  exp(-sum(x != y) / self$param_set$values$sigma)
                          #}
                          
                          # Convert to kernel matrix
                          #kernel_matrix <- as.kernelMatrix(kernelMatrix(hamming_kernel, as.matrix(train_data)))
                          hamming_kernel <- function(sigma = 1) {
                            rval <- function(x, y) {
                              if (!is.vector(x) || !is.vector(y)) stop("x and y must be vectors!")
                              if (length(x) != length(y)) stop("x and y must have the same length!")
                              
                              # Compute Hamming distance
                              dist <- sum(x != y)/length(x)
                              
                              # Convert to similarity using an RBF-like formulation
                              similarity <- exp(-dist / sigma)
                              return(similarity)  # Must return a single scalar value
                            }
                            
                            class(rval) <- "kernel"  # Assign the "kernel" class
                            return(rval)
                          }
                          
                          # Create Hamming kernel with hyperparameter sigma
                          custom_hamming_kernel <- hamming_kernel(sigma = self$param_set$values$sigma)
                          
                          # # Train GP model using the custom kernel
                          # self$state$gp_model <- kernlab::gausspr(
                          #   x = as.matrix(train_data),
                          #   y = transformed_labels,
                          #   kernel = custom_hamming_kernel,
                          #   scaled = FALSE
                          # )
                          
                          # Save the mean of the transformed labels to use during prediction
                          #self$state$mean_transformed_labels <- mean_transformed_labels
                          self$state$log_epsilon <- epsilon 
                          
                          # Ensure safe model training
                          if (length(unique(transformed_labels)) > 1) {
                            self$state$gp_model <- tryCatch(
                              kernlab::gausspr(x = as.matrix(train_data),
                                      y = transformed_labels,
                                      kernel = custom_hamming_kernel,
                                      scaled = FALSE,
                                      var = 0.002  # Small jitter term
                                      ),
                              error = function(e) NULL  # If training fails, set model to NULL
                            )
                          } else {
                            self$state$gp_model <- NULL  # Avoid training on constant labels
                          }
                          
                          
                          invisible(self)
                        },
                        
                        .predict = function(task) {
                          # Convert test data to numeric
                          test_data <- task$data(cols = task$feature_names)
                          #test_data_numeric <- as.data.frame(lapply(test_data, as.integer))
                          #browser()
                          # Predict with trained GP model
                          # Check if the model is available
                          if (is.null(self$state$gp_model)) {
                            message("Warning: No trained GP model available, using fallback mean prediction.")
                            # Use the fallback model if the GP model is not available
                            predictions <- rep(0, nrow(test_data))
                          } else {
                            # Make predictions using the trained GP model
                            predictions <- kernlab::predict(self$state$gp_model, newdata = as.matrix(test_data))
                          }
                          #predictions <- predict(self$state$gp_model, newdata = as.matrix(test_data))
                          
                          # Add back the mean of transformed labels to the predictions
                          #predictions <- predictions + self$state$mean_transformed_labels
                          
                          # Apply inverse Yeo-Johnson transformation manually (not using predict)
                          # If the transformed labels were all zero, reverse the transformation manually (or skip if needed)
                          # if (all(task$truth() == 0)) {
                          #   inverse_predictions <- predictions  # No transformation needed for all-zero case
                          # } else {
                          #   # Apply inverse Yeo-Johnson transformation manually
                          #   inverse_predictions <- predict(self$state$yj_trans, newdata = predictions, inverse = TRUE)
                          #   #inverse_predictions <- yeojohnson.inv(predictions, self$state$yj_trans$lambda)
                          # }
                          #inverse_predictions <- yeojohnson.inv(predictions, self$state$yj_trans$lambda)
                          # Undo the mean subtraction
                          predictions <- predictions + self$state$mean_train_labels
                          
                          # Apply the inverse log transformation
                          predictions <- exp(predictions) - self$state$log_epsilon
                          # Ensure non-negative outputs
                          predictions <- pmax(predictions, 0)
                          
                          list(response = predictions)
                        }
                      )
)

# custom_gp <- R6::R6Class(
#   "CustomGPRegr",
#   inherit = LearnerRegr,
#   public = list(
#     initialize = function() {
#       super$initialize(
#         id = "regr.custom_gp",
#         feature_types = c("numeric", "integer"),
#         predict_types = c("response"),
#         param_set = ps(
#           sigma = p_dbl(0, 2, default = 0.1),  # Adjusted range
#           C = p_dbl(0, 100, default = 1)
#         )
#       )
#     }
#   ),
#   private = list(
#     .train = function(task) {
#       train_data <- task$data(cols = task$feature_names)
#       train_labels <- task$truth()
#       
#       # Box-Cox transformation for target
#       lambda <- 0.5  # Example: Square root transformation
#       transformed_labels <- (train_labels^lambda - 1) / lambda
#       
#       self$state <- list(
#         train_data = train_data,
#         train_labels = transformed_labels,
#         inverse_transform = function(x) (lambda * x + 1)^(1 / lambda)  # Inverse
#       )
#       invisible(self)
#     },
#     
#     .predict = function(task) {
#       train_data <- self$state$train_data
#       train_labels <- self$state$train_labels
#       test_data <- task$data(cols = task$feature_names)
#       sigma <- self$param_set$values$sigma
#       
#       hamming_kernel <- function(x, y, sigma) {
#         exp(-sum(x != y) / sigma)
#       }
#       
#       predictions <- apply(test_data, 1, function(instance) {
#         similarities <- apply(train_data, 1, function(train_instance) {
#           hamming_kernel(train_instance, instance, sigma)
#         })
#         if (sum(similarities) == 0) {
#           return(mean(train_labels))  # Default prediction if no neighbors found
#         }
#         sum(similarities * train_labels) / sum(similarities)
#       })
#       
#       # Apply the inverse transformation to predictions
#       predictions <- self$state$inverse_transform(predictions)
#       list(response = predictions)
#     }
#   )
# )

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
          k = p_int(lower = 1, upper = 20, default = 5)
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


library(kernlab)

# custom_svm <- R6::R6Class(
#   "CustomSVMRegr",
#   inherit = LearnerRegr,
#   public = list(
#     initialize = function() {
#       super$initialize(
#         id = "regr.custom_svm",
#         feature_types = c("numeric", "integer"),
#         predict_types = c("response"),
#         param_set = ps(
#           sigma = p_dbl(0.1, 10, default = 0.5),  # Controls kernel width (you can adjust this)
#           C = p_dbl(0.01, 100, default = 1),  # Regularization parameter
#           epsilon = p_dbl(0.001, 1, default = 0.1)  # Epsilon for epsilon-SVR
#         )
#       )
#     }
#   ),
#   private = list(
#     .train = function(task) {
#       #browser()
#       # Extract the training data and target labels from the task
#       train_data <- task$data(cols = task$feature_names)
#       train_labels <- task$truth()
#       
#       # Apply log1p transformation to the target (y) to handle zeros and positive values
#       #transformed_labels <- log1p(train_labels)
#       # Box-Cox transformation for target
#       lambda <- 0.5  # Example: Square root transformation
#       transformed_labels <- (train_labels^lambda - 1) / lambda
#       
#       self$state <- list(
#         train_data = train_data,
#         train_labels = transformed_labels,
#         inverse_transform = function(x) (lambda * x + 1)^(1 / lambda)  # Inverse
#       )
#       # Store the training data, transformed labels, and the inverse transformation function
#       # self$state <- list(
#       #   train_data = train_data,
#       #   train_labels = transformed_labels,
#       #   inverse_transform = function(x) expm1(x)  # Inverse of log1p is expm1
#       # )
#       invisible(self)
#     },
#     
#     .predict = function(task) {
#       #browser()
#       # Extract training data and labels
#       train_data <- self$state$train_data
#       train_labels <- self$state$train_labels
#       
#       # Extract the test data for prediction
#       test_data <- task$data(cols = task$feature_names)
#       
#       # Extract hyperparameters
#       sigma <- self$param_set$values$sigma
#       C <- self$param_set$values$C
#       epsilon <- self$param_set$values$epsilon
#       
#       # Define the Hamming kernel function
#       hamming_kernel <- function(x, y, sigma) {
#         exp(-sum(x != y) / sigma)  # Compute Hamming distance and apply the kernel transformation
#       }
#       
#       # Compute the kernel matrix for training data (square matrix)
#       kernel_matrix_train <- apply(train_data, 1, function(train_instance) {
#         apply(train_data, 1, function(other_train_instance) {
#           hamming_kernel(train_instance, other_train_instance, sigma)
#         })
#       })
#       kernel_matrix_train <- kernlab::as.kernelMatrix(matrix(kernel_matrix_train, nrow = nrow(train_data), byrow = TRUE))
#       
#       # Train the model with precomputed kernel matrix
#       model <- kernlab::ksvm(
#         x = kernel_matrix_train,
#         y = train_labels,
#         kernel = "matrix",
#         C = C,
#         epsilon = epsilon,
#         type = "eps-svr"
#       )
#       
#         
#         
#         
#       # Compute the kernel matrix between test and training data (rectangular matrix)
#       # kernel_matrix_test <- apply(test_data, 1, function(test_instance) {
#       #   apply(train_data, 1, function(train_instance) {
#       #     hamming_kernel(train_instance, test_instance, sigma)
#       #   })
#       # })
#       kernel_matrix_test <- t(apply(test_data, 1, function(test_instance) {
#         apply(train_data[model@SVindex, ], 1, function(sv_instance) {
#           hamming_kernel(sv_instance, test_instance, sigma)
#         })
#       }))
#       
#       
#       kernel_matrix_test <- kernlab::as.kernelMatrix(kernel_matrix_test)  
#       #browser()
#       # Predict using trained model and kernel matrix
#       predictions <- kernlab::predict(model, newdata = kernel_matrix_test)
#       
#       # Apply the inverse transformation (exp(x) - 1) to predictions
#       transformed_predictions <- self$state$inverse_transform(as.numeric(predictions))
#       
#       # Return the predictions (transformed back to the original scale)
#       list(response = transformed_predictions)
#     }
#   )
# )


custom_svm <- R6::R6Class(
  "CustomSVMRegr",
  inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.custom_svm",
        feature_types = c("numeric", "integer"),
        predict_types = c("response"),
        param_set = ps(
          sigma = p_dbl(0.001, 10, default = 0.5),  # Controls kernel width
          C = p_dbl(0.001, 100, default = 1),  # Regularization parameter
          epsilon = p_dbl(0.001, 1, default = 0.1)  # Epsilon for epsilon-SVR
        )
      )
    }
  ),
  
  private = list(
    
    .model = NULL,
    .train = function(task) {
      tryCatch({
        train_data <- task$data(cols = task$feature_names)
        train_labels <- task$truth()
        #browser()
        # Box-Cox transformation
        # lambda <- 0.5
        # transformed_labels <- (train_labels^lambda - 1) / lambda
        
        # Choose a small constant to avoid log(0)
        eps <- 1e-3  
        transformed_labels <- log(train_labels + eps)
        
        self$state <- list(
          train_data = train_data,
          train_labels = train_labels
          #inverse_transform = function(x) (lambda * x + 1)^(1 / lambda)
        )
        
        # Extract hyperparameters
        sigma <- self$param_set$values$sigma
        C <- self$param_set$values$C
        epsilon <- self$param_set$values$epsilon
        

        # Save the mean of the transformed labels to use during prediction
        #self$state$mean_transformed_labels <- mean_transformed_labels
        self$state$log_eps <- eps 
        
        
        # Hamming kernel function
        hamming_kernel <- function(x, y, sigma) {
          # Compute Hamming distance
          dist <- sum(x != y)/length(x)

          # Convert to similarity using an RBF-like formulation
          exp(-dist / sigma)
          #exp(-(sum(x != y)/length(x)) / sigma)
        }

        # Compute kernel matrix
        kernel_matrix_train <- apply(train_data, 1, function(train_instance) {
          apply(train_data, 1, function(other_train_instance) {
            hamming_kernel(train_instance, other_train_instance, sigma)
          })
        })
        
        kernel_matrix_train <- kernlab::as.kernelMatrix(matrix(kernel_matrix_train, nrow = nrow(train_data), byrow = TRUE))
        # Compute Hamming kernel matrix
        
        #kernel_matrix_train <- private$compute_hamming_kernel(X = train_data, sigma = sigma)
        
        # Train model
        model <- kernlab::ksvm(
          x = kernel_matrix_train,
          y = transformed_labels,
          kernel = "matrix",
          C = C,
          epsilon = epsilon,
          type = "eps-svr"
        )
        
        # Check if Support Vectors exist
        #if (length(model@SVindex) == 0) {
          #warning("No Support Vectors found. Retrying with C = 10*C and sigma = sigma/2.")
          #C <- C * 10  # Increase C to force margin constraints
          
          # kernel_matrix_train <- apply(train_data, 1, function(train_instance) {
          #   apply(train_data, 1, function(other_train_instance) {
          #     hamming_kernel(train_instance, other_train_instance, sigma)
          #   })
          # })
          # kernel_matrix_train <- kernlab::as.kernelMatrix(matrix(kernel_matrix_train, nrow = nrow(train_data), byrow = TRUE))
          # 
          
          # model <- kernlab::ksvm(
          #   x = train_data,
          #   y = transformed_labels,
          #   kernel = custom_hamming_kernel,
          #   C = C,
          #   epsilon = epsilon,
          #   type = "eps-svr"
          # )
          
          if (length(model@SVindex) == 0) {
            stop("No Support Vectors found after retry.")
          }
        #}
        
        private$.model <- model
        invisible(self)
        
      }, error = function(e) {
        warning("Training failed: ", e$message)
        private$.model <- NULL  # Ensure model is cleared if training fails
        invisible(self)
      })
    },
    
    
    
    .predict = function(task) {
      #tryCatch({
       # browser()
        
        
        train_data <- self$state$train_data
        test_data <- task$data(cols = task$feature_names)
        sigma <- self$param_set$values$sigma
        model <- private$.model  # Retrieve trained model
        
        
        if (is.null(private$.model)) {
          #warning("Prediction skipped: Model was not trained successfully.")
          #return(list(response = rep(NA, task$nrow)))
          warning("Prediction skipped: No valid model found.")
          # Return a placeholder response: a vector of zeros or the mean of training labels
          placeholder_response <- rep(mean(self$state$train_labels, na.rm = TRUE), nrow(test_data))
          return(list(response = placeholder_response))  # Ensure no NAs
        }
        
        
        
        # Ensure model has support vectors
        if (length(model@SVindex) == 0) {
          warning("Prediction skipped 0: No valid model found.")
          # Return a placeholder response: a vector of zeros or the mean of training labels
          placeholder_response <- rep(mean(self$state$train_labels, na.rm = TRUE), nrow(test_data))
          return(list(response = placeholder_response))  # Ensure no NAs
        }
        
        # Define Hamming kernel
        hamming_kernel <- function(x, y, sigma) {
          # Compute Hamming distance
          dist <- sum(x != y)/length(x)

          # Convert to similarity using an RBF-like formulation
          exp(-dist / sigma)
        }

        # Compute kernel matrix for prediction
        kernel_matrix_test <- t(apply(test_data, 1, function(test_instance) {
          apply(train_data[model@SVindex, ], 1, function(sv_instance) {
            hamming_kernel(sv_instance, test_instance, sigma)
          })
        }))
        
    
         kernel_matrix_test <- kernlab::as.kernelMatrix(kernel_matrix_test)
        
        #kernel_matrix_test <- self$compute_hamming_kernel(test_data, train_data, sigma = sigma)
        
        
        
        # Predict using trained model
        predictions <- kernlab::predict(model, newdata = kernel_matrix_test)
        #transformed_predictions <- self$state$inverse_transform(as.numeric(predictions))
        
        #if (is.null(self$state$gp_model)) {
        #  message("Warning: No trained GP model available, using fallback mean prediction.")
          # Use the fallback model if the GP model is not available
         # predictions <- rep(0, nrow(test_data))
        #} else {
          # Make predictions using the trained GP model
        #if (length(model@SVindex) > 0) {
        #  predictions <- kernlab::predict(model, newdata = as.matrix(test_data))
       # }
      
        # Apply the inverse log transformation
        predictions <- exp(predictions) - self$state$log_eps
        # Ensure non-negative outputs
        predictions <- pmax(predictions, 0)
        
        return(list(response = predictions))
       # }
        
     # }#, error = function(e) {
       # warning("Prediction failed: ", e$message)
       # list(response = rep(NA, task$nrow))  # Return NA predictions if an error occurs
      #}
     # )
    }
  )
)


##### analyses #################################################################
extract_result_data = function(bmr, learner_id, type){
  for(i in 1:length(bmr)){
    aggr = bmr[[i]]$aggregate()
    rr = aggr$resample_result[[learner_id]]
    tab = as.data.table(rr$prediction())[,c("row_ids", type), with = FALSE]
    if(i ==1){
      colnames(tab)[2] = paste0("V",i)
      result_data = tab
    }
    else{
      colnames(tab)[2] = paste0("V",i)
      result_data = merge(result_data, tab, by = "row_ids") 
    }
  }
  return(result_data)
}

compute_jsd_distance = function(data_response, data_truth){
  jsd_distance = c()
  for(i in 1:nrow(data_response)){
    jsd_distance[i] = JSD(rbind(unlist(data_response[i,2:ncol(data_response)]), unlist(data_truth[i,2:ncol(data_truth)])), unit = "log", est.prob = "empirical", test.na = FALSE)
  }
  return(jsd_distance)
}

create_data_jsd_plot = function(data_jsd, model){
  #data_jsd = data_jsd[order(data_jsd[[model]], decreasing = TRUE),]
  levels_specie = data_jsd$Specie
  data_jsd = gather(data_jsd, model, jsd, 2:ncol(data_jsd))
  colnames(data_jsd)[which(colnames(data_jsd) == "model")] = "Model"
  data_jsd$Specie = factor(data_jsd$Specie, levels = levels_specie)
  return(data_jsd)
}

compute_dist_matrix = function(bmr, type, learner_id){
  data = lapply(bmr, function(x){
    aggr = x$aggregate(msrs(c("regr.rmse")))
    rr = aggr$resample_result[[learner_id]]
    tab = as.data.table(rr$prediction())
    tab[,c("row_ids", type), with = FALSE]
  })
  
  data = data %>% reduce(full_join, by='row_ids')
  data = data[order(data$row_ids),]
  
  dist = matrix(nrow = nrow(data), ncol = nrow(data) ) 
  for(i in 1:nrow(data)){
    subset_wo_i = (1:nrow(data))[-i]
    for(j in subset_wo_i){
      dist[i,j] = JSD(rbind(unlist(data[i,2:ncol(data)]), unlist(data[j,2:ncol(data)])), unit = "log", est.prob = "empirical", test.na = FALSE)
    }
    dist[i,i] = 0
  }
  return(dist)
}

compute_projection_response = function(pcoa_result_original, dist_response){
  # Extract eigenvectors (Q_original) and eigenvalues (Lambda_original) for the original dataset
  Q_original <- pcoa_result_original$vectors  # Eigenvectors (principal coordinates)
  Lambda_original <- pcoa_result_original$values$Eigenvalues  # Eigenvalues (variance explained)
  
  # 2. Compute the Distance Matrix for the New Dataset
  dist_matrix_new <- dist_response
  
  # 3. Apply Double-Centering on the New Dataset's Distance Matrix (same as PCoA)
  dist_matrix_new_sq <- dist_matrix_new^2  # Square the distance matrix
  row_means_new <- rowMeans(dist_matrix_new_sq)  # Row means
  col_means_new <- colMeans(dist_matrix_new_sq)  # Column means
  overall_mean_new <- mean(dist_matrix_new_sq)  # Overall mean
  
  # Apply double-centering formula
  B_new <- -0.5 * (dist_matrix_new_sq - row_means_new - col_means_new + overall_mean_new)
  
  # 4. Project the New Data into the Original PCoA Space
  # Filter out small eigenvalues (close to zero) to avoid instability
  positive_eigenvalues <- Lambda_original > 1e-10  # Filter out small eigenvalues
  Q_filtered <- Q_original  # Eigenvectors for the components with non-zero eigenvalues
  Lambda_filtered <- Lambda_original[positive_eigenvalues]  # Eigenvalues for the non-zero components
  
  # Project the centered new distance matrix (B_new) into the same space
  projected_new <- B_new %*% Q_filtered %*% diag(1 / sqrt(Lambda_filtered))
  
  return(projected_new)
}
