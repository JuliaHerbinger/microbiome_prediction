library(philentropy)
library(tidyr)
library(tidyverse)

##### modeling #################################################################

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
          k = p_int(lower = 1, upper = 12, default = 5)
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
  data_jsd = data_jsd[order(data_jsd[[model]], decreasing = TRUE),]
  levels_specie = data_jsd$specie
  data_jsd = gather(data_jsd, model, jsd, 2:ncol(data_jsd))
  data_jsd$specie = factor(data_jsd$specie, levels = levels_specie)
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
