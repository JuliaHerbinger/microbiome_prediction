library(mlr3verse)
library(mlr3learners)
library(mlr3extralearners)

library(R6)
library(mlr3)
library(mlr3tuning)
library(paradox)
library(data.table)

# Define Hamming Kernel function with scaling parameter
hamming_kernel <- function(data, sigma) {
  # Compute Hamming distance matrix
  distance_matrix <- as.matrix(dist(data, method = "binary"))
  # Apply Gaussian-like transformation with Hamming distance
  kernel_matrix <- exp(-sigma * distance_matrix)
  return(kernel_matrix)
}

# Define Custom Gaussian Process Learner for Regression
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
          sigma = p_dbl(0, 10, default = 1),
          C = p_dbl(0, 100, default = 1)
        )
      )
    }
  ),
  private = list(
    .train = function(task) {
      # Extract training data and labels
      self$state <- list(  # Store only the necessary training data
        train_data = task$data(cols = task$feature_names),
        #train_labels = log1p(task$truth())-mean(log1p(task$truth()))
        train_labels = task$truth()
      )
      #self$state$train_data <- task$data(cols = task$feature_names)
      #self$state$train_labels <- log1p(task$truth())-mean(log1p(task$truth()))
      invisible(self)
    },
    
    .predict = function(task) {
      # Ensure the training data exists
      if (is.null(self$state$train_data) || is.null(self$state$train_labels)) {
        stop("The model has not been trained yet.")
      }
      
      # Extract test data for prediction
      test_data <- task$data(cols = task$feature_names)
      
      # Define the Hamming kernel function
      hamming_kernel <- function(x, y, sigma) {
        exp(-sum(x != y) / sigma)
      }
      
      # Predict for each test instance
      predictions <- apply(test_data, 1, function(instance) {
        # Compute similarities using the Hamming kernel
        similarities <- apply(self$state$train_data, 1, function(train_instance) {
          hamming_kernel(train_instance, instance, self$param_set$values$sigma)
        })
        
        # Handle cases where the sum of similarities is zero
        if (sum(similarities) == 0) {
          # Return the mean of the training labels as a fallback
          return(mean(self$state$train_labels))
        }
        
        # Compute weighted mean prediction
        weighted_mean <- sum(similarities * self$state$train_labels) / sum(similarities)
        return(weighted_mean)
      })
      
      # Ensure predictions contain no missing values
      if (any(is.na(predictions))) {
        stop("Predictions contain NA values. Please check the input data or kernel.")
      }
      
      list(response = predictions)
    }
  )
)

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

# Custom KNN Learner (unchanged, using Hamming Distance)
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
          k = p_int(1, 50, default = 5)
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
      
      # Predict for each test instance
      predictions <- apply(test_data, 1, function(instance) {
        
        # Compute Hamming distances
        distances <- rowSums(self$state$train_data != instance)
        
        # Handle cases where there are fewer training instances than k
        k_actual <- min(k, nrow(self$state$train_data))
        
        # Find the k-nearest neighbors
        nearest <- order(distances)[1:k_actual]
        
        # Return the mean of their responses
        mean(self$state$train_labels[nearest])
      })
      
      list(response = predictions)
    }
  )
)




# Define tuning and search spaces
search_space_knn <- ps(
  k = p_int(1, 12) 
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
#data = cbind(X, "y" = Y[,1])

#task <- as_task_regr(data, target = "y")
#resample(task, custom_knn$new(), resampling = outer_resampling)

# Define AutoTuner for KNN
knn_tuner <- AutoTuner$new(
  learner = custom_knn$new(),
  resampling = inner_resampling,
  measure = msr("regr.rmse"),
  search_space = search_space_knn,
  terminator = trm("none"),
  tuner = tnr("grid_search",resolution = 6)
)

# Define AutoTuner for GP
gp_tuner <- AutoTuner$new(
  learner = custom_gp$new(),
  resampling = inner_resampling,
  measure = msr("regr.rmse"),
  search_space = search_space_gp,
  terminator = trm("evals", n_evals = 40),
  tuner = tnr("random_search")
)

library(future.apply)
options(future.globals.maxSize = 1e9)
#plan("future::multisession") ## => parallelize on your local computer
plan(multisession, workers = 10)
bmr <- future_lapply(Y[,1:10], function(y) {
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



save(bmr, file = "bmr_new.RData")
load("bmr_new.Rdata")
# 67, 143, 22, 349, 245, 328,117,156,74
aggr = bmr[[328]]$aggregate(msrs(c("regr.rsq")))
aggr
rr = aggr$resample_result[[2]]
as.data.table(rr$prediction())
tab = as.data.table(rr$prediction())
cor(tab$truth, tab$response)

#dat = merge(X, Y, by = "row.names")
library(philentropy)
jsd = JSD(rbind(tab$truth, tab$response), est.prob = "empirical", test.na = FALSE, unit = "log")


knn = lapply(bmr, function(x){
  aggr = x$aggregate(msrs(c("regr.rmse")))
  rr = aggr$resample_result[[1]]
  tab = as.data.table(rr$prediction())
  if(any(is.na(tab$response))) tab = tab[-which(is.na(tab$response)),]
  if(any(is.na(tab$truth))) tab = tab[-which(is.na(tab$truth)),]
  #print(JSD(rbind(tab$truth, tab$response), est.prob = "empirical"))
  JSD(rbind(tab$truth, tab$response), unit = "log", est.prob = "empirical", test.na = FALSE)
})

for(i in 1:length(bmr)){
  aggr = bmr[[i]]$aggregate(msrs(c("regr.rmse")))
  rr = aggr$resample_result[[2]]
  tab = as.data.table(rr$prediction())
  if(i ==1){
    colnames(tab)[3] = paste0("V",i)
    dat = tab[, c(1,3)]
  }
  else{
    colnames(tab)[3] = paste0("V",i)
    dat = merge(dat, tab[, c(1,3)], by = "row_ids") 
  }
}

for(i in 1:length(bmr)){
  aggr = bmr[[i]]$aggregate(msrs(c("regr.rmse")))
  rr = aggr$resample_result[[2]]
  tab = as.data.table(rr$prediction())
  if(i ==1){
    colnames(tab)[2] = paste0("V",i)
    dat_truth = tab[, c(1,2)]
  }
  else{
    colnames(tab)[2] = paste0("V",i)
    dat_truth = merge(dat_truth, tab[, c(1,2)], by = "row_ids") 
  }
}

library(philentropy)
jsd_specie2 = c()
for(i in 1:nrow(dat)){
  jsd_specie2[i] = JSD(rbind(unlist(dat[i,2:ncol(dat)]), unlist(dat_truth[i,2:ncol(dat_truth)])), unit = "log", est.prob = "empirical", test.na = FALSE)
}




ll = as.data.frame(cbind(unlist(knn), 1:length(knn)))
ll[order(ll$V1),]

library(tidyr)
#data_jsd = data.frame("knn" = jsd_specie1, "gp" = jsd_specie2, "specie" = row.names(X))
data_jsd = data.frame( "gp" = jsd_specie2, "specie" = row.names(X))
data_jsd = data_jsd[order(data_jsd$gp, decreasing = TRUE),]
levels_specie = data_jsd$specie
#data_jsd = gather(data_jsd, model, jsd, knn:gp)
data_jsd = gather(data_jsd, model, jsd, gp)
data_jsd$specie = factor(data_jsd$specie, levels = levels_specie)

#p1 = ggplot(data = data_jsd, aes(x = specie, y = jsd)) + geom_line(aes(group = model, col = model)) + theme_bw() + ylim(0,0.8) +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))

p1 = ggplot(data = data_jsd, aes(x = specie, y = jsd)) + geom_line(aes(group = model, col = model)) + theme_bw() + ylim(0,0.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))

ggsave("jsd_by_species.png", p1, width = 7, height = 4)
mean(jsd_specie2)
sd(jsd_specie2)

knn_pred = lapply(bmr, function(x){
  aggr = x$aggregate(msrs(c("regr.rmse")))
  rr = aggr$resample_result[[2]]
  tab = as.data.table(rr$prediction())
  tab[,c("row_ids", "response")]
})

library(tidyverse)
knn_pred = knn_pred %>% reduce(full_join, by='row_ids')
knn_pred = knn_pred[order(knn_pred$row_ids),]

dist = matrix(nrow = nrow(knn_pred), ncol = nrow(knn_pred) ) 
for(i in 1:nrow(knn_pred)){
  subset_wo_i = (1:nrow(knn_pred))[-i]
  for(j in subset_wo_i){
    dist[i,j] = JSD(rbind(unlist(knn_pred[i,2:ncol(knn_pred)]), unlist(knn_pred[j,2:ncol(knn_pred)])), unit = "log", est.prob = "empirical", test.na = FALSE)
  }
  dist[i,i] = 0
}

# truth
knn_true = lapply(bmr, function(x){
  aggr = x$aggregate(msrs(c("regr.rmse")))
  rr = aggr$resample_result[[2]]
  tab = as.data.table(rr$prediction())
  tab[,c("row_ids", "truth")]
})

library(tidyverse)
knn_true = knn_true %>% reduce(full_join, by='row_ids')
knn_true = knn_true[order(knn_true$row_ids),]

dist_true = matrix(nrow = nrow(knn_true), ncol = nrow(knn_true) ) 
for(i in 1:nrow(knn_true)){
  subset_wo_i = (1:nrow(knn_true))[-i]
  for(j in subset_wo_i){
    dist_true[i,j] = JSD(rbind(unlist(knn_true[i,2:ncol(knn_true)]), unlist(knn_true[j,2:ncol(knn_true)])), unit = "log", est.prob = "empirical", test.na = FALSE)
  }
  dist_true[i,i] = 0
}


library(patchwork)
Brassica_matches = c("Alliaria", "Arabidopsis", "Barbarea", "Brassica", "Capsella", "Cardamine", "Erophila", "Eruca", "Raphanus", "Sinapis")
Brassica <- unique (grep(paste(Brassica_matches,collapse="|"), row.names(X), value=TRUE))

Poaceae_matches = c("Avena", "Elymus", "Festuca", "Lolium", "Oryza")
Poaceae <- unique (grep(paste(Poaceae_matches,collapse="|"), row.names(X), value=TRUE))

Fabaceae_matches = c("Medicago", "Phaseolus", "Pisum", "Vicia")
Fabaceae <- unique (grep(paste(Fabaceae_matches,collapse="|"), row.names(X), value=TRUE))

colnames(dist) = colnames(dist_true) = rownames(dist) = rownames(dist_true) = rownames(X)
dist = dist[rownames(dist) %in% c(Brassica, Poaceae, Fabaceae), colnames(dist) %in% c(Brassica, Poaceae, Fabaceae)]
dist_true = dist_true[rownames(dist_true) %in% c(Brassica, Poaceae, Fabaceae), colnames(dist_true) %in% c(Brassica, Poaceae, Fabaceae)]
pc_pred = pcoa(dist )
pc_true = pcoa(dist_true)
data_pc = as.data.frame(rbind(pc_pred$vectors[,1:2], pc_true$vectors[,1:2]))
data_pc$PredVsTrue = c(rep("Pred", nrow(dist)), rep("True", nrow(dist)))
data_pc$Species = rep(row.names(dist), 2)
data_pc_sub = data_pc[data_pc$Species %in% c(Brassica, Poaceae, Fabaceae),]
data_pc_sub$Family = NA
data_pc_sub$Family[data_pc_sub$Species %in% Brassica] = "Brassica"
data_pc_sub$Family[data_pc_sub$Species %in% Poaceae] = "Poaceae"
data_pc_sub$Family[data_pc_sub$Species %in% Fabaceae] = "Fabaceae"

p2 = ggplot(data_pc_sub, aes(x = Axis.1, y = Axis.2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted") +
  ylab(paste("PC2:", round(pc_pred$values$Rel_corr_eig[2]*100,0), "%")) +
  xlab(paste("PC1:", round(pc_pred$values$Rel_corr_eig[1]*100,0), "%")) +
  theme_bw()+ labs(shape = NULL)

ggsave("pcoa_gp.png", p2, width = 5.2, height = 4)



wcmdscale(d = dist, eig = TRUE)
pc_pred = wcmdscale(d = dist, eig = TRUE)
pc_true = wcmdscale(d = dist_true, eig = TRUE)
data_pc = as.data.frame(rbind(pc_pred$points[,1:2], pc_true$points[,1:2]))

ggplot(data_pc_sub, aes(x = Dim1, y = Dim2)) + geom_point(aes(col = Family, shape = PredVsTrue)) +geom_line(aes(group = Species), col = "lightgrey", lty = "dotted")
