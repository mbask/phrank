#' Extract response variable (y) from a formula
#'
#' @param ml_formula 
#'
#' @return a character vector
#' @export
#'
#' @examples
get_response_variable <- function(ml_formula) {
  formula_terms <- terms(ml_formula)
  resp_var_name <- attr(formula_terms, "variables")[[attr(formula_terms, "response") + 1]]
  as.character(resp_var_name)
}

#' Wrapper around caret::createDataPartition
#'
#' Pre-defined arguments:
#' p     = .7, 
#' list  = FALSE, 
#' times = 1
#'
#' @param dataset 
#'
#' @return A list or matrix of row position integers corresponding to the training data
#' @export
#' @importFrom caret createDataPartition
#'
#' @examples
create_data_partition <- function(dataset) {
  caret::createDataPartition(
    dataset$genus_id, 
    p     = .7, 
    list  = FALSE, 
    times = 1)
}

#' Subset dataset by extracting train_index rows
#'
#' @param train_index 
#' @param dataset 
#'
#' @return filtered dataset
#' @export
#'
#' @examples
filter_train_set <- function(train_index, dataset) {
  dataset[train_index, ]
}

#' Subset dataset by filtering out train_index rows
#'
#' @param train_index 
#' @param dataset 
#'
#' @return filtered dataset
#' @export
#'
#' @examples
filter_test_set <- function(train_index, dataset) {
  dataset[-train_index, ]
}

#' Wrapper around e1071::tune
#'
#' Pre-defined arguments:
#' method  = svm
#' ranges  = list(
#'   cost    = 2^(0:14),
#'   gamma   = 10^(-8:0))
#' 
#' @param train_set 
#' @param svm_formula 
#'
#' @return an object of class "tune"
#' @export
#' @importFrom e1071 tune
#'
#' @examples
# tune_svm_model <- function(train_set, svm_formula) {
#   tune(
#     method  = svm,
#     train.x = svm_formula, 
#     ranges  = list(
#       cost    = 2^(0:14),
#       gamma   = 10^(-8:0)), 
#     data    = train_set)
# }
tune_svm_model <- function(train_set, svm_formula) {
  tune(
    method  = svm,
    train.x = svm_formula, 
    ranges  = list(
      #cost    = 2^(0:14),
      gamma   = seq(0, 1, by = 0.01)),
    kernel = "radial",
    data    = train_set)
}

#' Wrapper around e1071::svm
#'
#' @param train_set 
#' @param svm_tune_model an object of class "tune"
#' @param svm_formula 
#'
#' @return An object of class "svm"
#' @export
#' @importFrom e1071 svm
#'
#' @examples
# perform_svm_on_train_set <- function(train_set, svm_tune_model, svm_formula) {
#   svm(
#     formula    = svm_formula, 
#     cost       = svm_tune_model$best.parameters$cost, 
#     gamma      = svm_tune_model$best.parameters$gamma,
#     data       = train_set)
# }

#' Wrapper around e1071::predict.svm
#'
#' @param test_set 
#' @param svm_model 
#' @param response_var_name a character vector of the response variable of the svm_model
#'
#' @return A vector of predicted values
#' @export
#' @importFrom e1071 predict.svm
#'
#' @examples
predict_svm_on_test_set <- function(test_set, svm_model, response_var_name) {
  predict(
    svm_model$best.model,
    test_set[, colnames(test_set) != response_var_name])
}

#' Cross tabulation between the 2 sets of prediction and real factors 
#'
#' It's a wrapper of base::table
#' @param prediction_set 
#' @param real_set 
#' @param response_var_name 
#'
#' @return
#' @export
#'
#' @examples
get_confusion_matrix <- function(prediction_set, real_set, response_var_name) {
  true_set <- unlist(real_set[, response_var_name])
  table(
    pred = prediction_set, 
    true = true_set)
}

#' Get accuracy of a classification model from its confusion matrix
#' 
#' Accuracy is defined as the rate of matching row & columns over the total count of elements
#'
#' @param confusion_matrix 
#'
#' @return
#' @export
#'
#' @examples
get_model_accuracy <- function(confusion_matrix) {
  sum(diag(confusion_matrix) / sum(confusion_matrix))
}
get_model_accuracy.randomForest <- function(model) {
  stopifnot("randomForest" %in% class(model))
  get_model_accuracy(model$confusion)
}
get_model_accuracy.table <- function(model) {
  stopifnot("table" %in% class(model))
  get_model_accuracy(as.matrix(model))
}

#' Optimal tree size for a classification random forest
#'
#' @param m_formula 
#' @param train_set 
#'
#' @return
#' @export
#' @importFrom randomForest randomForest
#'
#' @examples
tune_rf_model <- function(train_set, ml_formula) {
  foo <- randomForest::randomForest(
    formula = ml_formula, 
    data    = train_set)

  which.min(foo$err.rate[,1])
}

#' Title
#'
#' @param test_set 
#' @param tree_size 
#' @param ml_formula 
#'
#' @return
#' @export
#' @importFrom randomForest randomForest
#'
#' @examples
perform_rf_on_test_set <- function(test_set, tree_size, ml_formula) {
  randomForest(
    formula = ml_l$ml_formula, 
    data    = ml_l$train_set_l$EVI, ntree = tree_size)
}
