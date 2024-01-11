
# _______________________________________________________________________   
#' @Title : Code ML for Anna Simoni courses
#' @author: Marc-André Buquet
#' @date: 7/01/2023
#  Mail: marc-andre.buquet@ensae.fr  
# _______________________________________________________________________       


## clean environment

rm(list = ls())
setwd("C:/Users/mabuq/Documents/M2_ENSAE/Machine learning/")


## Library

library(remotes)
library(minqa)
library(mvnfast)
library(HI) #install directly from the R archives as not available anymore from CRAN direct installation 
library(randomForest)
library(tree)
library(caret)
library(vip)
library(ranger)
library(h2o)
library(neuralnet)

## Source functions from external repositories in different files

source("C:/Users/mabuq/Documents/M2_ENSAE/VAR-LP/Projet/code_R/Fonctions_packages.R")


## Import data

data_Sala_i_Martin = rio::import("C:/Users/mabuq/Documents/M2_ENSAE/Machine learning/Code Sala_i_martin/ML Code/#1 Sala-I-Martin et. al (2004)/BACE_data.xls", na = ".") %>%
  filter(!is.na(GR6096))  # as too many values missing for Y as well as for regressors

dummy_variables <- c("BRIT","COLONY","EAST","EUROPE","LAAM","LANDLOCK","OIL","SAFRICA","SOCIALIST")


Y = data_Sala_i_Martin$GR6096 %>% as.numeric() # average growth rate between 1960 and 1996

X_dataframe = data_Sala_i_Martin %>% 
  select(-OBS, -COUNTRY,-CODE, -GR6096) # dummy variables will be reintegrated at the different regression steps

X = X_dataframe%>% # removal of the coding variables
  as.matrix() %>%
  as.numeric() %>%
  matrix(nrow = 123, ncol = 67) 

## Some data treatment

# Replace NA values with unconditional means (is OK as not too many missing values)
  
mut <- matrix(rep(colMeans(X, na.rm = TRUE), T), nrow = nrow(X), ncol = ncol(X), byrow = TRUE)

X[is.na(X)] <- mut[is.na(X)] 

## Data normalization on Y to find more meaningful results

X_normalised = transform_data(X, DEMEAN = 1)

X_normalised = as.data.frame(scale(X, scale=TRUE, center = TRUE))
Y_normalised = scale(Y, scale=TRUE, center = TRUE)


### Penalized linear regression -----

## Once the data transformation is done, we run different penalized linear regression to obtain different results for variable selections

# First, run a LASSO regression with thus the coefficient alpha of the R elastic net set up to 1

LASSO_regression = glmnet(X_normalised,Y_normalised,alpha=1)

plot(LASSO_regression, label = T)

print(LASSO_regression)

# By default and as expected when lambda = 0, ie we are back to the OLS case, we find no coefficients different from 0.
# Then, even by setting by default some small values above 1, we get some coefficients different from 0.

coef(LASSO_regression, s = 0.1)

plot(LASSO_regression,xvar="lambda")



# Then use a cross-validation method with the cv.glmnet function to get an idea of the optimal model to choose from

debug(cv.glmnet)
undebug(cv.glmnet)

cv.LASSO_regression <- cv.glmnet(as.matrix(X_normalised), matrix(Y_normalised), alpha = 1)

plot(cv.LASSO_regression)

# As noted in the R documentation for this package, the first vertical line returns the value of lambda such that we obtain the minimum cross-validated mean-square error,
# while the second line gives the value of lambda such that we have the most regularized model.

optimal_coefficient = coef(cv.LASSO_regression, s = "lambda.min")

beta_hat_optimal = c(optimal_coefficient[,1])[-1]

idx = c(1:67)[abs(beta_hat_optimal)>0.01]

colnames(X_dataframe)[idx]

plot(optimal_coefficient)


# This function returns the value of the penalized model at the optimum. We remark that out of the 67 variables initially considered to explain development,
# only about 23 are finally used in the optimal model

table_optimum = rbind(colnames(X_dataframe), optimal_coefficient@x[-1] )

colnames(X_dataframe)


# We run some consistency checks by using a 5-fold instead of the 10-fold used by default in the package

cv.LASSO_regression_consistency <- cv.glmnet(as.matrix(X_normalised), matrix(Y_normalised), alpha = 1, nfolds = 5)


# We then run regressions using elastic nets which create a pondaration between LASSO selection and Ridge selection to check consistency another way.



### Regression trees

X_data_tree = data_Sala_i_Martin %>% 
  select(-OBS, -COUNTRY,-CODE, -dummy_variables) %>% # dummy variables will be reintegrated at the different regression steps
  as.matrix() %>%
  as.numeric() %>%
  matrix(nrow = 123, ncol = 68 - length(dummy_variables)) 

cleaned_data_X = remove_outliers(X_data_tree)

mut <- matrix(rep(colMeans(cleaned_data_X, na.rm = TRUE), T), nrow = nrow(cleaned_data_X), ncol = ncol(cleaned_data_X), byrow = TRUE)

cleaned_data_X[is.na(cleaned_data_X)] <- mut[is.na(cleaned_data_X)] 
X_bind <- cbind(cleaned_data_X, data_Sala_i_Martin %>% select(all_of(dummy_variables)) %>% mutate_at(c("SOCIALIST"), as.numeric) %>% replace(.,is.na(.),0)) 
column_names = c(colnames(data_Sala_i_Martin %>% select(-OBS, -COUNTRY,-CODE, -dummy_variables)), dummy_variables)
colnames(X_bind) <- column_names

Index_zeros <- which(colSums(X_bind==0) == nrow(X_bind))

X1 = X_bind[,-Index_zeros] 

X1<-scale(X1,center=TRUE,scale=TRUE)

tree.fit<- tree(GR6096 ~. , data=as.data.frame(X1), split = "deviance")

tree.fit

summary(tree.fit)

plot(tree.fit)
text(tree.fit,cex=0.5)

# Possible to add cross-validation regarding the tree size to choose optimally

tree.fit_cv = cv.tree(tree.fit)
plot(tree.fit_cv$size, sqrt(tree.fit_cv$dev / nrow(X1)), type = "b",
     xlab = "Tree Size", ylab = "CV-RMSE")

# We keep a tree size of 6 has it has the lowest RMSE

tree.fit_pruning = prune.tree(tree.fit, best = 6)

plot(tree.fit_pruning)
text(tree.fit_pruning, pretty = 0)
title(main = "Pruned Regression Tree")

### Bagging procedure ----

# We start again by taking a baseline bagged model

bagged_tree_1 = ipred::bagging(formula = GR6096 ~ ., data = as.data.frame(X1) ,nbagg = 50, coob = TRUE)

bagged_tree_1

bagged_tree_2 = ipred::bagging(formula = GR6096 ~ ., data = as.data.frame(X1) ,nbagg = 200, coob = TRUE, trControl = trainControl(method = "cv", number = 10) )

# Produce the RMSE for a different number of trees

RMSE_array = array(NA, dim = 300)

for (i in 20:300) { # choice of this starting point as below this treshold, bagging function considered model was too small
  bagged_tree = ipred::bagging(formula = GR6096 ~ ., data = as.data.frame(X1) ,nbagg = i, coob = TRUE)
  OOB_RMSE = bagged_tree$err
  RMSE_array[i] = OOB_RMSE
}

RMSE_table = RMSE_array[-!is.na(RMSE_array)]



cv.bagged_tree <- caret::train(
  GR6096 ~ . ,
  data = as.data.frame(X1),
  method = "treebag",
  trControl = trainControl(method = "cv", number = 10),
  nbagg = 200
)

cv.bagged_tree_2 <- caret::train(
  GR6096 ~ . ,
  data = as.data.frame(X1),
  method = "treebag",
  trControl = trainControl(method = "cv", number = 5),
  nbagg = 200
)

## For interpretability reason, want to be able to understand the features contributing most to the bagged trees procedure

interpretation_table = vip::vip(cv.bagged_tree, num_features = 20)

interpretation_table #useful to obtain variable importance (The features with the largest average decrease in SSE (for regression) are considered most important according to )

### Random forest -----

# As before start again from the cleaned data used for trees and compute the random forest regression
# As seen in class, the number of features used in the random forest model is floor(p/3) so we first compute it that way.

random_forest_1 = ranger::ranger(GR6096 ~ ., 
                                 data = as.data.frame(X1),
                                 mtry = floor(dim(X1)[2] / 3),
                                 respect.unordered.factors = "order",
                                 importance = "impurity",
                                 seed = 42)

rmse <- sqrt(random_forest_1$prediction.error)

test <- vip::vip(random_forest_1, num_features = 20, bar = FALSE)
test

rf.fit <- randomForest(GR6096 ~ ., data = as.data.frame(X1),ntree=1000,mtry=20,importance=TRUE)
rf.fit
plot(rf.fit)

# From the baseline model, we want to tune some hyperparameters to have a more consistent model.
# We rely on the package h2o and on the implementation done in the following resource : https://bradleyboehmke.github.io/HOML/random-forest.html

h2o.no_progress()
h2o.init(max_mem_size = "6g")
train_h2o <- as.h2o(as.data.frame(X1))
response <- "GR6096"
predictors <- setdiff(colnames(as.data.frame(X1)), response)

hyper_grid <- list(
  mtries = floor(dim(X1)[2] * c(.05, .15, .25, .333, .4)),
  min_rows = c(1, 3, 5, 10),
  max_depth = c(10, 20, 30),
  sample_rate = c(.55, .632, .70, .80)
)

search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
  stopping_rounds = 10,         # over the last 10 models
  max_runtime_secs = 300     # or stop search after 5 min.
)

random_grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_random_grid",
  x = predictors, 
  y = response, 
  training_frame = train_h2o,
  hyper_params = hyper_grid,
  ntrees = dim(X1)[2] * 10,
  seed = 123,
  stopping_metric = "RMSE",   
  stopping_rounds = 10,           # stop if last 10 trees added 
  stopping_tolerance = 0.005,     # don't improve RMSE by 0.5%
  search_criteria = search_criteria
)

random_grid_perf <- h2o.getGrid(
  grid_id = "rf_random_grid", 
  sort_by = "mse", 
  decreasing = FALSE
)

random_grid_perf


tuned_rf_model <- ranger(
  formula = GR6096 ~ ., 
  data = as.data.frame(X1), 
  num.trees = 2000,
  mtry = 24,
  min.node.size = 1,
  sample.fraction = .80,
  replace = FALSE,
  importance = "impurity",
  respect.unordered.factors = "order",
  verbose = FALSE,
  seed  = 123
)

variable_importance_rf_tuned <- vip::vip(tuned_rf_model, num_features = 25, bar = FALSE)
variable_importance_rf_tuned


### Neural networks ------


# Fit a first NN as a test with only 1 layer and 2 neurons

NN_1 = neuralnet(GR6096~.,data=as.data.frame(X1),hidden=c(2),algorithm="rprop+")
NN_1
plot(NN_1, rep = "best")

fitted.values_1 <- as.numeric(NN_1$net.result[[1]])
error_1 = (Y - fitted.values)^2
rmse_1 = sqrt(mean(error))
rmse_1


NN_2 = neuralnet(GR6096~.,data=as.data.frame(X1),hidden=c(4,2),algorithm="rprop+")
NN_2
plot(NN_2)

fitted.values_2 <- as.numeric(NN_2$net.result[[1]])
error_2 = (Y - fitted.values)^2
rmse_2 = sqrt(mean(error))
rmse_2



