## Charger packages

#library(tidyverse) # manipulate data
library(ggplot2) # visualize data
library(dplyr)
library(janitor) # clean data
library(viridisLite) # nice color chart
library(lmtest) # linear regression package
library(stargazer) # export regression to a nice format for inclusion in word document
library(readxl)
library(plyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggfortify)
library(AER)
library(recipes)
library(performance)
library(leaps)
library(ivtools)
library(parallel)
library(foreach)
library(doParallel)
library(R.matlab)
library(matlab)
library(svars)
library(plotly)
library(rio)
library(glue)
library(ggformula)
library(cowplot)
library(pracma)
library(Matrix)

library("pacman")
#pacman::p_load(tidyverse,ggplot2,dplyr,janitor,viridisLite,lmtest,stargazer,readxl,plyr,lubridate,
#               tidyr,purrr,ggfortify,AER,recipes,performance,leaps,ivtools,parallel,foreach,doParallel,
#               R.matlab,matlab,plotly,rio,glue,ggformula)


## Charger fonctions


create_lag_vars <- function(data, var_names, num_lags) {
  for (var in var_names) {
    for (i in 1:num_lags) {
      lagged_var <- paste0(var, "_lag", i)
      data[[lagged_var]] <- dplyr::lag(data[[var]], i)
    }
  }
  return(data)
}

create_diff_vars <- function(data, vars, diff_order = 1) {
  new_names <- paste(vars, "_diff", diff_order, sep = "")
  diff_vars <- lapply(data[, vars], diff, differences = diff_order, na.rm = T)
  names(diff_vars) <- new_names
  diff_vars <- as.data.frame(diff_vars)
  data <- data[-(1:diff_order),]
  new_data <- cbind(data, diff_vars)
  return(new_data)
  #new_data <- cbind(data[-length(data), ], diff_vars)
  #return(new_data)
}

create_log_vars <- function(data, vars, base = exp(1), scale_factor = 100) {
  
  new_names <- paste(vars, "_log", sep = "")
  log_vars <- lapply(data[, vars], function(x) log(x, base = base) * scale_factor)
  #log_vars <- lapply(data[, vars], log, base = base)
  names(log_vars) <- new_names
  log_vars <- log_vars
  new_data <- cbind(data , log_vars)
  return(new_data)
}

iv_reg_all <- function(data, instr_var, ind_var, end_var, dep_var, nb_ind, nb_instr = 1, sig_level = 0.05 ) {
  
  # Generate all possible combinations of instrumental and independent variables
  iv_combos <- combn(instr_var, nb_instr, simplify = FALSE)
  ind_combos <- combn(ind_var, nb_ind, simplify = FALSE)
  
  # Define a function to run an instrumental variables regression with a given combination of variables
  run_iv_reg <- function(iv, ind, sig_level = 0.05) {
    
    # Create the model formula
    model_formula <- as.formula(paste0(dep_var, " ~ ", paste0(end_var, collapse = "+"), "+", 
                                       paste0(ind, collapse = "+"), "|", 
                                       paste0(iv, collapse = "+"), "+",
                                       paste0(ind, collapse = "+")))
    
    # Run the instrumental variables regression
    current_model <- ivreg(model_formula, data = data)
    
    # Extract the model summary
    model_summary <- broom::tidy(current_model)
    
    # Check if the parameter estimate for the endogenous variable is significant
    end_var_t_value <- model_summary[model_summary$term == end_var, "estimate"] / model_summary[model_summary$term == end_var, "std.error"]
    end_var_t_value <- as.numeric(end_var_t_value)
    end_var_p_value <- 2 * pt(abs(end_var_t_value), df = current_model$df.residual, lower.tail = FALSE)
    end_var_significant <- end_var_p_value < sig_level
    
    # Check if AIC is finite
    aic_finite <- is.finite(AIC(current_model))
    
    # Return a list containing the results if the criteria are met
    if (end_var_significant & aic_finite) {
      return(list(iv = iv, ind = ind, model_summary = model_summary, AIC = AIC(current_model)))
    } else {
      return(NULL)
    }
  }
  
  # Loop over each combination of instrumental and independent variables and run the regression
  result_list <- parallel::mclapply(iv_combos, function(iv) {
    parallel::mclapply(ind_combos, function(ind) {
      run_iv_reg(iv, ind, sig_level)
    })
  })
  
  result_list <- unlist(result_list, recursive = FALSE)
  result_list <- result_list[!sapply(result_list, is.null)]
  result_list <- result_list[order(sapply(result_list, function(x) x$AIC))]
  
  return(result_list[1:min(length(result_list), 10)])
}



create_seasonal_diff <- function(data, columns, lag, num_diff = 1) {
  # Loop through the specified columns
  for (col in columns) {
    # Perform seasonal differentiation for the specified number of times
    for (i in 1:num_diff) {
      # Perform lag() and replace the first lag rows with NA
      data[[col]] <- lag(data[[col]], n = lag)
      data[[col]][1:lag] <- NA
      data[[col]] <- as.numeric(data[[col]])
    }
  }

  # Return the modified dataframe
  return(data)
}


perform_seasonal_diff <- function(data, columns, lag, num_diff = 1) {
  # Loop through the specified columns
  for (col in columns) {
    # Perform seasonal differentiation for the specified number of times
    for (i in 1:num_diff) {
      # Create new column name for the differenced values
      new_col <- paste0(col, "_seasonal_diff_", i)
      
      # Perform lag() and replace the first lag rows with NA
      data[[new_col]] <- lag(data[[col]], n = lag)
      data[[new_col]][1:lag] <- NA
      data[[new_col]] <- as.numeric(data[[new_col]])
      data[[new_col]] <- data[[col]] - data[[new_col]]
      #data <- data[-(1:lag),]
    }
    
    
  }
  
  # Return the modified dataframe
  return(data)
}

month_span_data <- function(start_month, start_year, end_month, end_year) {
  time_span = lubridate::interval(start = ym(as.numeric(paste(start_year,as.character(start_month), sep = ""))), end = ym(as.numeric(paste(end_year, end_month, sep = "")))) 
  nb_months = round(time_span@.Data / (86400 * (365.25/12))) 
  return(nb_months)}

day_span_data <- function(start_day, start_month, start_year, end_day, end_month, end_year){
  time_span = lubridate::interval(start = ymd(as.numeric(paste(start_year,start_month, start_day , sep = ""))), end = ymd(as.numeric(paste(end_year,end_month,end_day, sep = "")))) 
  nb_days = time_span@.Data / 86400
  return(nb_days)
}

Data_retrieval = function(data,nb_var,nb_shocks, horizon){
  
  IRFs_representation = as.list(numeric(length = nb_var*nb_shocks))
  dim(IRFs_representation) = c(nb_var, nb_shocks)
  
  for (var in 1:nb_var) {
    for (shock in 1:nb_shocks) {
      IRFs_representation[[var,shock]] = data[1:horizon,var,shock]
    }
  }
  
 # IRFs_representation = t(IRFs_representation) # comme cela on a bien IRFs_representation[[shock,var]]

  return(IRFs_representation)
 }
  


#_#######################################################

Plot_figure = function(data, horizon, data_lower = NULL, data_upper = NULL, type_of_representation = "IRF", nb_shocks , nb_var  ){
  
 
   
  IRFs_representation = as.list(numeric(length = nb_var*nb_shocks))
  dim(IRFs_representation) = c(nb_var, nb_shocks)
  
  

    
    # if (!is.null(data_lower) | !is.null(data_upper)) 
    # {stop("please provide both lower and upper bounds if applicable")}
    # 
    if (is.null(data_lower) & is.null(data_upper)) 
    {for (shock in 1:nb_shocks) {
      for (var in 1:nb_var) {
        IRFs_representation[[var, shock]] = ggplot() + 
          theme_bw() +
          geom_line(data = as.data.frame(data[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs)) + 
          labs(y = as.character(shock_names[shock]) , x = as.character(unlist(SVAR_NSR$varNames[1,var])))
        
      }
    }
    }
  
    if (!is.null(data_lower) & !is.null(data_upper)){
             
      data_combined = as.list(numeric(length = nb_var*nb_shocks))
      dim(data_combined) = c(nb_var, nb_shocks)
      
      
      for (shock in 1:nb_shocks) {
        for (var in 1:nb_var) {
          if (type_of_representation == "IRF"){
          data_combined[[var,shock]] = cbind(data_lower[[var,shock]], data_upper[[var,shock]])}
          if (type_of_representation == "FEVD"){
            data_combined[[var,shock]] = cbind(data_lower[[var,shock]], data_upper[[var,shock]])}
          
          
                              }
                                 }
      
             
             for (shock in 1:nb_shocks) {
              for (var in 1:nb_var) {
                
               temp_data = ggplot() + 
                 geom_spline(data = as.data.frame(data_lower[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotted", spar = 0.675) + 
                 geom_spline(data = as.data.frame(data_upper[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotted", spar = 0.675) 
               
               temp_data_1 <- ggplot_build(temp_data)
               
               temp_data_2 <- data.frame(x = temp_data_1$data[[1]]$x,
                                 ymin = temp_data_1$data[[1]]$y,
                                 ymax = temp_data_1$data[[2]]$y) 
               
              
               IRFs_representation[[var, shock]] = ggplot() + 
                                                   theme_bw() +
                                                   geom_spline(data = as.data.frame(data_lower[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotted", color = "black", spar = Pspar) + 
                                                   geom_spline(data = as.data.frame(data_upper[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotted", color = "black", spar = Pspar) + 
                                                   labs(y = as.character(shock_names2[shock]) , x = as.character( var_names[var]   )) +
                                                   theme(axis.text.x= element_text(size=SL-2),axis.text.y= element_text(size=SL-2))+
                                                   theme(axis.title.x = element_text(size=SL+3),axis.title.y= element_text(size=SL+3))+
                                                  #geom_ribbon(data = as.data.frame(data_combined[var, shock]) %>% set_names(., "Lower_bound","Upper_bound"), aes(x=1:horizon, ymax = predict(loess(Upper_bound ~ (1:horizon))), ymin= predict(loess(Lower_bound ~ (1:horizon))), fill="pink", alpha=.2 )) +:
                                                   geom_ribbon(data = temp_data_2, aes(x = x, ymin = ymin, ymax = ymax), fill = "pink", alpha = 0.2) +       
                                                   geom_spline(data = as.data.frame(data[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), color = "black", spar = Pspar) 
               
             }
           }
             
             }
    
  
  if (type_of_representation == "IRF") {
    for (shock in 1:nb_shocks) {
      for (var in 1:nb_var) {
        IRFs_representation[[var, shock]] = IRFs_representation[[var, shock]] +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red")
  }}}

  
  
  # ASB: more visible and clearer labels
  for (shock in 1:nb_shocks) {
    for (var in 1:nb_var) {
     IRFs_representation[[var, shock]] 
      
      
    }}
  
  
  
  
  
  
  IRFs_final = plotly::subplot(IRFs_representation , nrows = nb_shocks, shareX = T, titleY = T)
  
  return(IRFs_final)
}

#_#######################################################







deseasonalization_dummies = function(dataframe, period_column, columns) {
  
  dataframe[["month"]] <- as.factor(lubridate::month(dataframe[[period_column]])) 
  
  for (col in columns) {

    new_col <- paste(col, "_deseasonalized", sep = "")
    model_formula <- as.formula(paste0(col, " ~ " , "month"))
    regression <- lm(model_formula, data = dataframe)
    temp_reg <- c(0, regression[[1]][-1])
    temp_data <- as.data.frame(cbind(factor(unique(dataframe[["month"]])), temp_reg)) %>%
      rename(month = 'V1', mean = 'temp_reg') %>%
      mutate(month = as.factor(month))
    mean = colnames(temp_data)[2]
    dataframe_temp <- left_join(dataframe, temp_data, by = "month")
    dataframe[[new_col]] <- dataframe[[col]] - dataframe_temp[[mean]]
  }

  return(dataframe)
  
}



  
  #   annotations_x_axis = as.list(numeric(length = nb_var))
  #   annotations_y_axis = as.list(numeric(length = nb_shocks))
  #   
  #   for (shocks in 1:nb_shocks) {
  #     
  #     annotations_x_axis[[shocks]] =  
  #       list( 
  #         x = shocks *  1 / nb_shocks,  
  #         y = 0,  
  #         text = as.character(shock_names[shocks])  ,  
  #         xref = "paper",  
  #         yref = "paper",  
  #         xanchor = "center",  
  #         yanchor = "bottom",
  #         showarrow = FALSE)
  #   } 
  #     
  #     for (var in 1:nb_var) {
  #       
  #       annotations_y_axis[[var]] = list( 
  #         
  #           x = 0,  
  #           y = var * 1 / nb_var,  
  #           text = as.character(unlist(SVAR_NSR$varNames[1,var]))  ,  
  #           xref = "paper",  
  #           yref = "paper",  
  #           xanchor = "center",  
  #           yanchor = "bottom",  
  #           showarrow = FALSE) 
  #       
  #   }
  # 
  # annotations_total = c(as.list(annotations_x_axis), as.list(annotations_y_axis))  
  # 
  # figure = IRFs_final %>% layout( annotations = annotations_total  )



#######################################################################################################


# Plot_figure_test = function(data, data_2 = NULL , horizon, data_lower = NULL, data_upper = NULL, data_lower_2 = NULL, data_upper_2 = NULL, type_of_representation = "IRF", selector ){
# 
#   IRFs_representation = as.list(numeric(length = nb_var*nb_shocks))
#   dim(IRFs_representation) = c(nb_var, nb_shocks)
#   
#   data_combined = as.list(numeric(length = nb_var*nb_shocks))
#   dim(data_combined) = c(nb_var, nb_shocks)
#   
#   data_combined_2 = as.list(numeric(length = nb_var*nb_shocks))
#   dim(data_combined_2) = c(nb_var, nb_shocks)
#   
#   
#   switch(selector,
#         1 = {for (shock in 1:nb_shocks) {
#            for (var in 1:nb_var) {
#              IRFs_representation[[var, shock]] = ggplot() +
#                theme_bw() +
#                geom_line(data = as.data.frame(data[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs)) +
#                labs(y = as.character(shock_names[shock]) , x = as.character(unlist(SVAR_NSR$varNames[1,var])))
#              
#           }
#          }
#         },
#         2 = {for (shock in 1:nb_shocks) {
#            for (var in 1:nb_var) {
#                data_combined[[var,shock]] = cbind(data_lower[[var,shock]], data_upper[[var,shock]])
#           }
#          }
#            
#            for (shock in 1:nb_shocks) {
#              for (var in 1:nb_var) {
#                IRFs_representation[[var, shock]] = ggplot() +
#                  theme_bw() +
#                  geom_line(data = as.data.frame(data_lower[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotted") +
#                  geom_line(data = as.data.frame(data_upper[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotted") +
#                  labs(y = as.character(shock_names[shock]) , x = as.character(unlist(SVAR_NSR$varNames[1,var]))) +
#                  geom_ribbon(data = as.data.frame(data_combined[var, shock]) %>% set_names(., "Lower_bound","Upper_bound"), aes(x=1:horizon, ymax = Upper_bound, ymin= Lower_bound), fill="pink", alpha=.2 ) +
#                  geom_line(data = as.data.frame(data[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs))
#         }
#            }


#         3 = {for (shock in 1:nb_shocks){
#           for (var in 1:nb_var) {
#             data_combined[[var,shock]] = cbind(data_lower[[var,shock]], data_upper[[var,shock]])
#             data_combined_2[[var,shock]] = cbind(data_lower_2[[var,shock]], data_upper_2[[var,shock]])}}
#             
#           for (shock in 1:nb_shocks) {
#             for (var in 1:nb_var) {
#             IRFs_representation[[var, shock]] = ggplot() +
#                  theme_bw() +
#                  geom_line(data = as.data.frame(data_lower[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotted") +
#                  geom_line(data = as.data.frame(data_upper[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotted") +
#                  geom_line(data = as.data.frame(data_lower_2[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotdash") +
#                  geom_line(data = as.data.frame(data_upper_2[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dotdash") +
#                  labs(y = as.character(shock_names[shock]) , x = as.character(unlist(SVAR_NSR$varNames[1,var]))) +
#                  geom_ribbon(data = as.data.frame(data_combined[var, shock]) %>% set_names(., "Lower_bound","Upper_bound"), aes(x=1:horizon, ymax = Upper_bound, ymin= Lower_bound), fill="pink", alpha=.2 ) +
#                  geom_ribbon(data = as.data.frame(data_combined_2[var, shock]) %>% set_names(., "Lower_bound","Upper_bound"), aes(x=1:horizon, ymax = Upper_bound, ymin= Lower_bound), fill="blue", alpha=.1 ) +
#                  geom_line(data = as.data.frame(data[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs)) +
#                  geom_line(data = as.data.frame(data_2[var,shock]) %>% setNames(., "IRFs")  , aes(x = 1:horizon, y = IRFs), linetype = "dashed")
#               }
#             }
#             
#           })
#   
#   if (type_of_representation == "IRF") {
#     for (shock in 1:nb_shocks) {
#       for (var in 1:nb_var) {
#         IRFs_representation[[var, shock]] = IRFs_representation[[var, shock]] +
#           geom_hline(yintercept = 0, linetype = "dashed", colour = "red")
#       }}}
#   
#   
#   IRFs_final = plotly::subplot(IRFs_representation , nrows = nb_shocks, shareX = T, titleY = T)
#   
#   return(IRFs_final)
# }
#   
#   

remove_outliers <- function(X) {
  # =========================================================================
  # DESCRIPTION:
  # This function takes a set of series aligned in the columns of a matrix
  # and replaces outliers with the value NA.
  #
  # -------------------------------------------------------------------------
  # INPUT:
  #           X   = dataset (one series per column)
  #
  # OUTPUT:
  #           Y   = dataset with outliers replaced with NA
  #
  # -------------------------------------------------------------------------
  # NOTES:
  #           1) Outlier definition: a data point x of a series X(:,i) is
  #           considered an outlier if abs(x-median)>10*interquartile_range.
  #
  #           2) This function ignores values of NaN and thus is capable of
  #           replacing outliers for series that have missing values.
  #
  # =========================================================================
  # Error checking
  # if (!inherits(X, "fredmd"))
  #   stop("Object must be of class 'fredmd'")
  #             - inherits(object, "fredmd"): The inherits function is used to check if an object inherits from a
  #               particular class.
  #             - !inherits(object, "fredmd"): The ! operator negates the result of the inherits function. So, this
  #               condition is true if the object does not inherit from the class "fredmd".
  #             - stop("Object must be of class 'fredmd'"): If the condition is true, the stop function is called.
  #               The stop function is used to generate an error message and halt the execution of the program.
  #             So, we are checking if the object is not of class "fredmd" and, if so, it stops the execution.
  # =========================================================================
  # Calculate median of each series
  #             We use the 'apply' function to calculate the median for each
  #             column (i.e., margin 2) of a matrix 'X'
  #             -'X': This is the matrix or data frame for which you want to calculate column-wise medians.
  #             -'2': This argument specifies that the operation should be applied to each column.
  #                 In R, 1 would mean rows, and 2 means columns.
  #             - stats::median: This is the function that will be applied to each column (or row) of the matrix.
  #               In this case, it's the median function from the stats package.
  #             - na.rm = TRUE: This argument specifies whether to remove missing values (NA) before applying
  #               the function. In this case, na.rm = TRUE means that any missing values in each column will be
  #               ignored when calculating the median.
  #  So, overall, this command is calculating the median of each column in the matrix X, ignoring any missing values
  #  in each column. The result will be a vector of median values, one for each column of the matrix.
  median_X <- apply(X, 2, stats::median, na.rm = TRUE)
  # median_X <- matrix(median_X)
  # median_X <- t(median_X)
  
  # Repeat median of each series over all data points in the series
  median_X_mat <- matrix(rep(median_X, nrow(X)), nrow = nrow(X),
                         ncol = ncol(X), byrow = TRUE)
  #             - rep(median_X, nrow(X)): The rep function is used to replicate the value median_X a certain number
  #               of times. In this case, it is replicated nrow(X) times, where nrow(X) is the number of rows in the
  #               matrix X. This creates a vector of repeated median values.
  #             - matrix(...): This function is then used to convert the repeated median vector into a matrix.
  #                 nrow = nrow(X): Specifies the number of rows in the resulting matrix, which is the same as the
  #                                 number of rows in the original matrix X.
  #                 ncol = ncol(X): Specifies the number of columns in the resulting matrix, which is the same as
  #                                 the number of columns in the original matrix X.
  #                 byrow = TRUE: This argument specifies that the values in the repeated median vector should be
  #                               filled into the matrix by rows. If byrow = FALSE (or not specified), the filling would be done by columns.
  # So, overall, this command creates a matrix where each row contains the same repeated median value, and the
  # number of rows and columns in the new matrix is the same as the number of rows and columns in the original
  # matrix.
  
  # Calculate quartiles
  Q <- apply(X, 2, stats::quantile, probs = c(0.25, 0.75), na.rm = TRUE)
  
  # Calculate interquartile range (IQR) of each series
  IQR <- Q[2,] - Q[1,]
  
  # Repeat IQR of each series over all data points in the series
  IQR_mat <- matrix(rep(IQR, nrow(X)), nrow = nrow(X),
                    ncol = ncol(X), byrow = TRUE)
  
  # Determine outliers
  Z <- abs(X - median_X_mat)
  outlier <- (Z > (10 * IQR_mat))
  
  # Replace outliers with NaN
  Y <- X
  Y[outlier] <- NA
  
  # Cleaned data
  outdata <- Y
  # We set the class attribute of the object outdata to be a combination of two classes: "data.frame" and "fredmd".
  # In R, an object can belong to multiple classes. The class function is used to query or set the class of an object.
  # In this case, it's setting the class of outdata to be both "data.frame" and "fredmd".
  #class(outdata) <- c("data.frame", "fredmd")
  return(outdata)
  
  # Print the number of outliers
  print("Number of outliers:", quote = FALSE)     # quote = FALSE: The quote argument in the print function controls
  # whether or not to print quotation marks around the character
  # string. When quote = FALSE, it means that the string will be
  # printed without quotation marks.
  print(sum(outlier, na.rm = TRUE), quote = FALSE)  # na.rm = TRUE: The na.rm argument is set to TRUE, which means
  # that any missing values (NA) in the vector or matrix will be
  # removed before calculating the sum.
  # If na.rm is set to FALSE or not specified, the presence of any
  # missing values would result in the sum being reported as NA.
}

transform_data <- function(x2, DEMEAN) {
  
  # =========================================================================
  # DESCRIPTION
  # This function transforms a given set of series based upon the input variable DEMEAN.
  #  The following transformations are possible:
  #
  #   1) No transformation.
  #
  #   2) Each series is demeaned only (i.e. each series is rescaled to have a mean of 0).
  #
  #   3) Each series is demeaned and standardized (i.e. each series is rescaled to have a mean of 0 and a standard
  #       deviation of 1).
  #
  #   4) Each series is recursively demeaned and then standardized. For a given series x(t), where t=1,...,T,
  #     the recursively demeaned series x'(t) is calculated as x'(t) = x(t) - mean(x(1:t)). After the recursively
  #     demeaned series x'(t) is calculated, it is standardized by dividing x'(t) by the standard deviation of the
  #     original series x. Note that this transformation does not rescale the original series to have a specified mean
  #     or standard deviation.
  #
  # -------------------------------------------------------------------------
  # INPUTS
  #           x2 = set of series to be transformed (one series per column); no missing values;
  #           DEMEAN  = an integer indicating the type of transformation performed on each series in x2; it can take
  #                     on the following values:
  #                           0 (no transformation)
  #                           1 (demean only)
  #                           2 (demean and standardize)
  #                           3 (recursively demean and then standardize)
  #
  # OUTPUTS
  #           x22     = transformed dataset
  #           mut     = matrix containing the values subtracted from x2
  #                     during the transformation
  #           sdt     = matrix containing the values that x2 was divided by
  #                     during the transformation
  #
  # =========================================================================
  # FUNCTION
  # Number of observations in each series (i.e. number of rows in x2)
  T <- nrow(x2)
  
  # Number of series (i.e. number of columns in x2)
  N <- ncol(x2)
  
  # Perform transformation based on type determined by 'DEMEAN'
  if (DEMEAN == 0){
    # No transformation
    mut <- matrix(0, nrow = T, ncol = N)
    sdt <- matrix(1, nrow = T, ncol = N)
    x22 <- x2
  } else if (DEMEAN == 1){
    # Each series is demeaned only
    mut <- matrix(rep(mean(x2), each = T), nrow = T, ncol = N)
    sdt <- matrix(1, nrow = T, ncol = N)
    x22 <- x2 - mut
  } else if (DEMEAN == 2){
    # Each series is demeaned and standardized
    mut <- matrix(rep(colMeans(x2), T), nrow = T, ncol = N, byrow = TRUE)
    sdt <- matrix(rep(sapply(x2,sd), T), nrow = T, ncol = N, byrow = TRUE)         # sd divides by T-1
    x22 <- (x2 - mut) / sdt
  } else if (DEMEAN == 3){
    # Each series is recursively demeaned and then standardized
    mut <- matrix(NA, nrow = T, ncol = N)
    for (t in 1:T) {
      mut[t, ] <- colMeans(x2[1:t, , drop = FALSE])
    }
    sdt <- matrix(rep(sd(x2),T), nrow = T, ncol = N)
    x22 <- (x2 - mut) / sdt
  }
  
  # Return the results
  return(list(x22 = x22, mut = mut, sdt = sdt))
}        
  
        
        

