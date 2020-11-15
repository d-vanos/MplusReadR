
mplus_apa_table <- function(tidy_data, model_type){ 
  
  # Save objects for later use
  unique_var <- unique(tidy_data$variable)
  unique_var_n <- length(unique_var)
  unique_param <- unique(tidy_data$param)
  parameters_per_var <- rep(length(unique(tidy_data$param)), length(unique_var))
  
  # rearranging data and adding confidence intervals to the same cell as the point estimate 
  data <- tidy_data %>% 
    mutate(bold_start = ifelse(((lower_2.5ci < 0 & upper_2.5ci < 0)| (lower_2.5ci > 0 & upper_2.5ci > 0)), "<b>", ""),
           bold_end = ifelse(((lower_2.5ci < 0 & upper_2.5ci < 0)| (lower_2.5ci > 0 & upper_2.5ci > 0)), "</b>", ""),
           value = paste0(bold_start, as.character(est), " <br> [", as.character(lower_2.5ci), ", ", as.character(upper_2.5ci), "]", bold_end)) %>% 
    select(-c(paramHeader, est, lower_2.5ci, upper_2.5ci, bold_start, bold_end)) %>% 
    arrange(variable)

  # Change to desired table format by:
  # 1. Pivoting again so that each variable has their own columns 
  if (model_type != "bivariate") {
    
    data <- data %>% 
      pivot_wider(names_from = "variable", values_from = "value")
    
    # 2. Pivoting so that each parameter has its own column 
    data <- data %>% 
      pivot_wider(names_from = "param", values_from = all_of(unique_var))
    
    # Setting appropriate column names
    new_parameter_labels <- rep(unique_param, unique_var_n)
    colnames(data)[(ncol(data)-sum(parameters_per_var) + 1):ncol(data)] <- new_parameter_labels
  }

  if (model_type == "bivariate") {
    
    data <- data %>% 
      select(-variable) %>% 
      pivot_wider(names_from = "param", values_from = "value")
  }
  
  
  # Remove empty columns
  data <- data[!sapply(data, function(x) all(is.na(x)))]
  
  # Determine the number of columns per variable group (for html table)
  n_start_cols <- ifelse(model_type == "null", 4, 3)
  col_group <- rep((ncol(data)- n_start_cols)/unique_var_n, unique_var_n)
  
  # Creating a tidied html table
  if (model_type != "bivariate") {
    data <- data %>% 
      addHtmlTableStyle(css.tspanner = "font-family: Times New Roman, Times, serif;",
                        css.cgroup = "font-family: Times New Roman, Times, serif;", # For the top heading 
                        css.header = "font-family: Times New Roman, Times, serif;", # For the second heading
                        css.cell = "font-family: Times New Roman, Times, serif;") %>% # For the individual cells 
      htmlTable(cgroup = c("", unique_var),
                n.cgroup = c(n_start_cols, col_group),
                rnames = c(rep("", nrow(data))))
  }
  
  # Bivariate models don't need an extra column level for variables 
  else if (model_type == "bivariate") {
    data <- data %>% 
      addHtmlTableStyle(css.tspanner = "font-family: Times New Roman, Times, serif;",
                        css.cgroup = "font-family: Times New Roman, Times, serif;", # For the top heading 
                        css.header = "font-family: Times New Roman, Times, serif;", # For the second heading
                        css.cell = "font-family: Times New Roman, Times, serif;") %>% # For the individual cells 
      htmlTable(n.cgroup = c(ncol(data)-sum(parameters_per_var), parameters_per_var),
                rnames = c(rep("", nrow(data))))
  }
  
  return(data)
}

