
# Check whether each model in an Mplus list converged 
mplus_model_converge <- function(Mplus_file){
  
  # Initialising the variable used in the loop below
  data <- tibble()
  
  # Check for each dataset in the Mplus file, whether one of the columns contains confidence intervals. If not, indicate that it did not converge. 
  for (model_n in 1:length(Mplus_file)) {
    
    colnames <- paste0(colnames(Mplus_file[[model_n]]$parameters$unstandardized), collapse = " ")
    
    data[model_n, "list_number"] <- model_n # Makes it easy to determine which model in the list of models it was 
    data[model_n, "file_name"] <- names(Mplus_file[model_n])
    data[model_n, "dataset_title"] <- as.character(Mplus_file[[model_n]]$input$title)
    data[model_n, "converged"] <- ifelse(grepl("lower_2.5ci", colnames), "TRUE", "FALSE")

  }
  return(data)
}

# Check whether each model in an Mplus list converged and remove those that did not 
remove_no_converge <- function(Mplus_model){
  # Create a table of values indicating which models did or did not converge
  data <- mplus_model_converge(Mplus_model)
  
  # Create a list of models that did not converge and save names 
  not_converged <- as_vector(data %>% 
                               filter(converged == FALSE) %>% 
                               select(file_name))
  
  # Remove these from the original Mplus model list
  Mplus_model <- Mplus_model[names(Mplus_model) %in% not_converged == FALSE]
  
  # Return (spit out) the new filtered Mplus model list
  return(Mplus_model)
  
}