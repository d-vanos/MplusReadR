

# 

mplus_check_parameters <- function(Mplus_file, parameter_type, standardised = TRUE){

  if (parameter_type == "variables") {
    
    mplus_check_vars(Mplus_file = Mplus_file)
  }
  
  else if (parameter_type == "parameters") {
    
    mplus_check_param(Mplus_file = Mplus_file, standardised = standardised)
  }
  
  else if (parameter_type == "outcomes") {
    
    mplus_check_outcomes(Mplus_file = Mplus_file)
    
  }
  
  else if (parameter_type == "paramheader") {
    
    mplus_check_paramheader(Mplus_file = Mplus_file)
  }
}


# Checks what vars are available 
mplus_check_vars <- function(Mplus_file){
  
  # Initialising the variable used in the loop below
  data <- tibble()
  
  # Add all the data from all the tables into one big dataset 
  for(n in 1:length(Mplus_file)){
    temp_data <- Mplus_file[[n]]$input$variable$usevar %>% 
      as.character() %>% 
      strsplit("\\ ") %>% 
      unlist() %>% 
      .[. != "PID"] %>% 
      as_tibble()
    data <- rbind(data, temp_data)
  }
  # keep only unique 
  data <- unique(data$value)
  
  # saving our new dataset 
  return(data)
}



# Checks what parameters are available
mplus_check_param <- function(Mplus_file, standardised = TRUE){
  
  # Initialising the variable used in the loop below
  data <- tibble()
  
  if(standardised == FALSE){
  # Add all the data from all the tables into one big dataset 
  for(n in 1:length(Mplus_file)){
    temp_data <- as_tibble(Mplus_file[[n]]$parameters$unstandardized$param)
    data <- rbind(data, temp_data)
  }
  }
  else if (standardised == TRUE){
    for(n in 1:length(Mplus_file)){
      temp_data <- as_tibble(Mplus_file[[n]]$parameters$stdyx.standardized$param)
      data <- rbind(data, temp_data)
    }
  }
  
  # keep only unique 
  data <- unique(data$value)
  
  # saving our new dataset 
  return(data)
}

mplus_check_outcomes <- function(Mplus_file){
  
  # Initialising the variable used in the loop below
  data <- tibble()
  
  # Add all the data from all the tables into one big dataset 
  for(n in 1:length(Mplus_file)){
  temp_data <- as_tibble(gsub(".*[ =]([^;]+)[;].*", "\\1", Mplus_file[[n]]$input$define[3]))
  data <- rbind(data, temp_data)
  }
  
  # keep only unique 
  data <- unique(data$value)
  
  # saving our new dataset 
  return(data)
}


mplus_check_paramheader <- function(Mplus_file){
  
  # Initialising the variable used in the loop below
  data <- tibble()
  
  for(n in 1:length(Mplus_file)){
    temp_data <- as_tibble(Mplus_file[[n]]$parameters$unstandardized$paramHeader)
    data <- rbind(data, temp_data)
  }
  
  # keep only unique 
  data <- unique(data$value)
  
  # saving our new dataset 
  return(data)
}

