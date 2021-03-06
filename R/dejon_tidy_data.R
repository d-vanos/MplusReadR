
##### ---- Tidy data - null models ---- #####

dejon_null <- function (Mplus_file, model_n = 1) {

  # Create a list of variables originally in the output
  # The variables are selected from the 'usevar' variable from the first model in the output
  # Warning: it presumes that the variables in each of the models is the same - if it is not, this will need
  # to be modified.
  # DEBUG - make this a list of all the variables - if time left

  orig_var <- Mplus_file[[model_n]]$input$variable$usevar %>%
    as.character() %>%
    strsplit("\\ ") %>%
    unlist() %>%
    .[. != "PID"]

  if(!is.null(orig_var)){
    orig_var = paste0(orig_var, collapse = "|")
    orig_var = gsub(pattern = "obs", replacement = "", x = orig_var)
    orig_var = strsplit(orig_var, split = '|', fixed = TRUE)[[1]]
  }

  # no variables are specified in a master file, so instead the 'orig_var' for a master file will just be all the unique
  # files in the list of Mplus files.
  if(is.null(orig_var)) {
    orig_var <- check_vars(Mplus_file)
  }

  # Create a table which contains the relevant information (the dataset number, number of observations,
  # same size), and including only parameters from New/Additional parameters

  data <- as_tibble(Mplus_file[[model_n]]$parameters$unstandardized) %>%
    mutate(dataset_title = as.character(Mplus_file[[model_n]]$input$title),
           `T` = Mplus_file[[model_n]]$summaries$Observations,
           N = Mplus_file[[model_n]]$data_summary$overall$NClusters) %>%
    select(dataset_title, paramHeader, `T`, N, param, est, lower_2.5ci, upper_2.5ci)

  # Assigning correct variable names to the parameters
  for(row in 1:nrow(data)){

    for(n in 1:length(orig_var)){

      # Remove the variable name from in front of the parameter
      if(grepl(orig_var[n], data[row,"param"])){

        data[row, "variable"] <- orig_var[n]
        data[row, "param"] <- gsub(orig_var[n], "", data[row,"param"])
      }

      # If this was a mean value (i.e., only had the variable name in it) and was one of the new additional parameters, assign it the value "MEAN".
      # Otherwise, assign it the value of the paramHeader.
      if (data[row, "param"] == "" & data[row, 'paramHeader'] == "New.Additional.Parameters") {

        data[row, "param"] <- "MEAN"
      }
      else if(data[row, "param"] == ""){

        data[row, "param"] <- as.character(data[row, "paramHeader"])
      }
    }
  }
  return(data)
}

##### ---- Tidy data - univariate models ---- #####

dejon_univar <- function (Mplus_file, model_n = 1, standardized = TRUE) {

  # Create a list of variables originally in the output
  orig_var <- Mplus_file[[model_n]]$input$variable$usevar  %>%
    as.character() %>%
    strsplit("\\ ") %>%
    unlist() %>%
    .[!. %in% c("PID")]

  if(!is.null(orig_var)){
    orig_var = paste0(orig_var, collapse = "|")
    orig_var = gsub(pattern = "obs", replacement = "", x = orig_var)
    orig_var = strsplit(orig_var, split = '|', fixed = TRUE)[[1]]
  }

  # no variables are specified in a master file, so instead the 'orig_var' for a master file will just be all the unique
  # files in the list of Mplus files.
  if(is.null(orig_var)) {
    orig_var <- check_vars(Mplus_file)
  }

  # Create a table which contains the relevant information
  # Allow the user to determine whether the values should be standardized or not
  if (standardized == TRUE) {

    data <- as_tibble(Mplus_file[[model_n]]$parameters$stdyx.standardized)
  }
  else if (standardized == FALSE) {

    data <- as_tibble(Mplus_file[[model_n]]$parameters$unstandardized)
  }



  # Regex rules:
  # .* - matches any word before the .*
  # [=] and [;] - matches a literal
  # ([^;]+) - matches everything up to (but not including the ;)
  # matches an a word after the .*
  # \\1 - replace the whole string with the contents of the first capturing group

  # Main dataset
  data <- data %>%
    mutate(dataset_title = as.character(Mplus_file[[model_n]]$input$title),
           outcome = (gsub(".*[ =]([^;]+)[;].*", "\\1", Mplus_file[[model_n]]$input$define[3]))) %>%
    select(dataset_title, paramHeader, outcome, param, est, lower_2.5ci, upper_2.5ci)

  # Assigning correct variable names to the parameters
  for (row in 1:nrow(data)) {

    for (n in 1:length(orig_var)) {

      string <- substr(data[row, "param"], start = 0, stop = length(orig_var[n])+1)

      if(grepl(orig_var[n], string)){

        data[row, "variable"] <- string
        data[row, "param"] <- sub(string, "", data[row,"param"])


        # If this was a mean value (i.e., only had the variable name in it) and was one of the new additional parameters, assign it the value "MEAN".
        # Otherwise, assign it the value of the paramHeader.
        if (data[row, "param"] == "" & data[row, 'paramHeader'] %in% c("New.Additional.Parameters", "Z.ON")) {

          data[row, "param"] <- "MEAN"
        }
        else if(data[row, "param"] == ""){

          data[row, "param"] <- as.character(data[row, "paramHeader"])
        }
      }
      # DEBUG - maybe remove this
      if (data[row, "param"] == "R2"){

        data[row, "variable"] <- "R2"
      }
    }
  }

  if(standardized == TRUE){
    # Add R2 as a param
    r2_data <- as_tibble(Mplus_file[[model_n]]$parameters$r2) %>%
      filter(param == "Z") %>%
      mutate(dataset_title = as.character(data[1, 'dataset_title']),
             paramHeader = "R2",
             outcome = as.character(data[1, 'outcome']),
             variable = gsub(pattern = "OBS", replacement = "", x = Mplus_file[[model_n]]$parameters$r2$param[1]), # Takes the variable name from the first line (otherwise would be Z). Be careful with this if you have multiple variables.
             param = "R2") %>%
      select(dataset_title, paramHeader, outcome, param, est, lower_2.5ci, upper_2.5ci, variable)



    # Bind R2 to the rest of the dataset
    data <- rbind(data, r2_data)

  }
  return(data)
}

##### ---- Tidy data - bivariate models ---- #####

dejon_bivar <- function (Mplus_file, model_n = 1, standardized = TRUE) {

  # Create a list of variables originally in the output
  orig_var <- Mplus_file[[1]]$input$variable$usevar  %>%
    as.character() %>%
    strsplit("\\ ") %>%
    unlist() %>%
    .[!. %in% c("PID", "Z")]

  if(!is.null(orig_var)){
    orig_var = paste0(orig_var, collapse = "|")
    orig_var = gsub(pattern = "obs", replacement = "", x = orig_var)
    orig_var = strsplit(orig_var, split = '|', fixed = TRUE)[[1]]
  }

  # Create a table which contains the relevant information
  if (standardized == TRUE) {

    data <- as_tibble(Mplus_file[[model_n]]$parameters$stdyx.standardized)
  }
  else if (standardized == FALSE) {

    data <- as_tibble(Mplus_file[[model_n]]$parameters$unstandardized)
  }

  data <- data %>%
    mutate(dataset_title = as.character(Mplus_file[[model_n]]$input$title),
           outcome = (gsub(".*[ =]([^;]+)[;].*", "\\1", Mplus_file[[model_n]]$input$define[3]))) %>%
    select(dataset_title, paramHeader, outcome, param, est, lower_2.5ci, upper_2.5ci)

  # Assigning correct variable names to the parameters
  for (row in 1:nrow(data)) {

    for (n in 1:length(orig_var)) {

      string <- substr(data[row, "param"], start = 0, stop = length(orig_var[n])+1)

      if(grepl(orig_var[n], string)){

        data[row, "variable"] <- string
        #data[row, "param"] <- sub(string, "", data[row,"param"])

        # If this was a mean value (i.e., only had the variable name in it) and was one of the new additional parameters, assign it the value "MEAN".
        # Otherwise, assign it the value of the paramHeader.
        if (data[row, "param"] == orig_var[n] & data[row, 'paramHeader'] %in% c("New.Additional.Parameters", "Z.ON")){

          data[row, "param"] <- paste0(orig_var[n], "MEAN")
        }
        else if(data[row, "param"] == ""){

          data[row, "param"] <- as.character(data[row, "paramHeader"])
        }
      }
    }
  }
  if(standardized == TRUE){
    # Add R2 as a param
    r2_data <- as_tibble(Mplus_file[[model_n]]$parameters$r2) %>%
      filter(param == "Z") %>%
      mutate(dataset_title = as.character(data[1, 'dataset_title']),
             paramHeader = "R2",
             outcome = as.character(data[1, 'outcome']),
             variable = gsub(pattern = "OBS", replacement = "", x = Mplus_file[[model_n]]$parameters$r2$param[1]), # Takes the variable name from the first line (otherwise would be Z). Be careful with this if you have multiple variables.
             param = "R2") %>%
      select(dataset_title, paramHeader, outcome, param, est, lower_2.5ci, upper_2.5ci, variable)

    data <- rbind(data, r2_data)
  }

  return(data)
}



##### ---- Tidy data - all models ---- #####

dejon_tidy <- function (Mplus_file, model_type, model_n = 1, rounding = 2, parameters = NULL, variables = NULL, paramheaders = NULL, standardized = TRUE, outcomes = NULL) {

  # Warning the user if they didn't specify an appropriate model
  if (!model_type %in% c("null", "univariate", "bivariate", "other")) {

    warning("model type should be one of 'null', 'univariate', or 'bivariate'.")
  }

  else if (model_type == "null") {

    data <- dejon_null(Mplus_file, model_n)

    #If the user doesn't specify a paramheader, presume "New.Additional Parameters" (only for null models)
    if (is.null(paramheaders)) {

      data <- data %>%
        filter(paramHeader == "New.Additional.Parameters")
    }
  }

  else if (model_type == "univariate") {

    data <- dejon_univar(Mplus_file, model_n, standardized = standardized)

    # If the user doesn't specify a paramheader, presume "Z.ON" (only for univariate and bivariate models)
    if (is.null(paramheaders)) {

      data <- data %>%
        filter(paramHeader %in% c("Z.ON", "R2"))
    }

  }

  else if (model_type == "bivariate") {

    data <- dejon_bivar(Mplus_file, model_n, standardized = standardized)

    # If the user doesn't specify a paramheader, presume "Z.ON" (only for univariate and bivariate models)
    if (is.null(paramheaders)) {

      data <- data %>%
        filter(paramHeader%in% c("Z.ON", "R2"))
    }
  }

  # Rounding
  # Printing a warning if the user enters a letter
  if (!is.numeric(rounding)) {

    warning("Rounding error: you have not entered a number. Reverting to the default (2 decimal places).")
  }

  # Printing a warning if they try to round to more than 3 decimal places (which is what the Mplus Output is)
  else if (rounding > 3) {
    ''
    warning("Rounding error: It is not possible to print a dataset with more than 3 decimal places. Reverting to default (2 decimal places).")
  }

  # Applying the user's rounding, if specified, or use the default of 2.
  else {
    data <- data %>%
      mutate(est = formatC(est, rounding, format = "f"),
             lower_2.5ci = formatC(lower_2.5ci, rounding, format = "f"),
             upper_2.5ci = formatC(upper_2.5ci, rounding, format = "f"))

    #data[, c("est", "lower_2.5ci", "upper_2.5ci")] <- formatC(data[, c("est", "lower_2.5ci", "upper_2.5ci")], rounding, format = "f")
  }

  # Parameters
  # If the user specifies parameters, filter the data by the parameters specified
  if (!is.null(parameters)) {

    data <- data %>%
      filter(param %in% parameters)
  }

  # Variables
  # If the user specifies variables, filter the data by the variables specified
  if (!is.null(variables)) {

    data <- data %>%
      filter(variable %in% variables)
  }

  else if(is.null(variables)){

      data <- data %>%
        filter(variable != "Z.")
  }

  # paramHeader
  # If the user specifies a paramHeader, filter the data by the paramHeaders specified
  if (!is.null(paramheaders)) {

    data <- data %>%
      filter(paramHeader %in% paramheaders)
  }

  # Outcome
  if(!is.null(outcomes) & (model_type %in% c("univariate", "bivariate"))){

    data <- data %>%
      filter(outcome %in% outcomes)

  }

  return(data)
}

