
# D. J. van Os
# 18/11/2020

mplus_tidy <- function(Mplus_file, model_n = 1, param_header = NULL, parameter = NULL, display = "all", standardized = TRUE, define = FALSE, rounding = 2){

  #### Create a tibble ####

  if(length(Mplus_file[[model_n]]$parameters) == 0) {

    warning("This model does not appear to contain any parameters (standardized or unstandardized).")
    stop()
  }

  # Creating a dataset of standardized estimates
  if (standardized == TRUE) {

    data <- as_tibble(Mplus_file[[model_n]]$parameters$stdyx.standardized)


    # Warning the user if there is no unstandardized output
    if(nrow(data) < 1){

      warning("This model does not appear to contain standardized estimates. Have you tried using unstandardized output (e.g., standardized = FALSE)?")
      stop()
    }
  }

  # Creating a dataset of unstandardized estimates
  if (standardized == FALSE) {

    data <- as_tibble(Mplus_file[[model_n]]$parameters$unstandardized)

    # Add within-level R2 values if dataset was successfully created
    r2_data <- as_tibble(Mplus_file[[model_n]]$parameters$r2)

    # Warning the user if there is no unstandardized output
    if(nrow(data) < 1){

      warning("This model does not appear to contain unstandardized estimates. Have you tried using standardized output (e.g., standardized = TRUE)?")
      stop()

    }
  }

    #### Add R2 ####
    # Add R2 values if dataset was successfully created
    if(nrow(data) >= 1){

      r2_data <- as_tibble(Mplus_file[[model_n]]$parameters$r2)

      # Add in columns to r2_data so it matches data (for binding)
      r2_colnames <- setdiff(colnames(data), colnames(r2_data))
      r2_data[r2_colnames] <- NA
      r2_data["paramHeader"] <- "R2"

      # Bind R2 to the rest of the dataset
      data <- rbind(data, r2_data)
    }


  #### Add dataset info ####
  title <- as.character(Mplus_file[[model_n]]$input$title)

  if(title == ""){
    warning("This model or set of models does not appear to have a title. Displaying file name instead.")
  }

  data <- data %>%
    mutate(dataset_title = gsub(".*\\.(.*)\\..*", "\\1", as.character(names(Mplus_file)[[model_n]])),
           `T` = Mplus_file[[model_n]]$summaries$Observations,
           N = Mplus_file[[model_n]]$data_summary$overall$NClusters) %>%
    select(dataset_title, `T`, N, everything())

  # Define statement
  if(define == TRUE){
    data <- data %>%
      mutate(define = paste(trimws(Mplus_file[[model_n]]$input$define), collapse = "|"))
    check <- trimws(Mplus_file[[model_n]]$input$define)
  }



  ##### Display options for extra columns ####

  if(length(display) == 1){

    if(display == "minimal"){

      # Check that all the required columns needed for 'minimal' exist in the dataset
      minimal_columns <- c("dataset_title", "paramHeader", "param", "est", "lower_2.5ci", "upper_2.5ci")

      if(length(setdiff(minimal_columns, colnames(data))) > 0){
        warning(paste0("Some of the columns required for this theme do not appear to be present in the dataset.
                     These are: ", setdiff(minimal_columns, colnames(data)),". Reverting to default: display = \"all\"."))
      }

      # If all the columns exist, select only these columns
      else if(length(setdiff(minimal_columns, colnames(data))) == 0){

        data <- data %>%
          select(dataset_title, paramHeader, param, est, lower_2.5ci, upper_2.5ci)

      }
    }

    if(display == "descriptives"){

      # Check that all the required columns needed for 'descriptives' exist in the dataset
      descript_columns <- c("dataset_title", "paramHeader", "T", "N", "param", "est", "lower_2.5ci", "upper_2.5ci")

      if(length(setdiff(descript_columns, colnames(data))) > 0){
        warning(paste0("Some of the columns required for this theme do not appear to be present in the dataset.
                     These are: ", setdiff(descript_columns, colnames(data)),". Reverting to default: display = \"all\"."))
      }

      # If all the columns exist, select only these columns
      else if(length(setdiff(descript_columns, colnames(data))) == 0){

        data <- data %>%
          select(dataset_title, paramHeader, `T`, N, param, est, lower_2.5ci, upper_2.5ci)

      }
    }
  }

  # If the user wants to specify the columns themselves
  else {

    # Remove unwanted columns if they exist
    if(length(setdiff(display, colnames(data))) == 0){

      if(!"dataset_title" %in% display){
        warning("dataset_title must be included in the display options.")
        stop()
      }

      else {
      data <- data %>% select(display)
      }
    }

    # If the columns do not exist (there are names in do_not_display that are not column names)
    else if(length(setdiff(display, colnames(data))) > 0){

      warning("The columns you have specified to do not appear to be present in the dataset. Displaying all columns.")

    }
  }




  ##### Filter by paramHeader #####

  if(!is.null(param_header)){

      present_params <- intersect(param_header, data$paramHeader)

      data <- data %>%
        filter(paramHeader %in% present_params)
  }




  ##### Filter by param ####

  # Check if the user specifies a parameter
  if(!is.null(parameter)){

    # Warn user if parameter does not exist
    if(length(setdiff(parameter, data$param)) > 0){
      warning(paste0("Some of the parameters specified do not exist in the dataset.
                     These are: ", setdiff(parameter, data$param),". Displaying all parameters."))
    }

    # Filter by parameter if it exists
    else if (length(setdiff(parameter, data$param)) == 0){

      data <- data %>%
        filter(param %in% parameter)
    }
  }





  #### Rounding #####

  # Printing a warning if the user enters a letter
  if (!is.numeric(rounding)) {

    warning("Rounding error: you have not entered a number. No rounding has been applied.")
  }

  # Printing a warning if they try to round to more than 3 decimal places (which is what the Mplus Output is)
  else if (rounding > 3) {
    ''
    warning("Rounding error: It is not possible to print a dataset with more than 3 decimal places. No rounding has been applied.")
  }

  # Applying the user's rounding, if specified, or use the default of 2.
  else {
    # data <- data %>%
    #   mutate(est = formatC(est, rounding, format = "f"),
    #          lower_2.5ci = formatC(lower_2.5ci, rounding, format = "f"),
    #          upper_2.5ci = formatC(upper_2.5ci, rounding, format = "f"))

    # Checking if any character columns need to be converted to numeric columns

    # Apply numeric_cols function (see 'helper_functions.R')
    numeric_cols <- check_numeric(data)

    # Apply rounding to all numeric cols
    data <- data %>%
      mutate(across(all_of(numeric_cols), .fns = formatC, digits = rounding, format = "f"))

  }


return(data)
}



