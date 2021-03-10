
# D. J. van Os
# 18/11/2020

mplus_tidy <- function(Mplus_file, model_n = 1, param_header = NULL, parameter = NULL, standardized = TRUE, define = FALSE, rounding = 2){

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
    # If confidence intervals are not found under 'unstandardized' (e.g., maximum likelihood models)
    if (setequal(intersect(x = c("lower_2.5ci", "upper_2.5ci"), y = names(data)), c("lower_2.5ci", "upper_2.5ci")) == FALSE){

      CIs <- as_tibble(Mplus_file[[model_n]]$parameters$ci.unstandardized)[c("paramHeader", "param", "low2.5", "up2.5")]
      data <- left_join(data, CIs, by = c("paramHeader", "param"))
      data <- data %>%
        rename(lower_2.5ci = low2.5,
               upper_2.5ci = up2.5)
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
  data <- data %>%
    mutate(dataset_title = gsub(".*\\.(.*)\\..*", "\\1", as.character(names(Mplus_file)[[model_n]])),
           `T` = Mplus_file[[model_n]]$summaries$Observations,
           N = Mplus_file[[model_n]]$data_summary$overall$NClusters) %>%
    select(dataset_title, `T`, N, everything())

  # Define statement
  if(define == TRUE){
    if(display != "all" && ("define" %in% display == FALSE)){
      warning("You have specified display = TRUE but have not selected 'define' as a column.")
    }

    data <- data %>%
      mutate(define = paste(trimws(Mplus_file[[model_n]]$input$define), collapse = "|"))
    check <- trimws(Mplus_file[[model_n]]$input$define)
  }


  ##### Filter by paramHeader #####

  if(!is.null(param_header)){

    # If there are multiple parameters, combining these into one string (needed for grepl function)
    collapsed_paramheaders <- paste(param_header, collapse = "|")

      data <- data %>%
        filter(grepl(collapsed_paramheaders, paramHeader))
  }




  ##### Filter by param ####

  # Check if the user specifies a parameter
  if(!is.null(parameter)){

    collapsed_params <- paste(parameter, collapse = "|")

    data <- data %>%
      filter(grepl(collapsed_params, param))



    # if(length(setdiff(parameter, data$param)) > 0){
    #   warning(paste0("Some of the parameters specified do not exist in the dataset.
    #                  These are: ", setdiff(parameter, data$param),".
    #                  Only displaying selected parameters which are present in the dataset."))
    #
    #   present_params <- intersect(parameter, data$param)
    # }
    #
    # # Filter by parameter if it exists
    #
    #   data <- data %>%
    #     filter(param %in% parameter)
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



