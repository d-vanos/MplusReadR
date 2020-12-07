# D. J. van Os
# 26/11/2020

##########################-
#### Helper Functions ####
##########################-

# HTML formatting
formatting <- function(data, table_title = NULL){
  data %>%
  addHtmlTableStyle(css.tspanner = "font-family: Times New Roman, Times, serif;",
                    css.cgroup = "font-family: Times New Roman, Times, serif;", # For the top heading
                    css.header = "font-family: Times New Roman, Times, serif;", # For the second heading
                    css.cell = "font-family: Times New Roman, Times, serif;") %>%
  htmlTable(cgroup = table_title)
}

# Add confidence intervals and bolding
merge_CIs <- function(data, bold = TRUE){
  if(bold == TRUE){
    data <- data %>%
      mutate(bold_start = ifelse(((lower_2.5ci < 0 & upper_2.5ci < 0)| (lower_2.5ci > 0 & upper_2.5ci > 0)), "<b>", ""),
             bold_end = ifelse(((lower_2.5ci < 0 & upper_2.5ci < 0)| (lower_2.5ci > 0 & upper_2.5ci > 0)), "</b>", ""),
             value = paste0(bold_start, as.character(est), " <br> [", as.character(lower_2.5ci), ", ", as.character(upper_2.5ci), "]", bold_end)) %>%
      select(-c(est, lower_2.5ci, upper_2.5ci, bold_start, bold_end))
  }
  else if (bold == FALSE){
    data <- data %>%
      mutate( value = paste0(as.character(est), " <br> [", as.character(lower_2.5ci), ", ", as.character(upper_2.5ci), "]")) %>%
      select(-c(lower_2.5ci, upper_2.5ci))
  }
  return(data)
}

mplus_corr <- function(data, triangle = "lower", CI = TRUE, dataset_n = 1){

  if(CI == TRUE){
    # Combine CI and est and bold
    if(!"dataset" %in% colnames(data)){
      warning("The column 'dataset' is required in order to display a correlations table.")
      stop()
    }

    else {
    data <- data %>%
      filter(dataset == dataset_n) %>%
      mutate(bold_start = ifelse(((lower_2.5ci < 0 & upper_2.5ci < 0)| (lower_2.5ci > 0 & upper_2.5ci > 0)), "<b>", ""),
             bold_end = ifelse(((lower_2.5ci < 0 & upper_2.5ci < 0)| (lower_2.5ci > 0 & upper_2.5ci > 0)), "</b>", ""),
             value = paste0(bold_start, as.character(est), " <br> [", as.character(lower_2.5ci), ", ", as.character(upper_2.5ci), "]", bold_end),
             paramHeader = sapply(strsplit(paramHeader, "\\."), "[", 1)) %>%
      select(paramHeader, param,  value)
    }
  }

  if(CI == FALSE){
    # Don't include CIs
    data <- data %>%
      filter(dataset == dataset_n) %>%
      mutate(paramHeader = sapply(strsplit(paramHeader, "\\."), "[", 1)) %>%
      rename(value = est) %>%
      select(paramHeader, param, value)
  }

  # Determine number of unique paramheaders and params
  vars <- unique(c(data$paramHeader, data$param))

  # Find all possible combinations of variables
  cor_matrix <- expand.grid(vars, vars)

  # Add correlations in
  cor_matrix <- rbind(merge(cor_matrix, data, by.x=c('Var1', 'Var2'), by.y=c('paramHeader', 'param')),
                      merge(cor_matrix, data, by.x=c('Var2', 'Var1'), by.y=c('paramHeader', 'param')),
                      data.frame(Var1=vars, Var2=vars, value ="-"))

  # Create correlation matrix and format row names
  cor_matrix <- reshape2::dcast(cor_matrix, Var1~Var2, value.var='value')
  rownames(cor_matrix) <- cor_matrix[,1]
  cor_matrix <- as.matrix(cor_matrix[-1])

  # Remove lower triangle
  if (triangle == "lower") {
    cor_matrix[lower.tri(cor_matrix,diag = FALSE)] <- NA
  }

  # Or remove upper triangle
  else if(triangle == "upper") {
    cor_matrix[upper.tri(cor_matrix,diag = FALSE)] <- NA
  }

  return(cor_matrix)

}





# Create list of many APA correlations tables (NOT SURE IF STILL RELEVANT)
# mplus_multiple_corr <- function(data, triangle = "lower", CI = TRUE){
#
#   new_list <- list()
#
#   # Checks number of unique datasets
#   for(n in 1:length(unique(data$dataset))) {
#
#     data <- data %>%
#       filter(dataset == n)
#
#     cor_matrix <- mplus_corr(data = data, triangle = triangle, CI = CI)
#
#     new_list[[n]] <- cor_matrix
#   }
#   return(list)
# }

##########################-
#### APA-Style Tables ####
##########################-

#' APA-style Mplus Tables
#'
#' Creates APA-style tables containing output from multiple Mplus objects. These include descriptives tables, correlations tables, multi-level reliability tables and general tables.
#'
#' @import tidyr
#' @import htmlTable
#' @import reshape2
#'
#' @param type One of 'general' or 'correlation'. Defaults to 'general'.
#' @param bold Whether significant results (as indicated by confidence intervals not including 0) should be bolded. Defaults to TRUE.
#' @param CI Whether confidence intervals should be displayed in square brackets. Defaults to TRUE.
#' @param SD Whether standard deviation should be displayed in round brackets. Defaults to FALSE. Note that CI and SD cannot both equal TRUE.
#' @param triangle Whether correlations tables, if applicable, should contain values in the upper or lower triangle. One of 'upper' or 'lower'. Defaults to "upper".
#' @param header Whether correlations tables should contain the name of the dataset. Defaults to TRUE.
#' @param orientation Whether the orientation of the table (if the type = 'general') should be vertical or horizontal. Defaults to "vertical".
#'
#' @return APA-style table of Mplus output.
#' @export
#'
mplus_apa_table <- function(data,
                            type = "general",
                            bold = TRUE,
                            CI = TRUE,
                            SD = FALSE,
                            triangle = "lower",
                            header = TRUE,
                            orientation = "vertical",
                            n_tables = NULL){

  #### General APA Table ####
  if(type == "general"){

    # Dealing with the case where users specify both SD and CI, which would result in inappropriate formatting
    if(SD == TRUE & CI == TRUE) {

      warning("Only SD = TRUE or CI = TRUE can be specified - not both. Please include only one. Aborting.")
      stop()
    }

    # Include just CIs (no SDs) - CI columns must exist and must be selected
    else if(CI == TRUE){
      if(is.element("lower_2.5ci", colnames(data)) & is.element("upper_2.5ci", colnames(data))){
        data <- merge_CIs(data = data, bold = bold)
      }
      else{

        warning("You have selected CI = TRUE but there do not appear to be confidence intervals (lower_2.5ci and upper_2.5ci) in the tidy data.")
        stop()

      }

    }

    # Include just SDs (no CIs) - SD column must exist and must be selected
    else if(SD == TRUE){

      # Check if SDs are actually in the dataset
      if(is.element("posterior_sd", colnames(data))){
        data <- data %>%
          mutate(value = paste0(as.character(est), " (", as.character(posterior_sd), ")")) %>%
          select(-c(est, posterior_sd))
      }
      else{

        warning("You have selected SD = TRUE but posterior_sd does not appear to exist in the tidy data.")
        stop()
      }

    }


    if(orientation == "horizontal"){

      data <- data %>%
        mutate(paramheader_param = paste("paramheader:", paramHeader, "<br> param: ", param)) %>%
        select(-c(paramHeader, param)) %>%
        pivot_wider(id_cols = c("dataset", "dataset_title"), names_from = "paramheader_param", values_from = "value")


    }


    html_table <- formatting(data)
  }


  #### Correlations Tables ####
  else if(type == "correlation"){

    # Initialise the html table object
    html_table <- tibble()

    # Formatting all the correlations tables and adding them to the html_table object created above
    for (n in 1:length(unique(data$dataset_title))){

      # Create correlation table
      cor_data <- mplus_corr(data = data, triangle = triangle, CI = CI, dataset_n = n)

      # Add html formatting with table title
      if(header == TRUE){

        # Extract header
        table_title <- as.character(data[data$dataset == n, "dataset_title"][[1]][1])

        # Add html formatting
        cor_data <- formatting(data = cor_data, table_title = table_title)
      }

      # Add html formatting without displaying header
      else if(header == FALSE){

        cor_data <- formatting(data = cor_data)
      }

      # Add to list of tables
      html_table[n, "table_contents"] <- cor_data
    }

    # Select only html tables of interest
    if(!is.null(n_tables)){
      html_table <- html_table %>% slice(n_tables)
    }

    # Merge
    html_table <- paste(as_vector(html_table), collapse = '')

  }


  return(html_table)
}








