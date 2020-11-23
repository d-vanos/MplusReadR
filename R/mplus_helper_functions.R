
#' @importFrom Hmisc all.is.numeric

# Checks which columns in a dataframe are numeric BUT NOT integer
check_numeric <- function(data){
  data_colnames <- colnames(data)
  numeric_cols <- c()

  for(n in 1:length(data_colnames)){

    # Check if the column is numeric
    vector <- as_vector(data[data_colnames[n]])
    numeric <- all.is.numeric(vector)

    if(numeric == TRUE){

      # Check if the column is not an integer
      integer <- function(x){
        test <- all.equal(as.numeric(x), as.integer(x), check.attributes = FALSE)
        if(test == TRUE){ return(TRUE) }
        else { return(FALSE) }
      }

      # Save the column name if it is not an integer
      if(integer(vector) == FALSE){

        numeric_cols <- c(numeric_cols, data_colnames[n])
      }
    }

  }
  return(numeric_cols)
}
