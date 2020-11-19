
library(tidyverse)

# Dataset names
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/original.RData")
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/no_z_models.RData")
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/ME_models.RData")
rm(bivarcov_ME)

objects <- ls()[sapply(ls(),function(t) is.list(get(t)))]
n_objects <- length(objects)

# Test functions
test_compile <- function(data) {

  tidy_data <- mplus_compile(data, standardized = FALSE)

  test_that("returns tidy data",{
    expect_true(is_tibble(tidy_data))
  })
}

### Running test_tidy ###
for(data_n in 1:n_objects){

  # get the right dataset
  data <- get(objects[data_n])

  # Run the test
  print(paste0("Dataset: ", objects[data_n]))
  test_compile(data = data)

}


