
library(tidyverse)

# Dataset names
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/original.RData")
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/no_z_models.RData")
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/ME_models.RData")

objects <- ls()[sapply(ls(),function(t) is.list(get(t)))]
n_objects <- length(objects)

# Test functions
test_tidy <- function(data, n = 1) {

  test_that("returns tidy data",{
    expect_true(is_tibble(mplus_tidy(data[!!n], standardized = FALSE)))
  })
}

### Running test_tidy ###
for(data_n in 1:n_objects){

  # get the right dataset
  data <- get(objects[data_n])

  # Run the test
  for(n in 1:length(data)){
    print(paste0("Dataset: ", objects[data_n], " List number:", n))
    test_tidy(data = data, n = n)
  }
}

check <- test_tidy(null, n = 1)


