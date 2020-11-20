
library(tidyverse)

# Dataset names
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/original.RData")
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/no_z_models.RData")
load("C:/Users/dvano/OneDrive - The University of Melbourne/MPlus Automation Project/Data/RData files/ME_models.RData")
rm(bivarcov_ME)

objects <- ls()[sapply(ls(),function(t) is.list(get(t)))]
n_objects <- length(objects)

##### Test functions ####

# Tibble
# compile_tibble <- function(data) {
#
#   tidy_data <- mplus_compile(data, standardized = FALSE)
#
#   test_that("returns tidy data",{
#     expect_true(is_tibble(tidy_data))
#   })
# }
#
# # Rounding
# compile_rounding <- function(data) {
#
#   test_that("returns correct rounding", {
#
#   })
#
# }

# Param_header


# Parameter


# Display
compile_display <- function(data) {

  full <- data %>% mplus_compile(standardized = FALSE) %>% select(-c(dataset, dataset_title))
  minimal <- mplus_compile(data, standardized = FALSE, display = "minimal")
  descriptives <- mplus_compile(data, standardized = FALSE, display = "descriptives")

  sample_cols <- c(sample(colnames(full), size = 3), "dataset_title")
  custom <- mplus_compile(data, standardized = FALSE, display = sample_cols)

  test_that("displays correct columns",{
    expect_true(setequal(colnames(minimal), c("dataset", "dataset_title", "paramHeader", "param", "est", "lower_2.5ci", "upper_2.5ci")))
    expect_true(setequal(colnames(descriptives), c("dataset", "dataset_title", "paramHeader", "T", "N", "param", "est", "lower_2.5ci", "upper_2.5ci")))
    expect_true(setequal(colnames(custom), sample_cols))

  })
}

for(data_n in 1:n_objects){

  # get the right dataset
  data <- get(objects[data_n])

  # Run the test
  print(paste0("Dataset: ", objects[data_n]))
  compile_display(data = data)

}


### Running test_tidy ###
# for(data_n in 1:n_objects){
#
#   # get the right dataset
#   data <- get(objects[data_n])
#
#   # Run the test
#   print(paste0("Dataset: ", objects[data_n]))
#   compile_tibble(data = data)
#
# }


