

library(tibble)
library(MplusReadR)


# test_univar <- function(data, n){
#
#   test_that("returns univariate tibble", {
#     expect_true(is_tibble(tidy_bivar(data[!!n])), info = print(paste0("Dataset:", as.character(data), "List Number", as.character(n))))  # outputs a tibble
#     expect_equal(colnames(tidy_bivar(data[!!n])), c("dataset_title", "paramHeader", "outcome", "param", "est", "lower_2.5ci", "upper_2.5ci", "variable")) # Right column headings
#   })
# }
#
# for(n in 1:length(univar_orig)){
#   print(paste0("Dataset: univar List number:", n))
#   test_univar(data = univar_orig, n = n)
# }


test_other <- function(data, n){

  test_that("returns univariate tibble", {
    expect_true(is_tibble(tidy_data(data[!!n], model_type = "other")), info = print(paste0("Dataset:", as.character(data), "List Number", as.character(n))))  # outputs a tibble
    expect_equal(colnames(tidy_data(data[!!n], model_type = "other")), c("dataset_title", "paramHeader", "variable", "est", "lower_2.5ci", "upper_2.5ci")) # Right column headings
  })
}

for(n in 1:length(simul)){
  print(paste0("Dataset: simul. List number:", n))
  test_other(data = univar_orig, n = n)
}

