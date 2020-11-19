library(tibble)
library(MplusReadR)


test_bivar <- function(data, n){

    test_that("returns bivariate tibble", {
    expect_true(is_tibble(tidy_bivar(data[!!n])), info = print(paste0("Dataset:", as.character(data), "List Number", as.character(n))))  # outputs a tibble
    expect_equal(colnames(tidy_bivar(data[!!n])), c("dataset_title", "paramHeader", "outcome", "param", "est", "lower_2.5ci", "upper_2.5ci", "variable")) # Right column headings
  })
}

for(n in 1:length(bivar_orig)){
  print(paste0("Dataset: bivar_orig List number:", n))
  test_bivar(data = bivar_orig, n = n)
}

for(n in 1:length(bivarcov_orig)){
  print(paste0("Dataset: bivarcov_orig, List number:", n))
  test_bivar(data = bivarcov_orig, n = n)
}
































#
# # Null models
# test_null <- function(data){
#   test_that("returns null tibble", {
#     data <- tidy_null(data)
#     columns <- colnames(data)
#
#     expect_true(is_tibble(data)) # outputs a tibble
#     expect_equal(columns, c("dataset_title", "paramHeader", "T", "N", "param", "est", "lower_2.5ci", "upper_2.5ci", "variable")) # Right column headings
#   })
# }
#
# for(n in 1:length(null)){
#   test_null(null[!!n])
# }
#
# # Univariate models
# test_univar <- function(data){
#   test_that("returns univariate tibble", {
#     data <- tidy_univar(data)
#     columns <- colnames(data)
#
#     expect_true(is_tibble(data)) # outputs a tibble
#     expect_equal(columns, c("dataset_title", "paramHeader", "outcome", "param", "est", "lower_2.5ci", "upper_2.5ci", "variable")) # Right column headings
#   })
# }
#
# for(n in 1:length(univar_orig)){
#   test_univar(univar_orig[!!n])
# }
#
# for(n in 1:length(univar_ME)){
#   test_univar(univar_ME[!!n])
# }
#
# for(n in 1:length(univar_ME)){
#   test_univar(univar_no_z[!!n])
# }



##### Bivariate models #####
# test_bivar <- function(data){
#   columns <- colnames(data[!!n])
#
#   test_that("returns univariate tibble", {
#     expect_true(is_tibble(data[!!n])) # outputs a tibble
#     expect_equal(columns, c("dataset_title", "paramHeader", "outcome", "param", "est", "lower_2.5ci", "upper_2.5ci", "variable")) # Right column headings
#   })
# }
# #
# # # Bivariate
# data <- tidy_bivar(bivar_orig)
#
# for(n in 1:length(bivar_orig)){
#   test_bivar(data[!!n])
# }

# # Bivariate Cov

# for(n in 1:length(bivarcov_orig)){
#   test_bivar(bivarcov_orig[!!n])
# }

# Bivariate - ME
#data <- tidy_bivar(bivar_ME)

# for(n in 1:length(bivar_ME)){
#   test_bivar(bivar_ME[!!n])
# }
#
# for(n in 1:length(bivar_ME)){
#   data <- tidy_bivar(bivar_ME[!!n])
#   columns <- colnames(data)
#
#   expect_true(is_tibble(data)) # outputs a tibble
#   expect_equal(columns, c("dataset_title", "paramHeader", "outcome", "param", "est", "lower_2.5ci", "upper_2.5ci", "variables")) # Right column headings
# }


#
# # Bivariate cov - ME
# for(n in 1:length(bivarcov_ME)){
#   test_bivar(bivarcov_ME[!!n])
# }
#
# # Bivariate - no Z
# for(n in 1:length(bivar_no_z)){
#   test_bivar(bivar_ME[!!n])
# }
#
# # Bivariate cov - no Z
# for(n in 1:length(bivarcov_no_z)){
#   test_bivar(bivar_ME[!!n])
# }



# f <- function(i) if (i > 3) i * 9 else i * 10
#
#
# for (i in 1:5) {
#   expect_equal(f(!!i), !!(i * 10))
# }
