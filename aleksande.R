# This file is used to test if my locally developed package transfers
# through the renv cache properly

library(aleksandeR)
library(testthat)

format_num_output <- format_num(234234.234238497239479234)
expected_output_format_num <- "234.234,2"

expect_equal(format_num_output, expected_output_format_num)
