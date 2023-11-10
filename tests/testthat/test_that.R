# Load necessary libraries
library(testthat)
library(latlong)  # Make sure to load your package

# Test cases for lat_long function
test_that("lat_long function retrieves correct coordinates for known location", {
  result <- lat_long("New York City")
  expect_equal(result$latitude, 40.7128, tolerance = 0.001)
  expect_equal(result$longitude, -74.0060, tolerance = 0.001)
})

test_that("lat_long function handles special characters in location names", {
  result <- lat_long("Los Angeles, CA")
  expect_equal(result$latitude, 34.0522, tolerance = 0.001)
  expect_equal(result$longitude, -118.2437, tolerance = 0.001)
})

test_that("lat_long function handles spaces in location names", {
  result <- lat_long("San Francisco")
  expect_equal(result$latitude, 37.7749, tolerance = 0.001)
  expect_equal(result$longitude, -122.4194, tolerance = 0.001)
})

