library(testthat)

# tests/testthat/test-myfunction.R
test_that("print.itinerary returns the correct result", {
  # Create a sample itinerary with valid data
  sample_itinerary <- list("Travel from A to B: 100 km", "Travel from B to C: 150 km")

  # Capture the console output when calling print.itinerary
  result <- capture.output(print.itinerary(sample_itinerary))

  # Remove trailing spaces from the actual output
  result_trimmed <- trimws(result)

  # Define the expected output without trailing spaces
  expected_output <- c("Travel from A to B: 100 km", "Travel from B to C: 150 km")

  # Check if the trimmed result matches the expected output
  expect_equal(result_trimmed, expected_output)
})
