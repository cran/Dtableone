library(testthat)
library(Dtableone) # Assumption: Dtableone is the package name and contains the CreateTableD2 function.

# Create example data
# Create a simple dataframe that resembles the structure of real data.
example_data <- data.frame(
  y1 = c(1, 0, 1, 1, 0, 0, 1, 0),
  y2 = c(1, 1, 0, 1, 0, 0, 1, 0),
  d = c(1, 1, 1, 1, 0, 0, 1, 0)
)

test_that("CreateTableD2 returns a list of data frames", {
  result <- CreateTableD2(example_data, my.printlayout = TRUE)

  # Check if the result is a list format:
  expect_true(is.list(result))

  # Check if each element of the list is a dataframe:
  expect_true(all(sapply(result, is.data.frame)))

  # Check if the list contains specific keys (e.g., 'Diseased', 'Non-diseased', 'Comparison'):
  expect_true(all(c("Diseased", "Non-diseased", "Comparison") %in% names(result)))

  # Check if the 'Comparison' dataframe includes the expected columns:
  expect_true(all(c("Modality 1", "Modality 2", "P-value") %in% names(result$Comparison)))
})
