# Load the necessary package
# install.packages("data.table")
library(data.table)

# Define the function to find null values in each column of a data.table
find_null_values <- function(dt) {
  null_counts <- sapply(dt, function(col) sum(is.na(col)))
  null_summary <- data.table(
    column = names(null_counts),
    null_count = null_counts,
    null_percentage = (null_counts / nrow(dt)) * 100
  )
  return(null_summary)
}

# Example usage
# Create a sample data.table with some NA values
dt <- data.table(
  col1 = c(1, 2, NA, 4, 5),
  col2 = c(NA, "b", "c", NA, "e"),
  col3 = c(TRUE, NA, FALSE, TRUE, NA)
)

# Use the function to find null values
null_values_summary <- find_null_values(dt)

# Print the result
print("Summary of Null Values in Columns:")
print(null_values_summary)
