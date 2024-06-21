# Install and load the lubridate package
if (!require(dplyr)) install.packages("lubridate")

library(lubridate)

# Define the function to standardize dates
standardize_dates <- function(df, date_columns) {
  for (col in date_columns) {
    # Create a new column name for the standardized date
    new_col <- paste0("standardized_", col)
    
    # Use lubridate functions to standardize dates
    df[[new_col]] <- case_when(
      grepl("^\\d{4}-\\d{2}-\\d{2}$", df[[col]]) ~ ymd(df[[col]]),          # For "Year-Month-Day" formats
      grepl("^\\d{4}/\\d{2}/\\d{2}$", df[[col]]) ~ ymd(gsub("/", "-", df[[col]])),  # Convert "/" to "-" and then parse
      grepl("^\\w+ \\d{1,2}, \\d{4}$", df[[col]]) ~ mdy(df[[col]]),          # For "Month Day, Year" formats
      grepl("^\\d{2}/\\d{2}/\\d{4}$", df[[col]]) ~ dmy(df[[col]]),           # For "Day/Month/Year" formats
      grepl("^\\d{2}-\\w+-\\d{4}$", df[[col]]) ~ dmy(df[[col]]),             # For "Day-Month-Year" formats with month names
      grepl("^\\d{8}$", df[[col]]) ~ ymd(df[[col]]),                        # For "YYYYMMDD" formats
      TRUE ~ NA_Date_                                                       # For any other format, set as NA
    )
  }
  return(df)
}

# Sample data frame with various date formats
# dates <- data.frame(
#   date1 = c("2021-06-21", "2021/06/22", "June 23, 2021", "InvalidDate"),
#   date2 = c("21/06/2021", "22-June-2021", "20210623", "23/06/2021"),
#   stringsAsFactors = FALSE
# )
# 
# # Use the function to standardize the dates
# standardized_dates <- standardize_dates(dates, c("date1", "date2"))
# 
# # Print the result
# print("Standardized Data:")
# print(standardized_dates)
