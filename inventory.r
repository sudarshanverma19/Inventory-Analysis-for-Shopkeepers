# Load required libraries
library(dplyr)
library(lubridate)

# Load the CSV file
data <- read.csv("C:\\Users\\Sud\\Desktop\\Book1.csv", stringsAsFactors = FALSE)
data$INVOICE_DATE <- as.Date(data$INVOICE_DATE)

# Add YEAR and MONTH columns (if not already in CSV)
data <- data %>%
  mutate(YEAR = year(INVOICE_DATE),
         MONTH = month(INVOICE_DATE, label = TRUE))

# User inputs
N <- as.integer(readline(prompt = "Enter product ID: "))

# Functions
predict_future_sales_15 <- function(data, future_date) {
  product_data <- subset(data, PRODUCT_ID == N)
  product_data$INVOICE_DATE <- as.numeric(product_data$INVOICE_DATE)  # Important
  model <- lm(SALES_COUNT ~ INVOICE_DATE, data = product_data)
  future_date <- as.numeric(future_date)
  future_date_data <- data.frame(INVOICE_DATE = future_date)
  future_sales <- predict(model, newdata = future_date_data)
  return(future_sales)
}

predict_future_sales_16 <- function(data, future_year) {
  product_data <- subset(data, PRODUCT_ID == N)
  model <- lm(SALES_COUNT ~ YEAR, data = product_data)
  future_year_data <- data.frame(YEAR = as.numeric(future_year))
  future_sales <- predict(model, newdata = future_year_data)
  return(future_sales)
}

predict_future_sales_17 <- function(data, future_date, future_month) {
  future_sales <- tryCatch({
    product_data <- subset(data, PRODUCT_ID == N)
    if (nrow(product_data) == 0) {
      cat("No data found for PRODUCT_ID", N, "\n")
      return(0)
    }
    product_data$MONTH <- as.factor(product_data$MONTH)
    product_data$MONTH <- factor(product_data$MONTH, levels = unique(c(levels(product_data$MONTH), future_month)))
    model <- lm(SALES_COUNT ~ YEAR + MONTH, data = product_data)
    future_date_data <- data.frame(YEAR = as.numeric(future_date), MONTH = as.factor(future_month))
    predict(model, newdata = future_date_data)
  }, error = function(e) {
    0
  })
  return(future_sales)
}

# Menu loop
while (TRUE) {
  cat("\nMenu:\n")
  cat("15. Predict sales based on INVOICE_DATE\n")
  cat("16. Predict sales based on YEAR\n")
  cat("17. Predict sales based on YEAR and MONTH\n")
  cat("18. Exit\n")
  
  choice <- as.integer(readline(prompt = "Enter your choice (15, 16, 17, 18): "))
  
  if (choice == 15) {
    user_date1 <- readline(prompt = "Enter a future date (YYYY-MM-DD): ")
    future_date <- as.Date(user_date1)
    predicted_sales <- predict_future_sales_15(data, future_date)
    cat("Predicted sales for Product ID", N, "on", future_date, ":", predicted_sales, "\n")
    
  } else if (choice == 16) {
    user_date2 <- readline(prompt = "Enter a future year (YYYY): ")
    future_year <- as.numeric(user_date2)
    predicted_sales <- predict_future_sales_16(data, future_year)
    cat("Predicted sales for Product ID", N, "in year", future_year, ":", predicted_sales, "\n")
    
  } else if (choice == 17) {
    user_date3 <- readline(prompt = "Enter a future year (YYYY): ")
    future_year <- as.numeric(user_date3)
    future_month <- readline(prompt = "Enter future month (e.g., Jan, Feb, etc.): ")
    predicted_sales <- predict_future_sales_17(data, future_year, future_month)
    cat("Predicted sales for Product ID", N, "in", future_month, future_year, ":", predicted_sales, "\n")
    
  } else if (choice == 18) {
    cat("Exiting the program.\n")
    break
    
  } else {
    cat("Invalid choice. Please try again.\n")
  }
}



