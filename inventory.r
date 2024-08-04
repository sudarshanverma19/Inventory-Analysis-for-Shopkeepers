library(dplyr)
library(readxl)
library(ggplot2)
library(caret)
library(tidyverse)
library(randomForest)
cat("welcome to AMAZON DATA FACTORY ")
N<-as.numeric(readline(prompt="please enter your product id in range 1001-1050"))
while(TRUE){
print("press 1 for over all products sales analysis")
print("press 2 for over all products sales report")
print("press 3 for for your product sales report")
print("press 4 for your product sales report yearwise")
print("press 5 for your product sales analysis yearwise")
print("press 6 for your product monthwise sales report")
print("press 7 for your product monthwise sales analysis")
print("press 8 for sales report of all products warehouse wise")
print("press 9 for sales analysis of all products warehousewise")
print("press 10 for your product sales report  warehouse wise")
print("press 11 for your product sales analysis warehousewise")
print("press 12 for your product sales report from a particular warehouse id")
print("press 13 to know your product sales by invoice date")
print("press 14 to know your product sales report by invoice date followed by warehouse")
print("press 15 to know your product sales in future by entering date")
print("press 16 to know your product sales in future by entering year")
print("press 17 to know your product sales in future in a specific year in a specific month")
print("press 0 to exit")
M<-readline(prompt("enter your option from above"))
data<-read_excel("C:\\Users\\garla\\OneDrive\\Desktop\\Book1.xlsx",sheet=1,col_names=TRUE)
data<-data[,1:4]
data$INVOICE_DATE<-as.Date(data$INVOICE_DATE)
data <- data %>%
  mutate(YEAR = lubridate::year(INVOICE_DATE),
         MONTH = lubridate::month(INVOICE_DATE, label = TRUE))

if(M=="0"){
  print("program exciting ")
  break
}
if(M=="1"){
  sales_summary <- aggregate(data$SALES_COUNT, by = list(data$PRODUCT_ID), FUN = sum)
  plotting<-ggplot(sales_summary, aes(x = Group.1, y = x)) +
    geom_bar(stat = "identity",color="black",fill="pink") +
    labs(x = "Product ID", y = "Total Sales")+ggtitle('overa all sales analysis of products')
  print(plotting)
}
if(M=="2"){
  total_sales <- tapply(data$SALES_COUNT,data$PRODUCT_ID, sum)
  result_sales_df <- data.frame(product_id = as.integer(names(total_sales)),
                                sales_count = as.integer(total_sales))
  print("here is the sales report of products thar were sold in amazon from begining")
  print(result_sales_df)
}
if(M=="3"){
  product_data <- data[data$PRODUCT_ID == N, ]
  print("here is the sales report of your product")
  print(product_data)
}
if(M=="4"){
  filtered_data <- subset(data, PRODUCT_ID ==N)
  total_sales <- filtered_data %>% 
    group_by(YEAR) %>% 
    summarize(TotalSales = sum(SALES_COUNT))
  print("here is yaerwise sales report of your product")
print(total_sales)}
if(M=="5"){
  filtered_data <- subset(data, PRODUCT_ID ==N)
  total_sales <- filtered_data %>% 
    group_by(YEAR) %>% 
    summarize(TotalSales = sum(SALES_COUNT))
  PLOT<-ggplot(total_sales, aes(x =YEAR, y = TotalSales)) +
    geom_bar(stat = "identity",color="orange",fill="blue") +
    labs(x = "YEAR", y = "TotalSales")+ggtitle(paste("year wise sales report of product id",N))
  print(PLOT)
}
if(M=="6"){
  Y<-readline(prompt("enter the year in which you wants to know monthwise sales report"))
  filtered_data <- subset(data, PRODUCT_ID ==N)
  total_sales <- filtered_data %>% 
    filter(YEAR==Y) %>% 
    group_by(MONTH)%>%
    summarize(TotalSales = sum(SALES_COUNT))
  print("here is the monthwise sales report of your product")
  print(total_sales)
}
if(M=="7"){
  Y<-readline(prompt("enter the year in which you wants to know monthwise sales analysis"))
  filtered_data <- subset(data, PRODUCT_ID ==N)
  total_sales <- filtered_data %>% 
    filter(YEAR==Y) %>% 
    group_by(MONTH)%>%
    summarize(TotalSales = sum(SALES_COUNT))
  PLOT<-ggplot(total_sales, aes(x =MONTH, y = TotalSales)) +
    geom_bar(stat = "identity",color="black",fill="green",width=0.5) +
    labs(x = "MONTH", y = "TotalSales")+ggtitle(paste("MONTH wise sales report of product id",N,"IN YEAR",Y))
  print(PLOT)
}
if(M=="8"){
  total_sales <- tapply(data$SALES_COUNT,data$WAREHOUSE_ID, sum)
  result_sales_df <- data.frame(WAREHOUSE_id = names(total_sales),
                                sales_count = as.integer(total_sales))
  print("here is the warehousewise sales report of all products")
  print(result_sales_df)
}
if(M=="9"){
  total_sales <- tapply(data$SALES_COUNT,data$WAREHOUSE_ID, sum)
  result_sales_df <- data.frame(WAREHOUSE_id = names(total_sales),
                               sales_count = as.integer(total_sales))
  PLOT<-ggplot(result_sales_df, aes(x =WAREHOUSE_id, y = sales_count)) +
    geom_bar(stat = "identity",color="orange",fill="darkblue") +
    labs(x = "WAREHOUSE ID", y = "TotalSales")+ggtitle("WAREHOUSEWISE wise sales analysis of all products")
  print(PLOT)
}
  
if(M=="10"){
  filtered_data <- subset(data, PRODUCT_ID ==N)
  total_sales <- filtered_data %>% 
    group_by(WAREHOUSE_ID)%>%
    summarize(TotalSales = sum(SALES_COUNT))
  print("here is warehousewise sales report of your product")
  print(total_sales)}

if(M=="11"){
  total_sales <- filtered_data %>% 
    group_by(WAREHOUSE_ID)%>%
    summarize(TotalSales = sum(SALES_COUNT))
  PLOT<-ggplot(total_sales, aes(x =WAREHOUSE_ID, y = TotalSales)) +
    geom_bar(stat = "identity",color="red",fill="yellow") +
    labs(x = "WAREHOUSE ID", y = "TotalSales")+ggtitle(paste("WAREHOUSEWISE wise sales analysis of product id",N))
  print(PLOT)}
if(M=="12"){
  filtered_data <-subset(data,PRODUCT_ID ==N)
  n=readline(promp="enter the ware house id")
  ware_data<-subset(filtered_data,WAREHOUSE_ID==n)
  print(ware_data)
}
if(M=="13"){
  filtered_data <-subset(data,PRODUCT_ID ==N)
  n=as.Date(readline(promp="enter the invoice date in format YYYY-MM-DD"))
  sort_data<-subset(filtered_data,INVOICE_DATE==n)
  print(paste("here is the sales report of your product on",n))
  print(sort_data)
  
}
if(M=="14"){
  filtered_data <-subset(data,PRODUCT_ID ==N)
  n=as.Date(readline(promp="enter the invoice date in format YYYY-MM-DD"))
  sort_data<-subset(filtered_data,INVOICE_DATE==n)
  n1=readline(prompt="enter a ware house in format W001-W020")
  sub_sort<-subset(sort_data,WAREHOUSE_ID==n1)
  print(paste("here is the sales report of your product on",n,"from warehouse",n1))
  print(sub_sort)
  
}


if(M=="15"){
predict_future_sales <- function(data,future_date) {
  product_data <- subset(data,PRODUCT_ID==N)
  model <- lm(SALES_COUNT~INVOICE_DATE, data = product_data)
  future_date <- as.Date(future_date)
  future_date_data <- data.frame(INVOICE_DATE= future_date)
  future_sales <- predict(model, newdata = future_date_data)
  return(future_sales)}
user_date1 <-readline("Enter a future date (YYYY-MM-DD): ")
user_date<-as.Date(user_date1)
#product_name <- readline("Enter the product name: ")
predicted_sales <- predict_future_sales(data,user_date)
if(predicted_sales<0){
  predicted_sales=0
}
cat("Predicted sales for", N, "on", user_date1, ":",as.integer(predicted_sales), "\n")}

if(M=="16"){
predict_future_sales <- function(data, future_date) {
  product_data <- subset(data,PRODUCT_ID==N)
  model <- lm(SALES_COUNT~YEAR, data = product_data)
  #future_date <- as.Date(future_date)
  future_date_data <- data.frame(YEAR= future_date)
  future_sales <- predict(model, newdata = future_date_data)
  return(future_sales)}
user_date1<-readline("Enter a future date (YYYY): ")
user_date<-as.numeric(user_date1)
#user_date<-as.Date(user_date1)
#product_name <- readline("Enter the product name: ")
predicted_sales <- predict_future_sales(data,user_date)
if(predicted_sales<0){
  predicted_sales=0
}
cat("Predicted sales for",N, "on", user_date1, ":",as.integer(predicted_sales), "\n")}

if(M=="17"){
  predict_future_sales <- function(data, future_date,future_month) {
    tryCatch({
    product_data <- subset(data, PRODUCT_ID == N)
    product_data$MONTH <- as.factor(product_data$MONTH)
    product_data$MONTH <- factor(product_data$MONTH, levels = unique(c(levels(product_data$MONTH), future_month)))
    model <- lm(SALES_COUNT ~ YEAR + MONTH, data = product_data)
    if (nrow(product_data) == 0) {
      cat("No data found for PRODUCT_ID", N, "\n")
      return(0)}
   future_date_data <- data.frame(YEAR = as.numeric(future_date), MONTH = as.factor(future_month))
    future_sales <- predict(model, newdata = future_date_data)
    if (!all(future_month %in% levels(product_data$MONTH))) {
      future_sales=0 }
    },error=function(e){
      future_sales=0
      return(future_sales)
    })
  }
  user_date1 <- readline("Enter a future year (YYYY): ")
  user_date <- as.numeric(user_date1)
  user_month<-readline(prompt="enter a month name in capital letters")
  predicted_sales <- predict_future_sales(data, user_date,user_month)
  
  cat("Predicted sales for",N, "on", user_date,user_month, ":",as.integer(predicted_sales), "\n")}}
print(data)

