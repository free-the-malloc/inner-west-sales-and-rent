library(tidyverse)
library(readxl)

months <- list("march","june","september","december")

add_month_df <- function(month,year,rent_or_sales,lga_or_postcode) {
  filename <- sprintf("data/dcj_%s/%s/%s-%s-%s.xlsx",rent_or_sales,year,year,month,rent_or_sales)
  if (file.exists(filename) == FALSE){
    return(NULL)
  }
  sheet <- read_excel(filename, sheet = lga_or_postcode)
  sheet <- sheet[rowSums(is.na(sheet[ , 2:(length(sheet))]))  != (length(sheet) - 1), ]
  
  colnames(sheet) <- NULL
  sheet <- sheet[-1, , drop=FALSE]
  sheet$date <- as.Date(sprintf("01 %s %s",month,year),format="%d %B %Y") + months(1) - days(1)
  sheet
}

add_year_df <- function(year,rent_or_sales,lga_or_postcode){
  year_list <- lapply(months,add_month_df,year,rent_or_sales,lga_or_postcode)
  year_df <- bind_rows(year_list)
  year_df
}

get_dataframes <- function(year_range,rent_or_sales,lga_or_postcode){
  df <- lapply(year_range,add_year_df,rent_or_sales,lga_or_postcode)
  final_df <- bind_rows(df)
  final_df
}

clean_comma <- function(df){
  new_df = as.numeric(gsub(",","",df))
  new_df
}
clean_percentage <- function(df){
  new_df = as.numeric(gsub("%","",df))
  new_df
}

clean_rent_df <- function(rent_df){
  rent_column_names <- c("gmr","greater_sydney","rings","lga","dwelling_type","no_of_bedrooms","first_quartile_rent_price","median_rent_price","third_quartile_rent_price","no_of_bonds_new","no_of_bonds_total","qtly_change_rent","annual_change_rent","qtly_change_bonds_new","annual_change_bonds_new","date")
  colnames(rent_df) <- rent_column_names
  rent_df <- janitor::clean_names((rent_df))
  rent_df[,7:11] <- lapply(rent_df[,7:11],clean_comma)
  rent_df[,12:15] <- lapply(rent_df[,12:15],clean_percentage)
  rent_df
}

clean_sales_df <- function(sales_df){
  sales_column_names <- c("gmr","greater_sydney","rings","lga","dwelling_type","first_quartile_sales_price","median_sales_price","third_quartile_sales_price","mean_sales_price","no_of_sales","qtly_change_price","annual_change_price","qtly_change_count","annual_change_count","date")
  colnames(sales_df) <- sales_column_names
  #sales_df[,6:14] <- as.numeric(gsub(",","",sales_df[,6:14]))14
  sales_df[,6:10] <- lapply(sales_df[,6:10],clean_comma)
  sales_df[,11:14] <- lapply(sales_df[,11:14],clean_percentage)
  sales_df
}

sales_df <- get_dataframes(2018:2024,"sales","LGA") |> clean_sales_df()
rents_df <- get_dataframes(2018:2024,"rent","LGA") |> clean_rent_df()

save(sales_df,file = "data/rdata/sales_df.RData")
save(rents_df,file = "data/rdata/rents_df.RData")
