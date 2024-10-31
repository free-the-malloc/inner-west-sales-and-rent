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
  sales_df[,6:10] <- lapply(sales_df[,6:10],clean_comma)
  sales_df[,11:14] <- lapply(sales_df[,11:14],clean_percentage)
  sales_df
}

sales_df <- get_dataframes(2018:2024,"sales","LGA")
rents_df <- get_dataframes(2018:2024,"rent","LGA")

sales_df = sales_df |> clean_sales_df()
rents_df = rents_df |> clean_rent_df()

# Get RBA cash rate data and merge into sales and rents dataframe

RBA_cash_rate <- read_excel("data/RBA-cash-rate-august-2024.xlsx",sheet="Data")
RBA_cash_rate <- RBA_cash_rate[-(1:11), -(3:18)]
RBA_cash_rate[,2] <- lapply(RBA_cash_rate[,2],as.numeric)
RBA_cash_rate[,1] <- lapply(RBA_cash_rate[,1],as.Date,"%d-%b-%Y")
colnames(RBA_cash_rate) <- c("date","cash_rate")

RBA_cash_rate_mod = RBA_cash_rate |>
  dplyr::filter(
    (month(date) == 3 | month(date) == 6 | month(date) == 9 | month(date) == 12) &
      date > as.Date("2018-01-01")
  ) |>
  group_by(strftime(date,"%Y-%m")) |>
  dplyr::filter(date == max(date)) |>
  dplyr::mutate(
    date = case_when(
      month(date) == 3 ~ as.Date(sprintf("%s-03-31",year(date))),
      month(date) == 6 ~ as.Date(sprintf("%s-06-30",year(date))),
      month(date) == 9 ~ as.Date(sprintf("%s-09-30",year(date))),
      month(date) == 12 ~ as.Date(sprintf("%s-12-31",year(date))),
    )
  )

RBA_cash_rate_mod = RBA_cash_rate_mod[,-3]

sales_df <- merge(x = sales_df,y=RBA_cash_rate_mod,by="date")
rents_df <- merge(x = rents_df,y=RBA_cash_rate_mod,by="date")

abs_earnings <- read_excel("data/ABS-weekly-earnings-trend.xlsx",sheet="Data1")
abs_earnings <- abs_earnings[-(1:9),10]
abs_earnings$date <- seq(as.Date('2012-07-01'), length.out=25, by='6 month') - days(1)
colnames(abs_earnings) <- c("nsw_average_total_earnings","date")

sales_df <- merge(sales_df,y = abs_earnings,by="date",all.x = TRUE)
rents_df <- merge(rents_df,y = abs_earnings,by="date",all.x = TRUE)

save(sales_df,file = "data/rdata/sales_df.RData")
save(rents_df,file = "data/rdata/rents_df.RData")
save(RBA_cash_rate, file = "data/rdata/rba_cash_rate.RData")
