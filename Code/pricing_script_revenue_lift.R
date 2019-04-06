# MKT 680 pricing project
setwd("C:\\Users\\sahaa\\OneDrive - Emory University\\Spring - Marketing Analytics\\Project - Pricing")

# loading packages ------------------------------------------------------

library(arules)
library(data.table)
library(dplyr)
library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)


# reading in data and merging -------------------------------------------

transactions <- fread("transaction_table.csv")
products <- fread("product_table.csv")

data <- merge(transactions, products, by='prod_id', all=TRUE)


# cleaning ----------------------------------------------------------------

# data <- data[data$brand_desc != 'FRUTAS&VEGETAIS']
# data <- data[data$category_desc != 'BANANA']
# data <- data[data$category_desc_eng != 'APPLE']
# 
# fresh_products <- unique(data[data$category_desc_eng %like% "FRESH "]$category_desc_eng)
# data <- data[!(data$category_desc_eng %in% fresh_products)]
# data <- data[!(data$prod_unit %like% 'KG')]
# 
# # minimum of 10 products in category 
# data[, num_prod := length(unique(prod_id)), by=category_id]
# #data <- data[data$num_prod > 9]
# 
# # minimum of 10 stores
# data[, num_stores := length(unique(store_id)), by = prod_id]
# data <- data[data$num_stores > 9]
# 
# 
# # first week in apr 2017 --------------------------------------------------
# 
# 
# data$tran_dt <- as.Date(data$tran_dt)
# 
# fst_wk_apr <- data[data$tran_dt >= "2017-04-01" & data$tran_dt <= "2017-04-07"]
# # Finding the transactions in last week of march 2017
# last_wk_mar <- data[data$tran_dt >= "2017-03-25" & data$tran_dt <= "2017-03-31"]
# 
# # Product store combination for first week of april
# prod_store_price <- fst_wk_apr[,.(mean(prod_unit_price),
#                                   sum(tran_prod_sale_qty)), by=.(prod_id, store_id)]
# colnames(prod_store_price) <- c("prod_id", "store_id", "fst_wk_apr_price", "qty_sales_apr")
# 
# # Product store combination for last week of March
# prod_store_price_mar <- last_wk_mar[,.(mean(prod_unit_price),
#                                        sum(tran_prod_sale_qty)), by=.(prod_id, store_id)]
# colnames(prod_store_price_mar) <- c("prod_id", "store_id", "lst_wk_mar_price", "qty_sales_mar")
# 
# prod_store_price <- merge(prod_store_price, prod_store_price_mar, by=c("prod_id", "store_id"), all=TRUE)
# 
# # Product sales and mean price in first week of april
# prod_price <- fst_wk_apr[,.(mean(prod_unit_price),
#                             sum(tran_prod_sale_qty)), by=.(prod_id)]
# colnames(prod_price) <- c("prod_id", "fst_wk_apr_price", "qty_sales_apr")
# 
# # Product sales and mean price in last week of march
# prod_price_mar <- last_wk_mar[,.(mean(prod_unit_price),
#                                  sum(tran_prod_sale_qty)), by=.(prod_id)]
# colnames(prod_price_mar) <- c("prod_id", "lst_wk_mar_price", "qty_sales_mar")
# 
# prod_price <- merge(prod_price, prod_price_mar, by=c("prod_id"), all=TRUE)
# prod_price$price_diff <- prod_price$fst_wk_apr_price - prod_price$lst_wk_mar_price
# prod_price$diff_percent <- prod_price$price_diff / prod_price$lst_wk_mar_price
# summary(prod_price$diff_percent)
# hist(prod_price$diff_percent, breaks = 20, xlab = "Percentage price changes", ylab = "Count of products", 
#      main = "Histogram of price changes between Mar 25-31 & Apr 1-7 in 2017")
# prod_price$qty_diff <- prod_price$qty_sales_apr - prod_price$qty_sales_mar
# prod_price$qty_diff_percent <- prod_price$qty_diff / prod_price$qty_sales_mar
# summary(prod_price$qty_diff_percent)
# # Removing outliers to better view the histogram
# prod_price <- prod_price[!prod_price$qty_diff_percent == 493]
# prod_price <- prod_price[!prod_price$qty_diff_percent == 309]
# prod_price <- prod_price[!prod_price$qty_diff_percent == 176]
# prod_price <- prod_price[!prod_price$qty_diff_percent == 157]
# prod_price <- prod_price[!prod_price$qty_diff_percent == 142]
# prod_price <- prod_price[!prod_price$qty_diff_percent == 122]
# summary(prod_price$qty_diff_percent)
# hist(prod_price$qty_diff_percent, breaks = 20, xlab = "Percentage quantity sold changes", 
#      ylab = "Count of products", main = "Histogram of demand changes between Mar 25-31 & Apr 1-7 in 2017")
# 
# 
# # fst_wk_apr2016 <- data[data$tran_dt >= "2016-04-01" & data$tran_dt <= "2016-04-07"]
# 
# 
# 
# unique(fst_wk_apr$tran_prod_discount_amt)
# sort(unique(fst_wk_apr$brand_desc))
# 
# 
#  
# apr2017_sale_prices <- data.table(fst_wk_apr[,c(1,5,12)])
# apr2017_sale_prices_n <- apr2017_sale_prices[, mean(prod_unit_price), by = .(prod_id, store_id)]
# 
# data1 <- merge(data, apr2017_sale_prices_n, by = c("prod_id", "store_id"),all.x = TRUE)
# 
# data1 <- data1[!is.na(data1$V1)]
# 
# data1$diff <- data1$prod_unit_price - data1$V1
# 
# test <- data1[data1$diff < -4]
# 
# 
# # trying arules -----------------------------------------------------------
# 
# sample <- sample_n(data, 100000)
# 
# transactionData <- ddply(sample,c("cust_id","tran_id","tran_dt"),
#                          function(df1)paste(df1$prod_id,
#                                             collapse = ","))
# 
# #set column InvoiceNo of dataframe transactionData  
# transactionData$cust_id <- NULL
# #set column Date of dataframe transactionData
# transactionData$tran_id <- NULL
# transactionData$tran_dt <- NULL
# #Rename column to items
# colnames(transactionData) <- c("items")
# 
# 
# write.csv(transactionData,'transaction_data.csv')
# tr <- read.transactions('transaction_data.csv', format = 'basket', sep=',')
# #rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
# 
# affinity_mat <- affinity(tr)
# 
# 
# 
# # response functions ------------------------------------------------------
# 
# sample <- sample_n(data, 1000000)
# 
# product_list <- unique(data$prod_id)
# store_list <- unique(data$store_id)
# elasticity_vec <- list()
# 
# for (i in 1:length(product_list)){
#   temp <- data[data$prod_id == product_list[i]]
#   temp_list <- unique(temp$prod_unit_price)
#   price_vec <-c()
#   demand_vec <- c()
#   for (j in 1:length(temp_list)){
#     demand <- mean(temp$tran_prod_sale_qty[temp$prod_unit_price == temp_list[j]])
#     price_vec <-c(price_vec,temp_list[j])
#     demand_vec <- c(demand_vec,demand)
#   }
#   temp_data <- as.data.frame(cbind(price_vec, demand_vec))
#   slope <- lm(demand_vec ~ price_vec, data = temp_data)$coefficients[[2]]
#   mean_price <- mean(price_vec)
#   mean_demand <- mean(demand_vec)
#   elasticity_vec[i] <- mean_price*slope/mean_demand
# }
# 
# ev <- data.frame(matrix(unlist(elasticity_vec), nrow=length(elasticity_vec), byrow=T))
# ev 
# 
# product_info <- data.table(product_list)
# product_info$ev <- ev
# 
# test <- merge(data, product_info,by.x = 'prod_id',by.y = 'product_list')
# 
# test1 <- test[,mean(ev), by = .(prod_id, category_desc_eng, sub_category_desc)]
# test2 <- test1[(test1$V1 <= 0)]
# test3 <- test1[(test1$V1 <= -.3)]
# 
# count_by_category <- test3[, .N, by = category_desc_eng]
# 
# wine_yogurt <- test3[(test3$category_desc_eng == 'FINE WINES') | test3$category_desc_eng == 'YOGURT HEALTH']
# 
# 
# # Seasonality: Category weekly average sales -------------------------------------------
# 
# data$week <- strftime(data$tran_dt, format = "%V")
# data$year <- year(data$tran_dt)
# category_weekly_sales <- data[, sum(tran_prod_sale_qty), by=c('year', 'week', 'category_id')]
# colnames(category_weekly_sales) <- c('year', 'week', 'category_id', 'total_sales')
# category_avg_weekly_sales <- category_weekly_sales[,mean(total_sales), by = c('week', 'category_id')] 
# colnames(category_avg_weekly_sales) <- c('week', 'category_id', 'avg_sales')
# category_weekly_sales <- merge(category_weekly_sales, category_avg_weekly_sales, 
#                                by = c('week', 'category_id'))
# 
# write.csv(category_weekly_sales,"category_weekly_sales.csv")
# 
# 
# # Price of complements and substitutes ------------------------------------
# 
# complements <- fread("complements.csv")
# substitues <- fread("substitutes.csv")
# 
# max(data$tran_dt)
# 
# data_complements <- data[prod_id %in% complements$complements]
# data_substitutes <- data[prod_id %in% substitues$substitute]
# 
# data_complements <- data_complements[tran_dt >= "2017-12-25" & tran_dt <= "2017-12-31"]
# complements_price <- data_complements[,mean(prod_unit_price), by=prod_id]
# colnames(complements_price) <- c('prod_id', 'avg_price')
# 
# write.csv(complements_price, "complements_price.csv")
# 
# unique(substitues$substitute)
# 
# data_substitutes <- data_substitutes[tran_dt >= "2016-01-01" & tran_dt <= "2017-12-31"]
# substitutes_price <- data_substitutes[,mean(prod_unit_price), by=prod_id]
# colnames(substitutes_price) <- c('prod_id', 'avg_price')
# write.csv(substitutes_price, "substitutes_price.csv")


# Calculating sales and revenue increase ----------------------------------

wine_yogurt <- fread("wine_yogurt.csv")
top_stores <- fread("store_info.csv")
wine_yogurt <- wine_yogurt[order(elasticity),]
wine_yogurt <- wine_yogurt[1:100,]
top_stores <- top_stores[order(-store_revenue)]

# Considering only top 10 stores (in revenue & quantity sales) for analysis
top_10_stores <- top_stores[1:10,]

top_10_store_id <- c(349, 346, 343, 342, 345, 588, 335, 344, 347, 398)

top_10_stores_transactions <- data[prod_id %in% wine_yogurt$prod_id]
top_10_stores_transactions <- top_10_stores_transactions[store_id %in% top_10_store_id]
top_10_stores_transactions$week <- strftime(top_10_stores_transactions$tran_dt, format = "%V")

prod_store_weekly_sales <- top_10_stores_transactions[,.(sum(tran_prod_sale_qty),
                                                         mean(prod_unit_price)), 
                                                      by=c('prod_id', 'store_id', 'week')]
colnames(prod_store_weekly_sales) <- c('prod_id', 'store_id', 'week', 'sales_qty', 'avg_unit_price')

prod_store_weekly_sales[,max_week:= max(week), by=.(prod_id,store_id)]

prod_store_latest_sales <- prod_store_weekly_sales[max_week == week,]
prod_store_latest_sales$week <- NULL
prod_store_latest_sales$max_week <- NULL

product_list <- wine_yogurt$prod_id
store_list <- top_10_store_id

all_prod_stores <- merge(product_list, store_list, all=TRUE)

colnames(all_prod_stores) <- c('prod_id', 'store_id')

prod_store_latest_sales_all <- merge(x=all_prod_stores, y=prod_store_latest_sales, 
                                     by=c('prod_id', 'store_id'), all=TRUE)

write.csv(prod_store_latest_sales_all, "prod_store_latest_sales_all.csv")

# prod_store_weekly_sales1[,avg_price := median(avg_unit_price), by = .(prod_id, store_id)]
# 
# prod_store_latest_sales_all1 <- merge(x=prod_store_latest_sales_all, y=prod_store_weekly_sales1,
#                                       by=c('prod_id', 'store_id'))
# prod_store_latest_sales_all2 <- merge(x=prod_store_latest_sales_all, y=prod_store_latest_sales_all1,
#                                       by.x = c('prod_id, store_id'), by.y)
# write.csv(prod_store_weekly_sales, "prod_store_weekly_sales.csv")
