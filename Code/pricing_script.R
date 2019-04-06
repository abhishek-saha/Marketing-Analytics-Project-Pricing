# MKT 680 pricing project
setwd("~/Desktop/Emory/Spring 2019/MKT 680 - Marketing Analytics/Pernalonga Projects")

# loading packages ------------------------------------------------------

library(arules)
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)


# reading in data and merging -------------------------------------------

transactions <- fread("transaction_table.csv")
products <- fread("product_table.csv")
data <- merge(transactions, products, by='prod_id', all=TRUE)


# cleaning ----------------------------------------------------------------

#if a product has the word fresh in its name, then filter it out of the data
fresh_products <- unique(data[data$category_desc_eng %like% "FRESH "]$category_desc_eng)
data <- data[!(data$category_desc_eng %in% fresh_products)]

# removing all products that are measured in kg
data <- data[data$prod_unit != 'KG']


# minimum of 10 products in a category 
data[, num_prod := length(unique(prod_id)), by=category_id]
data <- data[data$num_prod > 9]

# product appears in a minimum of 10 stores
data[, num_stores := length(unique(store_id)), by = prod_id]
data <- data[data$num_stores > 9]


data <- data[data$brand_desc != 'FRUTAS&VEGETAIS']
data <- data[data$category_desc != 'BANANA']
data <- data[data$category_desc_eng != 'APPLE']


# analyzing first week in apr 2017 --------------------------------------------


data$tran_dt <- as.Date(data$tran_dt)

fst_wk_apr <- data[data$tran_dt >= "2017-04-01" & data$tran_dt <= "2017-04-07"]

# for comparison, look at first week in Apr 2016 as well
fst_wk_apr2016 <- data[data$tran_dt >= "2016-04-01" & data$tran_dt <= "2016-04-07"]

# discounts present
unique(fst_wk_apr$tran_prod_discount_amt)
#list of brands sold
sort(unique(fst_wk_apr$brand_desc))

# filtering for only relevant sales info
apr2017_sale_prices <- data.table(fst_wk_apr[,c(1,5,12)])

#calculating the average unit price for that week per product and store
apr2017_sale_prices_n <- apr2017_sale_prices[, mean(prod_unit_price), by = .(prod_id, store_id)]


data1 <- merge(data, apr2017_sale_prices_n, by = c("prod_id", "store_id"),all.x = TRUE)

data1 <- data1[!is.na(data1$V1)]

data1$diff <- data1$prod_unit_price - data1$V1

test <- data1[data1$diff < -4]


# response functions ------------------------------------------------------

#sample <- sample_n(data, 1000000)

product_list <- unique(data$prod_id)
product_list1 <- unique(wine_yogurt$prod_id)
store_list <- unique(data$store_id)
elasticity_vec <- list()
alphas <-c()

for (i in 1:length(product_list1)){
  temp <- data[data$prod_id == product_list1[i]]
  temp_list <- unique(temp$prod_unit_price)
  price_vec <-c()
  demand_vec <- c()
  for (j in 1:length(temp_list)){
    demand <- sum(temp$tran_prod_sale_qty[temp$prod_unit_price == temp_list[j]])
    price_vec <-c(price_vec,temp_list[j])
    demand_vec <- c(demand_vec,demand)
  }
  temp_data <- as.data.frame(cbind(price_vec, demand_vec))
  slope <- lm(demand_vec ~ price_vec, data = temp_data)$coefficients[[2]]
  alpha <- lm(demand_vec ~ price_vec, data = temp_data)$coefficients[[1]]
  alphas <- c(alphas, alpha)
  mean_price <- mean(price_vec)
  mean_demand <- mean(demand_vec)
  elasticity_vec[i] <- mean_price*slope/mean_demand
}

colnames(slopes) <- c('product_id','alpha')

#write.csv(slopes, "product_alphas1.csv")


elasticity_values <- data.frame(matrix(unlist(elasticity_vec), nrow=length(elasticity_vec), byrow=T))
View(elasticity_values)

product_info <- data.table(product_list)
product_info$ev <- ev

test <- merge(data, product_info,by.x = 'prod_id',by.y = 'product_list')

test1 <- test[,mean(ev), by = .(prod_id, category_desc_eng, sub_category_desc)]
test2 <- test1[(test1$V1 <= 0)]
test3 <- test1[(test1$V1 <= -.3)]

count_by_category <- test3[, .N, by = category_desc_eng]

wine_yogurt <- test3[(test3$category_desc_eng == 'FINE WINES') | test3$category_desc_eng == 'YOGURT HEALTH']
wine_yogurt1 <- test1[(test1$category_desc_eng == 'FINE WINES') | test1$category_desc_eng == 'YOGURT HEALTH']

write.csv(wine_yogurt, "wine_yogurt.csv")

# finding stores that sold most wine and yogurt

test_wy <- test[test$prod_id %in% wine_yogurt$prod_id]

test_wy[, store_rev := sum(tran_prod_sale_amt), by = store_id]
test_wy[, store_quantity := sum(tran_prod_sale_qty), by = store_id]
store_wy_rev <- test_wy[, mean(store_rev), by = store_id]
store_wy_quantity <- test_wy[, mean(store_quantity), by = store_id]

stores <- merge(store_wy_rev, store_wy_quantity, by = 'store_id')
colnames(stores) <- c('store_id','store_revenue','store_quantity')

write.csv(stores, 'store_info.csv')
# 
# pizza_prods <- unique(test[test$category_desc_eng == 'PIZZA']$prod_id)
# test_pizza <- test[test$prod_id %in% pizza_prods]
# 
# test_pizza[, store_rev := sum(tran_prod_sale_amt), by = store_id]
# test_pizza[, store_quantity := sum(tran_prod_sale_qty), by = store_id]
# store_pizza_rev <- test_pizza[, mean(store_rev), by = store_id]
# store_pizza_quantity <- test_pizza[, mean(store_quantity), by = store_id]

t1 <- test[, sum(tran_prod_sale_amt), by = category_desc_eng]
t1[, percent := V1/sum(V1)]
t2 <- test[, sum(tran_prod_sale_qty), by = category_desc_eng]

t1 <- t1[order(-percent),]


# graphs ------------------------------------------------------------------


cdat1<-as.data.frame(cumsum(t1$percent))
cdat1$index <- c(1:length(t1$V1))
colnames(cdat1) <- c('perent','index')
ggplot(cdat1, aes(x=index,y=perent)) +
  geom_point() +
  scale_y_continuous('Percent of Sales',breaks=seq(0,1,.10))+
  scale_x_continuous('Number of Products') +
  geom_hline(yintercept = .8, color = 'red')+
  geom_vline(xintercept = 75, color = 'red')

store_wy_rev <- store_wy_rev[order(-V1),]

plot((1:length(store_wy_rev$store_id)), store_wy_rev$V1)

plot((1:50), store_wy_rev$V1[1:50])
