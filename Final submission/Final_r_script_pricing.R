# Group 3 - Abhishek, Alana, Gary, Miller
# Pricing Project R Code

# Loading necessary packages ----------------------------------------------

library(arules)
library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)

# Reading in data and merging ---------------------------------------------

transactions <- fread("transaction_table.csv")
products <- fread("product_table.csv")
data <- merge(transactions, products, by='prod_id', all=TRUE)


# data cleaning/ prep ------------------------------------------------------


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



# Analysis of First Week of Apr, 2018 -------------------------------------

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


# Long tail graphs --------------------------------------------------------

t1 <- data[, sum(tran_prod_sale_amt), by = category_desc_eng]
t1[, percent := V1/sum(V1)]
t2 <- data[, sum(tran_prod_sale_qty), by = category_desc_eng]
t1 <- t1[order(-percent),]

cumulative_vals<-as.data.frame(cumsum(t1$percent))
cumulative_vals$index <- c(1:length(t1$V1))
colnames(cumulative_vals) <- c('perent','index')
ggplot(cumulative_vals, aes(x=index,y=perent)) +
  geom_point() +
  scale_y_continuous('Percent of Sales',breaks=seq(0,1,.10))+
  scale_x_continuous('Number of Product categories') +
  geom_hline(yintercept = .8, color = 'red')+
  geom_vline(xintercept = 75, color = 'red')



# Finding top 100 products ------------------------------------------------

# finding list of stores and products
store_list <- unique(data$store_id)
product_list <- unique(data$prod_id)

# initializing empty lists for elasticity values and alpha values
elasticity_vec <- list()
alphas <-c()

# for loop overview:
# 1. loop over each product, taking a subset of the data for that item
# 2. loop over each different price point for that item
# 3. For each price point, find the demand (sum of transaction quantity)
# 4. Find the slope and alpha of the prices and demands for that product
# 5. calculate elasticity using the average price, average demand, and slope

for (i in 1:length(product_list)){
  temp <- data[data$prod_id == product_list[i]]
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

#write.csv(slopes, "product_alphas.csv")

elasticity_values <- data.frame(matrix(unlist(elasticity_vec), nrow=length(elasticity_vec), byrow=T))
View(elasticity_values)

# working with product info -----------------------------------------------


product_info <- data.table(product_list)
product_info$ev <- ev

data_with_prod_info <- merge(data, product_info,by.x = 'prod_id',by.y = 'product_list')

# get mean elasticity by product
data_with_elasticity <- data_with_prod_info[,mean(ev), by = .(prod_id, category_desc_eng, sub_category_desc)]

# filter the data by elasticity value
filtered_elas_data <- data_with_elasticity[(data_with_elasticity$V1 <= -.3)]

# count up the number of products in the list by each category
count_by_category <- filtered_elas_data[, .N, by = category_desc_eng]

# wine and yogurt were top 2 categories, so filter the data for only those products
wine_yogurt <- filtered_elas_data[(filtered_elas_data$category_desc_eng == 'FINE WINES') |
                                    filtered_elas_data$category_desc_eng == 'YOGURT HEALTH']

#write.csv(wine_yogurt, "wine_yogurt.csv")


# Data with only 100 products ---------------------------------------------


# finding stores that sold most wine and yogurt
wy_data <- data_with_prod_info[data_with_prod_info$prod_id %in% wine_yogurt$prod_id]

wy_data[, store_rev := sum(tran_prod_sale_amt), by = store_id]
wy_data[, store_quantity := sum(tran_prod_sale_qty), by = store_id]

store_wy_rev <- wy_data[, mean(store_rev), by = store_id]
store_wy_quantity <- wy_data[, mean(store_quantity), by = store_id]

store_data <- merge(store_wy_rev, store_wy_quantity, by = 'store_id')
colnames(store_data) <- c('store_id','store_revenue','store_quantity')

#write.csv(store_data, 'store_info.csv')


# graph -------------------------------------------------------------------

store_wy_rev <- store_wy_rev[order(-V1),]

ggplot(store_wy_rev,aes(x=1:length(store_id), y=V1))+
  geom_point() + 
  scale_x_continuous('Count of stores', breaks = seq(0,450,50))+
  scale_y_continuous('Wine/Yogurt Revenue', breaks = seq(0,10000,1000))

store_wy_rev_50 <- store_wy_rev[1:50,]
ggplot(store_wy_rev_50,aes(x=1:length(store_id), y=V1))+
  geom_point() + 
  scale_x_continuous('Count of stores', breaks = seq(0,50,10))+
  scale_y_continuous('Wine/Yogurt Revenue', breaks = seq(0,10000,1000))


# finding substitutes ---------------------

subcat = unique(wine_yogurt$sub_category_desc)

all_sub = list()
for (i in c(1:length(subcat))){
  sub = data[sub_category_desc == subcat[i],]
  # create a list of products bought in one transaction within the same category
  transactionData = ddply(sub,c("cust_id","tran_dt"),
                          function(df1)paste(df1$prod_id,
                                             collapse = ","))
  transactionData$cust_id = NULL
  transactionData$tran_dt = NULL
  colnames(transactionData) = c("items")
  
  # get the table of purchases by transaction including co-purchases
  # transform it to the transaction table form
  transactionData = data.frame(lapply(transactionData,as.factor))
  items = strsplit(as.character(transactionData$items), ",")
  tr = as(items,"transactions")
  
  # generate the association rule to find the products that have been purchased together
  rules = apriori(tr, parameter = list(supp=0, conf=0,minlen=2, maxlen=2))
  # extract the results showing lift
  out = capture.output(inspect(rules))
  rhs = gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\2", out)[-1]
  lhs = gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\1", out)[-1]
  lift = vector()
  for (j in (2:length(out)) ){
    lift[j-1] = substr(out[j],61,64)
  }
  lift_value = as.data.frame(lift)
  
  co_buy = as.data.frame(cbind(lhs, rhs, lift))
  sub_list = as.data.table(co_buy[lift<1,])
  sub_list[,list(rhs),by=lhs]
  colnames(sub_list) = c("item","substitute","lift")
  all_sub[[i]] = sub_list
}

all_sub_data = do.call(rbind, all_sub)
all_sub_data$lift = NULL

# ** for substitutes data later **
write.csv(all_sub_data, "substitutes.csv", row.names = FALSE)

# find complements -------------------------------------

# list out the transactions with items bought together
transactionData = ddply(data_comp,c("cust_id","tran_dt"),
                        function(df1)paste(df1$prod_id,
                                           collapse = ","))
transactionData$cust_id = NULL
transactionData$tran_dt = NULL
colnames(transactionData) = c("items")

# transactionData: Data to be written
# ** important to write this file and then read in as transactions **
write.csv(transactionData,"transactionData.csv", quote = FALSE, row.names = FALSE)
# read in with transaction format==
tr = read.transactions('transactionData.csv', format = 'basket', sep=',')
summary(tr)


# generate list of complements for products of interest
subcat_comp = unique(wine_yogurt$prod_id)

# generate the association rules
rules = apriori(tr, parameter = list(supp=0,conf=0.25,minlen=2,maxlen=2))

out = capture.output(inspect(rules))
rhs = gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\2", out)[-1]
lhs = gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\1", out)[-1]
co_buy = as.data.frame(cbind(lhs, rhs))
colnames(co_buy) = c("item","complements")
comp_list = co_buy[co_buy$item %in% subcat_comp,]

# ** important for complements csv later **
write.csv(comp_list, "complements.csv", row.names = FALSE)

# build table for regression --------------------------------

subsitutes = fread("ubstitutes.csv", header=TRUE)
complements = fread("complements.csv", header=TRUE)
wine_yogurt = fread("wine_yogurt.csv", header=TRUE)

colnames(wine_yogurt) = c("Index", "item","category_desc_eng","sub_category_desc","elas")
wine_yogurt = wine_yogurt[abs(elas)>=0.3158406,]

seasonality = fread("category_weekly_sales.csv", header=TRUE)
seasonality[,year_week := paste(year,week,sep=",")]

#alpha = fread("D:/Emory/Marketing/Pricing/pricing_data/product_alphas.csv", header=TRUE)
substitutes_price = fread("D:/Emory/Marketing/Pricing/pricing_data/substitutes_price.csv", header=TRUE)
substitutes_price$prod_id = paste("s",substitutes_price$prod_id,sep = "_")
complements_price = fread("D:/Emory/Marketing/Pricing/pricing_data/complements_price.csv", header=TRUE)
complements_price$prod_id = paste("c",complements_price$prod_id,sep = "_")

data_reg = data[data$prod_id %in% wine_yogurt$item|data$prod_id %in% subsitutes$substitute|
                  data$prod_id %in% complements$complements,]

# add year and week to this dataset
data_reg$week = strftime(data_reg$tran_dt, format = "%V")
data_reg$year = strftime(data_reg$tran_dt, format = "%Y")
data_reg[,year_week := paste(year,week,sep=",")]

prod_of_interest = unique(wine_yogurt$item)
prod_store_time = unique(data_reg[data_reg$prod_id %in% 
                                    prod_of_interest,][,c("year_week","prod_id","store_id")])

op_price = c()
for (i in c(1:length(prod_of_interest))){ 
  product = prod_of_interest[i]
  time = unique(prod_store_time[prod_store_time$prod_id == product,]$year_week)
  sub = unique(subsitutes[item==product,]$substitute)
  sub = sub[!(sub %in% wine_yogurt$item)]
  comp = unique(complements[item == product,]$complements)
  comp = comp[!(comp %in% wine_yogurt$item)]
  
  # all subs and comps average price for one product on a weekly level  
  sub_price_week = c()
  comp_price_week = c()
  for (t in c(1:length(time))){ 
    # sub
    sub_price = list()
    for (k in c(1:length(sub))){
      sub_tran = data_reg[data_reg$year_week ==time[t] & data_reg$prod_id ==sub[k],]
      sub_price[k] = sub_tran[,mean(prod_unit_price)]
    }
    sub_price = data.frame(sub_price)
    colnames(sub_price) = sub
    if (!(is.na(colnames(sub_price)))){
      sub_price_week = rbind(sub_price_week, sub_price)}else{
        sub_price_week = as.data.frame(matrix(nrow = length(time),ncol = 1))}
    
    # comp
    comp_price = list()
    for (k in c(1:length(comp))){
      comp_tran = data_reg[data_reg$year_week ==time[t] & data_reg$prod_id ==comp[k],]
      comp_price[k] = comp_tran[,mean(prod_unit_price)]
    }
    comp_price = data.frame(comp_price)
    colnames(comp_price) = comp
    if (!(is.na(colnames(comp_price)))){
      comp_price_week = rbind(comp_price_week, comp_price)}else{
        comp_price_week = as.data.frame(matrix(nrow = length(time),ncol = 1))}
  }
  
  # sub
  rownames(sub_price_week) = time
  # clean the columns with NANs
  sub_price_week = sub_price_week %>%
    select_if(~ !any(is.nan(.)))
  sub_prod = colnames(sub_price_week)
  # rename the columns to prepare for model
  if (ncol(sub_price_week)>0){
    colnames(sub_price_week) = paste("s", colnames(sub_price_week),sep = "_")
  } 
  
  # comp
  rownames(comp_price_week) = time
  # clean the columns with NANs
  comp_price_week = comp_price_week %>%
    select_if(~ !any(is.nan(.)))
  comp_prod = colnames(comp_price_week)
  # rename the columns to prepare for model
  if (ncol(comp_price_week)>0){
    colnames(comp_price_week) = paste("c", colnames(comp_price_week),sep = "_")
  }
  
  # find the table for all subs and comps
  affinity_price = cbind(sub_price_week,comp_price_week)
  affinity_price = setDT(affinity_price, keep.rownames = TRUE)[]
  # find the other attributes
  tran = data_reg[prod_id ==product,]
  prod_price = tran[,mean(prod_unit_price),by=.(year_week)]
  discount_amount = tran[,mean(tran_prod_discount_amt/tran_prod_sale_qty),by=.(year_week)]
  demand = tran[,sum(tran_prod_sale_qty),by=.(year_week)]
  
  # bind those in
  affinity_price$prod_price = prod_price$V1
  affinity_price$discount_amount = discount_amount$V1
  affinity_price$demand = demand$V1
  # add the seasonality in
  seasonality_product = seasonality[category_id==
                                      unique(tran$category_id),][,c("avg_sales","year_week")]
  setnames(affinity_price,old="rn",new="year_week")
  affinity_price = merge(affinity_price, seasonality_product,by="year_week")
  
  # change demand to the first column
  affinity_price = affinity_price %>%
    select(demand, everything())
  # clean NA columns
  affinity_price = affinity_price %>%
    select_if(~ !any(is.na(.)))
  
  # build the model for this product
  fml = as.formula(paste("demand ~ ", 
                         paste(colnames(affinity_price)[3:ncol(affinity_price)], collapse = " + ")))
  
  model = lm(fml, data= affinity_price)
  coeff = as.data.frame(model$coefficients)
  coeff[is.na(coeff)] = 0
  if (coeff["prod_price","model$coefficients"]<0){
    coeff = setDT(coeff, keep.rownames = TRUE)[]
    setnames(coeff, old="rn",new="prod_id")
    coeff = merge(coeff, complements_price,all.x=TRUE,by=("prod_id"))
    coeff = merge(coeff, substitutes_price,all.x=TRUE,by=("prod_id"))
    coeff$V1.x = NULL
    coeff$V1.y = NULL
    coeff$value = coalesce(coeff$avg_price.x, coeff$avg_price.y)
    coeff$avg_price.x = NULL
    coeff$avg_price.y = NULL
    coeff[coeff$prod_id=="(Intercept)","value"]= 1
    # add in seasonality factor
    seasonality_product2 = seasonality[category_id==
                                         unique(tran$category_id) &
                                         year_week=="2017,14",][,c("avg_sales")]
    coeff[coeff$prod_id=="avg_sales","value"]= seasonality_product2
    coeff[is.na(coeff)] = 0
    other = sum(coeff$`model$coefficients`*coeff$value)
    price_coeff = as.numeric(coeff[coeff$prod_id=="prod_price","model$coefficients"])
    op_fun = function(p){
      (other+price_coeff*p)*p
    }
    result = optimize(f=op_fun, interval = c(0,max(tran$prod_unit_price)*1.5),maximum = TRUE)
    optimum = c(product, result$maximum)
    op_price = rbind(op_price, optimum)
  }  else{
    optimum = c(product,max(tran$prod_unit_price))
    op_price = rbind(op_price, optimum)
  }
}
# save it as a data frame
optimal_price = as.data.frame(op_price)
colnames(optimal_price) = c("prod_id","op_price")

#write.csv(optimal_price,"optimal_price.csv", row.names = FALSE)

# record alpha and beta -------------------

alpha = c()
beta = c()
for (i in c(31:60)){ 
  product = prod_of_interest[i]
  time = unique(prod_store_time[prod_store_time$prod_id == product,]$year_week)
  sub = unique(subsitutes[item==product,]$substitute)
  sub = sub[!(sub %in% wine_yogurt$item)]
  comp = unique(complements[item == product,]$complements)
  comp = comp[!(comp %in% wine_yogurt$item)]
  
  # all subs and comps average price for one product on a weekly level  
  sub_price_week = c()
  comp_price_week = c()
  for (t in c(1:length(time))){ 
    # sub
    sub_price = list()
    for (k in c(1:length(sub))){
      sub_tran = data_reg[data_reg$year_week ==time[t] & data_reg$prod_id ==sub[k],]
      sub_price[k] = sub_tran[,mean(prod_unit_price)]
    }
    sub_price = data.frame(sub_price)
    colnames(sub_price) = sub
    if (!(is.na(colnames(sub_price)))){
      sub_price_week = rbind(sub_price_week, sub_price)}else{
        sub_price_week = as.data.frame(matrix(nrow = length(time),ncol = 1))}
    
    # comp
    comp_price = list()
    for (k in c(1:length(comp))){
      comp_tran = data_reg[data_reg$year_week ==time[t] & data_reg$prod_id ==comp[k],]
      comp_price[k] = comp_tran[,mean(prod_unit_price)]
    }
    comp_price = data.frame(comp_price)
    colnames(comp_price) = comp
    if (!(is.na(colnames(comp_price)))){
      comp_price_week = rbind(comp_price_week, comp_price)}else{
        comp_price_week = as.data.frame(matrix(nrow = length(time),ncol = 1))}
  }
  
  # sub
  rownames(sub_price_week) = time
  # clean the columns with NANs
  sub_price_week = sub_price_week %>%
    select_if(~ !any(is.nan(.)))
  sub_prod = colnames(sub_price_week)
  # rename the columns to prepare for model
  if (ncol(sub_price_week)>0){
    colnames(sub_price_week) = paste("s", colnames(sub_price_week),sep = "_")
  } 
  
  # comp
  rownames(comp_price_week) = time
  # clean the columns with NANs
  comp_price_week = comp_price_week %>%
    select_if(~ !any(is.nan(.)))
  comp_prod = colnames(comp_price_week)
  # rename the columns to prepare for model
  if (ncol(comp_price_week)>0){
    colnames(comp_price_week) = paste("c", colnames(comp_price_week),sep = "_")
  }
  
  # find the table for all subs and comps
  affinity_price = cbind(sub_price_week,comp_price_week)
  affinity_price = setDT(affinity_price, keep.rownames = TRUE)[]
  # find the other attributes
  tran = data_reg[prod_id ==product,]
  prod_price = tran[,mean(prod_unit_price),by=.(year_week)]
  discount_amount = tran[,mean(tran_prod_discount_amt/tran_prod_sale_qty),by=.(year_week)]
  demand = tran[,sum(tran_prod_sale_qty),by=.(year_week)]
  
  # bind those in
  affinity_price$prod_price = prod_price$V1
  affinity_price$discount_amount = discount_amount$V1
  affinity_price$demand = demand$V1
  # add the seasonality in
  seasonality_product = seasonality[category_id==
                                      unique(tran$category_id),][,c("avg_sales","year_week")]
  setnames(affinity_price,old="rn",new="year_week")
  affinity_price = merge(affinity_price, seasonality_product,by="year_week")
  
  # change demand to the first column
  affinity_price = affinity_price %>%
    select(demand, everything())
  # clean NA columns
  affinity_price = affinity_price %>%
    select_if(~ !any(is.na(.)))
  
  # build the model for this product
  fml = as.formula(paste("demand ~ ", 
                         paste(colnames(affinity_price)[3:ncol(affinity_price)], collapse = " + ")))
  
  model = lm(fml, data= affinity_price)
  coeff = as.data.frame(model$coefficients)
  coeff[is.na(coeff)] = 0
  coeff = setDT(coeff, keep.rownames = TRUE)[]
  setnames(coeff, old="rn",new="prod_id")
  coeff = merge(coeff, complements_price,all.x=TRUE,by=("prod_id"))
  coeff = merge(coeff, substitutes_price,all.x=TRUE,by=("prod_id"))
  coeff$V1.x = NULL
  coeff$V1.y = NULL
  coeff$value = coalesce(coeff$avg_price.x, coeff$avg_price.y)
  coeff$avg_price.x = NULL
  coeff$avg_price.y = NULL
  coeff[coeff$prod_id=="(Intercept)","value"]= 1
  # add in seasonality factor
  seasonality_product2 = seasonality[category_id==
                                       unique(tran$category_id) &
                                       year_week=="2017,14",][,c("avg_sales")]
  coeff[coeff$prod_id=="avg_sales","value"]= seasonality_product2
  coeff[is.na(coeff)] = 0
  other = sum(coeff$`model$coefficients`*coeff$value)
  price_coeff = as.numeric(coeff[coeff$prod_id=="prod_price","model$coefficients"])
  alpha_value = c(product, other)
  alpha = rbind(alpha,alpha_value)
  beta_value = c(product, price_coeff)
  beta = rbind(beta,beta_value)
}
# save it as a data frame
alpha = as.data.frame(alpha)
colnames(alpha) = c("prod_id","alpha")
beta = as.data.frame(beta)
colnames(beta) = c("prod_id","beta")


# Calculating sales and revenue increase ----------------------------------

top_stores <- store_data
wine_yogurt <- wine_yogurt[order(elasticity),]
wine_yogurt <- wine_yogurt[1:100,]
top_stores <- top_stores[order(-store_revenue)]

# Our selected stores 
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

#write.csv(prod_store_latest_sales_all, "prod_store_latest_sales_all.csv")
