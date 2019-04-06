library(data.table)
library(plyr)
library(dplyr)
library(arules)
library(splitstackshape)
options(max.print=10000000)
setwd('D:/Emory/Marketing/Segmentation/Pernalonga')

transac = fread("transaction_table.csv",header=TRUE)
prod = fread("product_table.csv",header=TRUE)

data = merge(transac, prod, by='prod_id')

# find substitutes ---------------------

wine_yogurt = fread("D:/Emory/Marketing/Pricing/pricing_data/wine_yogurt.csv", header=TRUE)
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

write.csv(all_sub_data, "D:/Emory/Marketing/Pricing/pricing_data/substitutes.csv", row.names = FALSE)

# find complements -------------

# list out the transactions with items bought together
transactionData = ddply(data_comp,c("cust_id","tran_dt"),
                        function(df1)paste(df1$prod_id,
                                           collapse = ","))
transactionData$cust_id = NULL
transactionData$tran_dt = NULL
colnames(transactionData) = c("items")

# transactionData: Data to be written
write.csv(transactionData,"D:/Emory/Marketing/Pricing/pricing_data/transactionData.csv", quote = FALSE, row.names = FALSE)

# read in with transaction format
tr = read.transactions('D:/Emory/Marketing/Pricing/pricing_data/transactionData.csv', format = 'basket', sep=',')
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
write.csv(comp_list, "D:/Emory/Marketing/Pricing/pricing_data/complements.csv", row.names = FALSE)

# build table for regression ------------------

subsitutes = fread("D:/Emory/Marketing/Pricing/pricing_data/substitutes.csv", header=TRUE)
complements = fread("D:/Emory/Marketing/Pricing/pricing_data/complements.csv", header=TRUE)
wine_yogurt = fread("D:/Emory/Marketing/Pricing/pricing_data/wine_yogurt.csv", header=TRUE)
colnames(wine_yogurt) = c("Index", "item","category_desc_eng","sub_category_desc","elas")
wine_yogurt = wine_yogurt[abs(elas)>=0.3158406,]
seasonality = fread("D:/Emory/Marketing/Pricing/pricing_data/category_weekly_sales.csv", header=TRUE)
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

write.csv(optimal_price,"D:/Emory/Marketing/Pricing/pricing_data/optimal_price.csv", row.names = FALSE)

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

write.csv(alpha,"D:/Emory/Marketing/Pricing/pricing_data/alpha.csv", row.names = FALSE)
write.csv(beta,"D:/Emory/Marketing/Pricing/pricing_data/beta.csv", row.names = FALSE)

