library(ggplot2)
library(dplyr)
library(arules)

#data import
orders <- read.csv("orders.csv")
products <- read.csv("products.csv")
prior <- read.csv("order_products__prior.csv")
order_train <- read.csv("order_products__train.csv")

str(order(orders$order_id))
str(prior)
str(order_train)

#data merging

ord <- orders %>% arrange((order_id))
str(ord)

ord <- orders %>% left_join(order_train)
summary(ord)

setwd("G:\\DA Assignment")
sample_data <- read.csv("sample_data.csv")

sample_train <- sample_data %>% left_join(products)

sample_train <- subset(sample_train,select=-c(X))

for (i in 2:12){sample_train[,i]<-discretize(sample_train[,i])}


rules <- apriori(sample_train,
                 
                 parameter = list(supp = 0.001, conf = 0.80, maxlen = 20))

inspect(rules[1:10])
