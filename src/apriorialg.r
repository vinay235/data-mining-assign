# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

#Insta cart project
library(dplyr) # Data manipulation
library(ggplot2)
library(arules)#Apriori
library(arulesViz)#Apriori visualization

#Data import
orders<-read.csv('orders.csv',header = TRUE,sep=",",na.strings = c(""," ","na","NA"))
products<-read.csv('products.csv',header = TRUE,sep=",",na.strings = c(""," ","na","NA"))
order_products<-read.csv('order_products__train.csv',header = TRUE,sep=",",na.strings = c(""," ","na","NA"))
order_products_prior<-read.csv('order_products__prior.csv',header = TRUE,sep=",",na.strings = c(""," ","na","NA"))
aisles<-read.csv('aisles.csv',header = TRUE,sep=",",na.strings = c(""," ","na","NA"))
departments<-read.csv('departments.csv',header = TRUE,sep=",",na.strings = c(""," ","na","NA"))

#Save RDS file 
#saveRDS(orders,'orders.RDS')
#saveRDS(products,'products.RDS')
#saveRDS(order_products,'order_products.RDS')
#saveRDS(order_products_prior,'order_products_prior.RDS')

#Read RDS file
#orders<-readRDS('orders.RDS')
#products<-readRDS('products.RDS')
#order_products<-readRDS('order_products.RDS')
#order_products_prior<-readRDS('order_products_prior.RDS')

#Data exploration
str(orders)
table(is.na(orders))
colSums(is.na(orders))
orders$order_hour_of_day=as.factor(orders$order_hour_of_day)

str(products)
table(is.na(products))
#products$product_name=as.factor(products$product_name)

str(order_products)
table(is.na(order_products))

str(order_products_prior)
table(is.na(order_products_prior))

str(aisles)
table(is.na(aisles))
aisles$aisle=as.factor(aisles$aisle) 

str(departments)
table(is.na(departments))
departments$department=as.factor(departments$department)

#Data Exploration
#order_basket<-order_products_prior%>%
#  inner_join(products,by='product_id')%>%
#  group_by(order_id)%>%
#  summarise(baskect=as.vector(list(product_name)))

#What time of day people order
ggplot(orders,aes(x=order_hour_of_day))+
  geom_histogram(stat = 'count',fill='blue')

#Which week people buy
ggplot(orders,aes(x=order_dow))+
  geom_histogram(stat = 'count',fill='blue')

#When do they order
ggplot(orders,aes(x=days_since_prior_order))+
  geom_histogram(stat='count',fill='blue')

#How many items do people rebuy
ggplot(order_products,aes(x=reordered))+
  geom_histogram(stat='count',fill='blue')

#Bestseller
ggplot(order_products,aes(x=product_id))+
  geom_histogram(stat='identity',fill='blue')

# Association rule 
tmp=discretize(orders)
tmp=data.frame(sapply(orders,as.factor))
rule<-apriori(tmp,
              parameter=list(minlen=2,support=0.005,conf=0.8),control = list(verbose=F))

rule.sort=sort(rule,by='lift')
inspect(rule.sort)

#find redundant rule
subset.ma=is.subset(rule.sort,rule.sort)
subset.ma[lower.tri(subset.ma, diag=T)] <- NA
redundant <- colSums(subset.ma, na.rm=T) >= 1
which(redundant)

#Remove redundant rule
rule.prune<-rule.sort[!redundant]
plot(rule)

rule.prune
