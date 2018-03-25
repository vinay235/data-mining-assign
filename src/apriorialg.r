# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
# Any results you write to the current directory are saved as output.

#Insta cart project
library(dplyr) # Data manipulation
library(ggplot2)
library(arules)#Apriori
library(viridisLite)
library(arulesViz)#Apriori visualization

#Data import
data <- read.csv("sample_data.csv")
products <- read.csv("products.csv")
fdata <- data %>% left_join(products)
groc <- data("Groceries")

#data for arules
adata <- fdata %>% select(order_id,product_name)

adata <- arrange(adata, desc(order_id))
adata <- adata %>% group_by(order_id) %>% summarise(products = paste(product_name, collapse = "$"))


fdata <- adata
fdata$order_id <- NULL
write.csv(bdata,"bdata.csv")

fdata <- read.transactions("bdata.csv", sep=',')

size(head(fdata))
LIST(head(fdata,3))
frq <- eclat(fdata,parameter = list(supp=0.01,maxlen=15))
inspect(frq)

# Association rule
rule<-apriori(fdata,
              parameter=list(minlen=2,support=0.0001,conf=0.08),control = list(verbose=F))

rule.sort=sort(rule,by='lift', decreasing = TRUE)
inspect(rule.sort)

subsetRules <- which(colSums(is.subset(rule,rule))>1)
length(subsetRules)
rules <- rule[-subsetRules]

plot(head(sort(rules, by="lift"),n=20), method="graph",control=list(cex=.8))
