#Load the datasets
aisles <- read.csv("aisles.csv",header = T)
departments <- read.csv("departments.csv",header = T)
order_prior <- read.csv("order_products__prior.csv", header = T)
order_train <- read.csv("order_products__train.csv", header = T)
orders <- read.csv("orders.csv", header = T)
products <- read.csv("products.csv", header = T)

library(rpart)
library(caret)
library(dplyr)
library(data.table)

#order_prior <- as.matrix(order_prior)
#orders <- as.matrix(orders)

str(order_prior)
str(order_train)
str(orders)

#data merging
data <- order_prior %>% left_join(orders,by="order_id")
data2 <- order_train %>% left_join(orders, by="order_id")
fdata <- rbind(data,data2)
fdata2 <- fdata %>% left_join(products,by="product_id")

#write.csv(fdata,file = "merged_data_no_test.csv")

str(fdata)
summary(fdata)
summary(orders)


# values in 30 days
day30 <- data %>% filter(days_since_prior_order == 30)
summary(day30)
day30 <- day30 %>% left_join(products,by ="product_id")
day30$aisle_id <- NULL
day30$department_id <- NULL

tmp <- day30 %>% group_by(product_name) %>% summarise(count= n()) %>% top_n(10, wt=count) %>% arrange(desc(count))
head(tmp)

tmp %>% ggplot(aes(x=reorder(product_name,-count), y=count)) + geom_bar(stat="identity", fill="red") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))
str(tmp)

tmp <- day30 %>% group_by(reordered) %>% summarise(count= n()) %>% mutate(reordered = as.factor(reordered)) %>% mutate(proportion = count/sum(count))
summary(tmp)
kable(tmp)
tmp %>% ggplot(aes(x=reordered, y=count, fill=reordered)) + geom_bar(stat="identity")

tmp <- day30 %>% group_by(product_name) %>% summarise(proportion_reordered = mean(reordered), n=n()) %>%
  filter(n>40) %>% top_n(10, wt=proportion_reordered) %>% arrange(desc(proportion_reordered))

tmp %>% 
  ggplot(aes(x=reorder(product_name,-proportion_reordered), y=proportion_reordered))+
  geom_bar(stat="identity",fill="red")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.75,0.85))

# NA values

dayNA <- data %>% filter(is.na(days_since_prior_order))
summary(dayNA)
dayNA <- dayNA %>% left_join(products, by = "product_id")
dayNA$aisle_id <- NULL
dayNA$department_id <- NULL

tmp <- dayNA %>% group_by(product_name) %>% summarise(count= n()) %>% top_n(10, wt=count) %>% arrange(desc(count))
head(tmp)

tmp %>% ggplot(aes(x=reorder(product_name,-count), y=count)) + geom_bar(stat="identity", fill="red") +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))
str(tmp)

tmp <- dayNA %>% group_by(reordered) %>% summarise(count= n()) %>% mutate(reordered = as.factor(reordered)) %>% mutate(proportion = count/sum(count))
summary(tmp)
str(tmp)
tmp %>% ggplot(aes(x=reordered, y=count, fill=reordered)) + geom_bar(stat="identity")

tmp <- dayNA %>% group_by(product_name) %>% summarise(proportion_reordered = mean(reordered), n=n()) %>%
  filter(n>40) %>% top_n(10, wt=proportion_reordered) %>% arrange(desc(proportion_reordered))


#sampling the data
library(splitstackshape)

sdata <- stratified(fdata,"user_id", .3) #stratified sampling to contain 30% of all user_id entries
#write.csv(sdata, "sample_data.csv")
sdata <- read.csv("sample_data.csv")
#splitting the data into train and test
sdata$X <-NULL

train.data <- sdata %>% filter(eval_set=="prior")
test.data <- sdata %>% filter(eval_set=="train")

#train.data[is.na(train.data)] <- -1
train.data <- train.data %>% na.omit

train.data$days_since_prior_order <- as.factor(train.data$days_since_prior_order)
train.data$reordered <- as.factor(train.data$reordered)

test.data$days_since_prior_order <- as.factor(test.data$days_since_prior_order)



#decision tree
tree1 <- rpart(reordered~ product_id + user_id + order_dow + order_hour_of_day, data=train.data, method = "class")

library(rattle)
library(rpart.plot)
library(RColorBrewer)

#plot(tree1)
#text(tree1)

fancyRpartPlot(tree1)

prediction = predict(tree1,test.data[,-4],type="class")
result <- data.frame(order_id = test.data$order_id, product_id=test.data$product_id,reordered = prediction)

confmat <- table(test.data$reordered,result$reordered)
accuracy <- sum(diag(confmat))/sum(confmat)
accuracy 
#old - 64.25, new(cleaned) - 61.93, new(-order_id,eval_test,order_num) & NA = -1 = 62.27
#new2(-addtocartorder) & na.omit = 61.14
#59.78

printcp(tree1)
plotcp(tree1)
summary(tree1)

#logistic regerssion
library(caTools)
model <- glm(reordered~ order_dow + order_hour_of_day + days_since_prior_order + add_to_cart_order,
             data = train.data, family = binomial())
summary(model)

library(car)
vif(model)

library(InformationValue)

# ANOVA
anova(model, test = 'Chisq')

predict <- predict(model, newdata = test.data[,-4], type="response")
pred <- ifelse(predict>0.6,1,0)
res <- as.data.frame(pred)
#plot ROC
library(ROCR)
library(Metrics)

pr <- prediction(pred,test.data$reordered)
perf <- performance(pr,measure = "tpr", x.measure = "fpr")
plot(perf)
auc(test.data$reordered, pred) #accuracy


Concordance(test.data$reordered, res$pred)
confusionMatrix(test.data$reordered,res$pred)
sensitivity(test.data$reordered,res$pred)
specificity(test.data$reordered,res$pred)

plotROC(test.data$reordered,res$pred)
