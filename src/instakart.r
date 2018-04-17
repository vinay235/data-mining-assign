#Load the libraries-----------------------------------------------------
library(rpart)
library(caret)
library(dplyr)
library(data.table)
library(corrplot)
library(ROCR)
library(InformationValue)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(splitstackshape)
library(caTools)
library(e1071)
library(klaR)

#Load the datasets-------------------------------------------------------
aisles <- read.csv("aisles.csv",header = T)
departments <- read.csv("departments.csv",header = T)
order_prior <- read.csv("order_products__prior.csv", header = T)
order_train <- read.csv("order_products__train.csv", header = T)
orders <- read.csv("orders.csv", header = T)
products <- read.csv("products.csv", header = T)

# str(order_prior)
# str(order_train)
# str(orders)

#Data Preprocessing---------------------------------------

#Data merging
data <- order_prior %>% left_join(orders,by="order_id")
data2 <- order_train %>% left_join(orders, by="order_id")
fdata <- rbind(data,data2)
fulldata <- fdata %>% left_join(products,by="product_id")

# write.csv(fulldata,file = "merged_data_no_test.csv")
# fdata <- read.csv("merged_data_no_test.csv")
fulldata$eval_set <- NULL
str(fulldata)
summary(fulldata)
head(fulldata)

#Missing data imputation----
fdata$days_since_prior_order[is.na(fdata$days_since_prior_order)]<- 31 
# sdata$reordered[sdata$reordered==0]<-"NO"
# sdata$reordered[sdata$reordered==1]<-"YES"

#Sampling the data-----------------------------------------

samdata <- stratified(fulldata,"user_id", .07) #stratified sampling to contain 7% of all user_id entries
# write.csv(samdata, "sample_data_final.csv")
# samdata <- read.csv("sample_data_final.csv", row.names = 1)
# sdataf <- read.csv("sample_data_features.csv", row.names = 1)

hist(fulldata$reordered) #checking the distribution of the target class in both population and the sample
hist(sdata$reordered)
hist.df <- data.frame(sapply(sdata, as.numeric))
hist(hist.df)

# sdata$reordered[sdata$reordered==0]<-"NO"
# sdata$reordered[sdata$reordered==1]<-"YES"

#Feature Engineering-------------------------------------------

#In terms of users
users <- samdata %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order),
    user_mean_days_since_prior = mean(days_since_prior_order)
  )

us <- samdata %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
  )

us$user_reorder_ratio[is.na(us$user_reorder_ratio)] <- 0

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders

us <- samdata %>% select(user_id, order_id, time_since_last_order = days_since_prior_order)

sdataf <- users %>% inner_join(us)

samdata <- samdata %>% arrange(user_id)
sdataf <- sdataf %>% arrange(user_id)

sdataf$order_id<-NULL # Deleting the duplicate variables
sdataf$user_id <- NULL
sdata$days_since_prior_order <- NULL

sdata <- cbind(samdata, sdataf) #Joining the newly created variables and the existing variables

# Feature Selection-------------------------------------

#Checking the correlation and eliminating variables which has correlation of more than 80%
corf <- sapply(sdata, as.numeric)
corel <- cor(corf)
corrplot(corel,method="circle")

sdata$user_distinct_products<-NULL #Had 97% correlation with user_total_products but user_total_products had more correlation(sightly) with our target variable(reordered)

# summary(sdata)
# str(sdata)

fit.glm1 <- glm(reordered~.,data = train.data, family = "binomial")

# Chisq. test
anova(fit.glm1, test = 'Chisq')

sdataf$order_id<-NULL # Least importance according to the Chisq. test
sdataf$user_id <- NULL

# #EDA-------------------------
# #Histograms plots
# par(mfrow=c(1,9))
# for(i in 1:9) {
#   hist(hist.df[,i], main=names(hist.df)[i])
# }

# #Box plots
# par(mfrow=c(1,9))
# for(i in 1:9) {
#   boxplot(hist.df[,i], main=names(hist.df)[i])
# }

# #Pairplot by target class
# pairs(reordered ~ ., data = hist.df, col=hist.df$reordered)


set.seed(100) # to reproduce the results

sdata$reordered[sdata$reordered==0]<-"NO"
sdata$reordered[sdata$reordered==1]<-"YES"
sdata$reordered <- as.factor(sdata$reordered)


#Splitting the data into train and test
ix <- sample.split(sdata$user_id,SplitRatio = 0.7)
train.data <- subset(sdata, ix==TRUE)
test.data <- subset(sdata, ix==FALSE)

#Logistic regression-------------------------------

fit.glm <- train(reordered ~ ., data = train.data, method="glm", preProcess=c("center","scale"), family="binomial", metric="Accuracy")
summary(fit.glm)
pred.glm <- predict(fit.glm, test.data[,-4])

caret::confusionMatrix(pred.glm,test.data$reordered, positive='YES')
#74.32

#ROC plot
probs <- predict(fit.glm, test.data[,-4], type = "prob")
thres <- 0.5
pred <- factor(ifelse(probs[,'NO'] > thres, "yes", "no"))
pred <- relevel(pred, "yes")
pred <- data.frame(pred)
pred$pred <- as.numeric(pred$pred)
roc_pred <- prediction(pred, test.data$reordered)
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE, main="ROC for Logistic Regression")

#AUC Value
auc <- performance(roc_pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Naive Bayes-------------------------
fit.nb <- train(reordered~ ., data = train.data, method="nb", preProcess=c("center","scale"))
summary(fit.nb)

fit.nb <- naiveBayes(reordered ~ ., data = train.data)

pred.nb <- predict(fit.nb, test.data[,-4])

caret::confusionMatrix(pred.nb,test.data$reordered, positive='YES')

#ROC plot
probs <- predict(fit.nb, test.data[,-4], type = "raw")
thres <- 0.5
pred <- factor(ifelse(probs[,'NO'] > thres, "yes", "no"))
pred <- relevel(pred, "yes")
pred <- data.frame(pred)
pred$pred <- as.numeric(pred$pred)
roc_pred <- prediction(pred, test.data$reordered)
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE, main="ROC for Naive Bayes")

#AUC Value
auc <- performance(roc_pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Decision tree-----------------------------------
fit.tree <- rpart(reordered ~ ., data = train.data, method = "class" )

#printcp(fit.tree)
#plotcp(fit.tree)
summary(fit.tree)
fancyRpartPlot(fit.tree)
pred.tree = predict(fit.tree,test.data[,-4], type="class")
#73.96
caret::confusionMatrix(pred.tree,test.data$reordered, positive='YES')

#Decison tree with Cross Validation
folds=10
repeats=10
fitControl <- trainControl(method="repeatedcv",number=folds,repeats=repeats,
                           classProbs=T,
                           allowParallel=T,
                           summaryFunction=twoClassSummary)

fit.tree1 <- train(reordered~.,
                  data=train.data, method = "rpart", parms=list(split='information'), tuneLength=10, trControl=fitControl)

pred.tree = predict(fit.tree1,test.data[,-4])
#plot(tree1)
#text(tree1)
fancyRpartPlot(fit.tree1)

pred.tree1 = predict(fit.tree1,test.data[,-4])
#xtab <- table(pred.tree, test.data$reordered)
#sum(diag(xtab))/sum(xtab)
caret::confusionMatrix(pred.tree1,test.data$reordered, positive='YES')

#70.48
#71.22 with repeatedcv
plotcp(tree1)
summary(tree1)

#ROC plot
probs <- predict(fit.tree, test.data[,-4], type = "prob")
thres <- 0.5
pred <- factor(ifelse(probs[,'NO'] > thres, "yes", "no"))
pred <- relevel(pred, "yes")
pred <- data.frame(pred)
pred$pred <- as.numeric(pred$pred)
roc_pred1 <- prediction(pred, test.data$reordered)
plot(performance(roc_pred1, measure="tpr", x.measure="fpr"), colorize=TRUE, main="ROC for Decision Tree")

#AUC Value
auc <- performance(roc_pred1, measure = "auc")
auc <- auc@y.values[[1]]
auc

#Random Forest-------------------------------------
control <- trainControl(method="repeatedcv", number=10, repeats=3)
fit.rf<-train(reordered ~ .,
              data = train.data, method='rf', trControl=control, tuneLength=5, ntree = 30)

pred.rf <- predict(fit.rf,test.data[,-4])

caret::confusionMatrix(pred.rf,test.data$reordered, positive='YES')

#ROC plot
probs <- predict(fit.rf, test.data[,-4], type = "prob")
thres <- 0.5
pred <- factor(ifelse(probs[,'NO'] > thres, "yes", "no"))
pred <- relevel(pred, "yes")
pred <- data.frame(pred)
pred$pred <- as.numeric(pred$pred)
roc_pred <- prediction(pred, test.data$reordered)
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE, main="ROC for Random Forest")

#AUC Value
auc <- performance(roc_pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
