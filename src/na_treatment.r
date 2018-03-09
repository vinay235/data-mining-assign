#missing value treatment

library(mice)
library(rpart)
library(rpart.plot)

orders <- read.csv("orders.csv", header = T)
table(is.na(orders$days_since_prior_order))

sam <- orders[1:342108,]
str(sam)

# MICE

md.pattern(sam) #pattern or missing values in data

miceMod <- mice(sam, method = "sample") #perform mice imputation by rf- random forests method

miceOut <- complete(miceMod) # generate the complete data

md.pattern(miceOut)


# regression

reg_pred <- rpart(days_since_prior_order ~ . , data = sam, method = "anova")

res <- predict(reg_pred, sam[is.na(sam$days_since_prior_order),])

res1 <- as.data.frame(res)
