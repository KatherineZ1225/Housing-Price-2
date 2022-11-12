library(caret)
library(data.table)

train <- fread('./project/volume/data/interim/train.csv')
test <- fread('./project/volume/data/interim/test.csv')
sample <- fread("./project/volume/data/raw/Stat_380_sample_submission.csv")

LF_med <- median(train$LotFrontage, na.rm = T)
test_LF_med <- median(test$LotFrontage, na.rm = T)
train[,"LotFrontage"][is.na(train[,"LotFrontage"])] <- LF_med
test[,"LotFrontage"][is.na(test[,"LotFrontage"])] <- test_LF_med

### keep future_price as train_y
train_y <- train$SalePrice

# Drop id -----------------------------------------------------------------
drops <- c('Id')
train <- train[, !drops, with = FALSE]
test <- test[, !drops, with = FALSE]
train
# Make dummy variables ----------------------------------------------------

### make fake future_price for dummyVars
test$SalePrice <- 0

### work with dummies
dummies <- dummyVars(SalePrice ~ ., data=train)

train <- predict(dummies, newdata=train)
test <- predict(dummies, newdata=test)

### convert them back to data.table
train <- data.table(train)
test <- data.table(test)

# Fit a model using cross-validation --------------------------------------

### glmnet needs matrix
train <- as.matrix(train)


gl_model<-glmnet(train, train_y, alpha = 1,family="gaussian")

error_DT<-NULL
for (i in 1:length(unclass(gl_model)$lambda)){
  model_lambda<-unclass(gl_model)$lambda[i]
  pred<-predict(gl_model,s=model_lambda, newx = train,type="response")
  error<-mean(rmse(train_y,pred[,1]))
  new_row<-c(model_lambda,error)
  error_DT<-rbind(error_DT,new_row)
}

error_DT<-data.table(error_DT)
setnames(error_DT,c("V1","V2"),c("lambda","error"))
lambda<-error_DT[error==min(error_DT$error)]$lambda









### fit a model
CVmodel <- cv.glmnet(train, train_y, alpha=1, family="gaussian")
plot(CVmodel)

### choose the best lambda
best_lambda <- CVmodel$lambda.min

### see what predictors chosen from the lambda
predict(CVmodel, s=lambda, newx=test, type="coefficients")

# Fit a full model --------------------------------------------------------
glmodel <- glmnet(train, train_y, alpha=1, lambda = lambda,family="gaussian")
plot_glmnet(glmodel)

### save the model
saveRDS(glmodel,"./project/volume/models/model.model")

# Predict by the model ----------------------------------------------------
### glmnet needs matrix
test <- as.matrix(test)
pred <- predict(glmodel, newx=test)

# Make a submission file --------------------------------------------------
sample$SalePrice <- pred
fwrite(sample, "./project/volume/data/processed/submit.csv")




















test$SalePrice <- 0

LF_med <- median(train$LotFrontage, na.rm = T)
test_LF_med <- median(test$LotFrontage, na.rm = T)


master <- rbind(qual_train,qual_test)
dummies <- dummyVars(SalePrice ~ ., data = master)

qual_train <- predict(dummies, newdata = qual_train)
qual_test <- predict(dummies, newdata = qual_test)

qual_train <- data.table(qual_train)
qual_train$Id <- train$Id
qual_test <- data.table(qual_test)
qual_test$Id <- test$Id

train_rdy <- merge(qual_train,train[,.(Id, LotFrontage, LotArea, OverallQual, OverallCond, 
                                       FullBath, HalfBath, TotRmsAbvGrd, YearBuilt, TotalBsmtSF, 
                                       BedroomAbvGr, GrLivArea, PoolArea, YrSold, SalePrice)])
test_rdy <- merge(qual_test, test[,.(Id, LotFrontage, LotArea, OverallQual, OverallCond, 
                                     FullBath, HalfBath, TotRmsAbvGrd, YearBuilt, TotalBsmtSF, 
                                     BedroomAbvGr, GrLivArea, PoolArea, YrSold, SalePrice)])
train_rdy[,"LotFrontage"][is.na(train_rdy[,"LotFrontage"])] <- LF_med
test_rdy[,"LotFrontage"][is.na(test_rdy[,"LotFrontage"])] <- test_LF_med

lm_model <- lm(SalePrice~ BldgType1Fam + BldgTypeDuplex
                  + CentralAirN + LotFrontage +
                 LotArea + OverallQual + OverallCond + FullBath + HalfBath + TotRmsAbvGrd + YearBuilt + TotalBsmtSF +
                 BedroomAbvGr + GrLivArea,data=train_rdy)
summary(lm_model)

saveRDS(dummies,"./project/volume/models/SalePrice_lm.dummies")
saveRDS(lm_model,"./project/volume/models/SalePrice_lm.model")

test$SalePrice <- predict(lm_model,newdata = test_rdy)
submit <- test[,.(Id, SalePrice)]
fwrite(submit,"./project/volume/data/processed/submit_med_lm.csv")
