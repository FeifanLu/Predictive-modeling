

# seperate the credit_data into testing set and training set
set.seed(1234) 
credit_data1$default_0 <- as.numeric(as.character(credit_data1$default_0))
str(credit_data1)
summary(credit_data1)
subset <- createDataPartition(y = credit_data1$default_0,
                              p = 18999/24000, list = FALSE)
credit_train <- credit_data1[subset, ]
credit_test <- credit_data1[-subset, ]




###########################################XGboost
# The outcome column
str(credit_train)
(outcome <- "default_0")

# The input columns

vars <- colnames(credit_data1)[-c(1,25)]
vars
class(vars)


# Load the package vtreat
library(vtreat)

# (the training data)
#Set the flag verbose=FALSE to prevent the function from printing too many messages.
treatplan <- designTreatmentsZ(credit_train, vars, verbose = FALSE)
treatplan
# Get the "clean" and "lev" variables from the scoreFrame
(newvars <- treatplan %>%
    use_series(scoreFrame) %>%               
    filter(code %in% c("clean", "lev")) %>%  # get the variables you care about
    use_series(varName))                     # get the varName column
newvars
# Prepare the training data
train.treat <- prepare(treatplan, credit_train,  varRestriction = newvars)

# Prepare the test data
test.treat <- prepare(treatplan, credit_test, varRestriction = newvars)

# Call str() on the treated data
str(train.treat) 
str(test.treat)


##########################################Find the right number of trees for a gradient boosting machine



# Load the package xgboost
library(xgboost)
class(credit_train$default_0)
# Run xgb.cv
set.seed(1234)
cv <- xgb.cv(data = as.matrix(train.treat), 
             label = credit_train$default_0,
             nrounds = 100,
             nfold = 10,
             objective = "binary:logistic",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0   # silent
)

# Get the evaluation log
elog <- cv$evaluation_log
elog
# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_error_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_error_mean))    # find the index of min(test_rmse_mean)
ntrees <- which.min(elog$test_error_mean)
ntrees

####################################Fit an xgboost bike rental model and predict



# The number of trees to use, as determined by xgb.cv
ntrees

# Run xgboost
set.seed(1234)
model_xgb <- xgboost(data = as.matrix(train.treat), # training data as matrix
                          label = credit_train$default_0,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective = "binary:logistic", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
                          
)

# Make predictions
credit_test$pred <- predict(model_xgb, as.matrix(test.treat))
credit_test$pred 

#########################################Evaluate the xgboost 

####ROC Curve
XGboost_ROC_prediction <- prediction(credit_test$pred, credit_test$default_0) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value






money <-function(prob_of_def, actual_value){
  cutoff=rep(NA, 101)
  profit_list =rep(NA, 101)
  accept_rate=seq(1,0,by=-0.01)
  
  for (a in 1:101){
    cutoff[a]=quantile(prob_of_def,accept_rate[a])
    pred_a=ifelse(prob_of_def> cutoff[a], 1, 0)
    profit = 0
    for (i in 1:length(pred_a)){
      if(pred_a[i]==0 & actual_value[i] == 1){
        profit = profit - 5000 
      }else if(pred_a[i] ==0 & actual_value[i]==0){
        profit = profit + 1500
      }else{
        profit = profit
      }
      
    }
    
    profit_list[a]=(profit/5)
  }
  table=cbind(accept_rate, cutoff=round(cutoff,4),profit_list)
  return(table)
}

credit_test$pred
cut_off <- money(credit_test$pred, credit_test$default_0)
cut_off


confmat1 <- table(credit_test$default_0,ifelse(credit_test$pred>0.8689066, 1,0))
confmat1
(3879*1500-1122*5000)/5

confmat2 <- table(credit_test$default_0,ifelse(credit_test$pred>0.6777221, 1,0))
confmat2
(3812*1500-939*5000)/5


confmat3 <- table(credit_test$default_0,ifelse(credit_test$pred>0.5231692, 1,0))
confmat3
(3711*1500-789*5000)/5






#################################Tune the XGboost


# Load the package xgboost

summary(credit_train$default_0)




obj <- function(preds, dtrain) {
  beta = 3
  labels <- getinfo(dtrain, "label")
  preds <- 1 / (1 + exp(-preds))
  grad <- preds*((beta - 1) * labels +1) - beta * labels
  hess <- ((beta - 1) * labels + 1) * preds * (1.0 - preds)
  return(list(grad = grad, hess = hess))
}
# Run xgb.cv
##
set.seed(1234)
cv.f1 <- xgb.cv(data = as.matrix(train.treat), 
             label = credit_train$default_0,
             nrounds = 100,
             nfold = 10,
             objective = obj,
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0   # silent
)



 # Get the evaluation log
elog.f1 <- cv$evaluation_log
elog.f1
# Determine and print how many trees minimize training and test error
elog.f1 %>% 
  summarize(ntrees.train = which.min(train_error_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_error_mean))    # find the index of min(test_rmse_mean)
ntrees.f1 <- which.min(elog.f1$test_error_mean)
ntrees.f1

####################################Fit an xgboost bike rental model and predict



# The number of trees to use, as determined by xgb.cv
ntrees.f1

# Run xgboost
set.seed(1234)
model_xgb.f1 <- xgboost(data = as.matrix(train.treat), # training data as matrix
                     label = credit_train$default_0,  # column of outcomes
                     nrounds = ntrees,       #number of trees to build
                     objective = obj, # objective
                     eta = 0.3,
                     depth = 6,
                     verbose = 0  # silent
                     
)

####ROC Curve
XGboost_ROC_prediction <- prediction(credit_test$pred.f1, credit_test$default_0) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value



# Make predictions
credit_test$pred.f1 <- predict(model_xgb.f1, as.matrix(test.treat))
credit_test$pred.f1

cut_off.f1 <- money(credit_test$pred.f1, credit_test$default_0)
cut_off.f1 <- as.data.frame(cut_off.f1)
class(cut_off.f1)
cut_off.f1
cut_off.f1[which.max(cut_off.f1$profit_list),]

#Maximum 491600



