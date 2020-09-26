


library(tidyverse)
library(ggthemes)
library(corrgram)
library(corrplot)
library(caTools) 
library(readxl)
library(caret)
library(VIM)
library(Amelia)
library(mice)
library(ggrepel)
library(psych)
library(moments)
library(plyr)
library(glmnet)
library(randomForest)
                                          #1.Load data
my.train <- file.choose()
my.train
my.test <- file.choose()
my.test
head(df.train$SalePrice)
df.train<-read.csv( "C:\\Users\\lufei\\Desktop\\kaggle\\train.csv")
glimpse(df.train)
head(df.train)

df.test<-read.csv("C:\\Users\\lufei\\Desktop\\kaggle\\test.csv")
glimpse(df.test)
head(df.test)

#combine trian and test dataset together for the consistency of data manipulation between these two.
#before combining, add SalePrice column to df.test to match numbers of columns with df.train
#we have 1460 rows of training, 1459 of test.
df.test$SalePrice <- NA
data<-rbind(df.train,df.test)
#examine the combined dataset
glimpse(data)
summary(data)
table(sapply(data,class))


                                                    #2.EDA
#SalePrice is right skewed
ggplot(data=data[!is.na(data$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = scales::comma)

summary(data$SalePrice)

numeric.vars <- names(which(sapply(data,is.numeric)))
#use="pairwise.complete.obs" will remove data with NA
cor.data <- cor(data[numeric.vars],use="pairwise.complete.obs")
cor.data
class(cor.data)
#sort on decreasing correlations with SalePrice
cor.sorted <- as.matrix(sort(cor.data[,'SalePrice'], decreasing = TRUE))
#select only high correlations
CorHigh <- names(which(apply(cor.sorted, 1, function(x) abs(x)>0.5)))

cor.data <- cor.data[CorHigh, CorHigh]
corrplot.mixed(cor.data, tl.col="red", tl.pos = "lt")



#explore the relationship between OverallQual and SalePrice
#variance of SalePrice increases as Overall Qaulity increases
#the increasing rate of SalePrice increases as the Over Quality increase
#so we may need to log both the SalePrice and Overall Quality to capture this trend
ggplot(data=data[!is.na(data$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='red') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels =scales::comma)

#explore the relationship between GrLivArea and SalePrice
#variance of SalePrice increases as GrLiveArea increases
#need to log the GrLivArea
ggplot(data=data[!is.na(data$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black") +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels =scales::comma) +
  geom_text_repel(aes(label = ifelse(data$GrLivArea[!is.na(data$SalePrice)]>4500, rownames(data), '')))
#drop the two outliers in the right corner
data <- data[-c(524, 1299),]


#3.Dealing with missing values
#Identify the missing values
md.pattern(data)
missing <- aggr(data, prop=T, number = T)
summary(missing)
glimpse(data)
table(is.na(data))
#create the missing.table dataframe with missing percentage
na.data=data.frame(is.na(data))%>%map_int(sum)
missing.table = as.data.frame(na.data)
colnames(missing.table)[1] <- 'Count'
missing.table
missing.table$Percentage <- missing.table$Count/2919
missing.table <- missing.table[missing.table$Count > 0,]
head(missing.table)
#sort the missing table
missing.table <- missing.table[order(-missing.table$Count),]
missing.table


                                    #3.Deal with the missing value
#for missing percentage bigger than 15%, we drop these columns,except for the LotFrontage
#which is a numeric variable we can apply imputation techniques.
#We also need to remove the SalePrice from the dorpping list which we need to use in training and testing
#drop.col <- missing.table[missing.table$Percentage > 0.15,]
#drop.list <- rownames(drop.col)
#remove the LotFrontage and SalePrice from the dropping name

#drop.list <- drop.list[!drop.list=='LotFrontage']

#drop.list <- drop.list[!drop.list=='SalePrice']

#drop.list

#data <- data[!names(data) %in% drop.list]

#Start to handle variables with missing percentage below 15%
#We have Garage group, Bsmt group as two major groups wih missing values

#Start with PoolQc
summary(data$PoolQC)
data$PoolQC
Qualities <- c('None' = 0, 'Po'=1, 'Fa'=2, 'Gd'=4, 'TA'=3, 'Ex'=5)
Qualities

#Replace NA with None
data[['PoolQC']] <- factor( data[['PoolQC']], levels= c(levels(data[['PoolQC']]),c('None')))
data$PoolQC[is.na(data$PoolQC)] <- 'None'
data$PoolQC <- as.character(data$PoolQC)
data$PoolQC <- as.integer(revalue(data$PoolQC, Qualities))

table(data$PoolQC)

#We have three poolqc actually have pool area number
data[data$PoolArea>0 & data$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]
#Overqual scale from 1 to 10, poolqc scale from 1 to 5
# so we use half of the overqual to impute this value for poolqc
data$PoolQC[2421] <- 2
data$PoolQC[2504] <- 3
data$PoolQC[2600] <- 2


#MiscFeature
#add None to replace missing value
summary(data$MiscFeature)
data[['MiscFeature']] <- factor( data[['MiscFeature']], levels= c(levels(data[['MiscFeature']]),c('None')))
data$MiscFeature[is.na(data$MiscFeature)] <- 'None'

#Alley
#add None to replace missing value  
summary(data$Alley)
data[['Alley']] <- factor( data[['Alley']], levels= c(levels(data[['Alley']]),c('None')))
data$Alley[is.na(data$Alley)] <- 'None'


#Fence
#add None to replace missing value 
summary(data$Fence)
data[['Fence']] <- factor( data[['Fence']], levels= c(levels(data[['Fence']]),c('None')))
data$Fence[is.na(data$Fence)] <- 'None'

#FireplaceQu
#it has ranking scale, so we label encoding after imputation 
data$FireplaceQu
summary(data$FireplaceQu)
data[['FireplaceQu']] <- factor( data[['FireplaceQu']], levels= c(levels(data[['FireplaceQu']]),c('None')))
data$FireplaceQu[is.na(data$FireplaceQu)] <- 'None'
data$FireplaceQu <- as.character(data$FireplaceQu)
data$FireplaceQu <- as.integer(revalue(data$FireplaceQu, Qualities))






#for GarageYrBlt we replace it with YearBuilt

data$GarageYrBlt[is.na(data$GarageYrBlt)] <- data$YearBuilt[is.na(data$GarageYrBlt)]

#These are categorical variables, null means not existed. We can add a new level None to this factor.
Garages <- c('GarageFinish','GarageQual','GarageCond','GarageType')
Bsmts <- c("BsmtExposure","BsmtFinType2","BsmtQual","BsmtCond","BsmtFinType1")



for (x in c(Garages, Bsmts)){
  data[[x]] <- factor( data[[x]], levels= c(levels(data[[x]]),c('None')))
  data[[x]][is.na(data[[x]])] <- "None"
}

#These are numeric variables, null probably means 0 for these attribute
Bsm.other <- c("BsmtFullBath","BsmtHalfBath","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF")
for (x in Bsm.other )    data[[x]][is.na(data[[x]])] <- 0
#Replace the MasVnrType by using its mode, None.
data[["MasVnrType"]][is.na(data[["MasVnrType"]])] <- "None"
#The corresponding variable MasVnrArea is a numeric variable, we use 0 to replace it
data[["MasVnrArea"]][is.na(data[["MasVnrArea"]])] <- 0
#Utilities only have one missing value, and all existing values are the same
#if we impute it by using mode, this variable is useless, so I drop it
table(data$Utilities)
data$Utilities <- NULL
#Garages' numeric variables, we use 0 to replace
Garage.other <- c("GarageCars","GarageArea")
for (x in Garage.other )    data[[x]][is.na(data[[x]])] <- 0
#Remaining variables only have a small amount of missing values, we use the mode
Rem <- c("MSZoning","Functional","Exterior1st","Exterior2nd","KitchenQual","Electrical","SaleType")
for (x in Rem)    {
  data[[x]][is.na(data[[x]])] <- levels(data[[x]])[which.max(table(data[[x]]))]
}

#For LotFrontage, we use advanced imputation.We replace missing values
#by using the median of their neighborhood.
#why not mean? mean is highly affected by outliers
#why use each neighborhood's median, instead of the total median?
#it could avoid weakening the variance of LotFrontage and the correlation between neighborhood and LotFrontage


for (i in 1:nrow(data)){
  if(is.na(data$LotFrontage[i])){
    data$LotFrontage[i] <- as.integer(median(data$LotFrontage[data$Neighborhood==data$Neighborhood[i]], na.rm=TRUE)) 
  }
}

na.data2=data.frame(is.na(data))%>%map_int(sum)
missing.table2 = as.data.frame(na.data2)
colnames(missing.table2)[1] <- 'Count'
missing.table2
data$BsmtFinType1









                                      #4. Find ordinal data and do Label encoding

#Land slope  

data$LandSlope <- as.character(data$LandSlope)
data$LandSlope<-as.integer(revalue(data$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))
table(data$LandSlope)

#AC
#Qualities are factors, it's tricky because it returns level index when revalue


glimpse(data$HeatingQC)
summary(data$HeatingQC)
data$HeatingQC <- as.character(data$HeatingQC)
glimpse(data$HeatingQC)
#if we don't transform the factor into character first, even though we
#revalue successfully, when we transform factor directly into 
#inteagers, it will give us the index of the level as number,
#instead of the level itself
data$HeatingQC<-as.integer(revalue(data$HeatingQC, Qualities))

table(data$HeatingQC)

data$CentralAir <- as.character(data$CentralAir)
data$CentralAir<-as.integer(revalue(data$CentralAir, c('N'=0, 'Y'=1)))
table(data$CentralAir)


#Street Pavement
data$Street <- as.character(data$Street)
data$Street<-as.integer(revalue(data$Street, c('Grvl'=0, 'Pave'=1)))
table(data$Street)

#Driveway Pavement
data$PavedDrive <- as.character(data$PavedDrive)
data$PavedDrive<-as.integer(revalue(data$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
table(data$PavedDrive)

#LotShape
summary(data$LotShape)
data$LotShape <- as.character(data$LotShape)
data$LotShape<-as.integer(revalue(data$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
table(data$LotShape)

#GarageFinish 
summary(data$GarageFinish)

Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
data$GarageFinish <- as.character(data$GarageFinish)
data$GarageFinish<-as.integer(revalue(data$GarageFinish, Finish))
table(data$GarageFinish)

#GarageQual  
summary(data$GarageQual)
data$GarageQual <- as.character(data$GarageQual)
data$GarageQual<-as.integer(revalue(data$GarageQual, Qualities))
table(data$GarageQual)

#GarageCond 

summary(data$GarageCond)
data$GarageCond <- as.character(data$GarageCond)
data$GarageCond<-as.integer(revalue(data$GarageCond, Qualities))
table(data$GarageCond)

#BsmtQual 
summary(data$BsmtQual)
data$BsmtQual <- as.character(data$BsmtQual)
data$BsmtQual<-as.integer(revalue(data$BsmtQual,Qualities))
table(data$BsmtQual)

#BsmtCond 
summary(data$BsmtCond)
data$BsmtCond <- as.character(data$BsmtCond)
data$BsmtCond<-as.integer(revalue(data$BsmtCond,Qualities))
table(data$BsmtCond)

#BsmtExposure   
summary(data$BsmtExposure)
data$BsmtExposure <- as.character(data$BsmtExposure)
data$BsmtExposure<-as.integer(revalue(data$BsmtExposure,c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)))
table(data$BsmtExposure)

#BsmtFinType1 
summary(data$BsmtFinType1)
data$BsmtFinType1 <- as.character(data$BsmtFinType1)
data$BsmtFinType1<-as.integer(revalue(data$BsmtFinType1,c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)))
table(data$BsmtFinType1)


#BsmtFinType2 
summary(data$BsmtFinType2)
data$BsmtFinType2 <- as.character(data$BsmtFinType2)
data$BsmtFinType2<-as.integer(revalue(data$BsmtFinType2,c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)))
table(data$BsmtFinType2)


#MasVnrType  
summary(data$MasVnrType)
data$MasVnrType <- as.character(data$MasVnrType)
data$MasVnrType<-as.integer(revalue(data$MasVnrType,c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)))
table(data$MasVnrType)


#Kitchen quality 
summary(data$KitchenQual)
data$KitchenQual <- as.character(data$KitchenQual)
data$KitchenQual<-as.integer(revalue(data$KitchenQual,Qualities))
table(data$KitchenQual)

#Functional 
summary(data$Functional)
data$Functional <- as.character(data$Functional)
data$Functional<-as.integer(revalue(data$Functional,c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(data$Functional)

#ExterQual 
summary(data$ExterQual)
data$ExterQual <- as.character(data$ExterQual)
data$ExterQual<-as.integer(revalue(data$ExterQual,Qualities))
table(data$ExterQual)

#ExterCond  
summary(data$ExterCond)
data$ExterCond <- as.character(data$ExterCond)
data$ExterCond<-as.integer(revalue(data$ExterCond,Qualities))
table(data$ExterCond)





                                    #5 Turn some numeric data into factors 
#year and month(I need to use year of sold to create new feature as a numeric varibale, so we wait
#unitl that new feature created to change transform the year)
data$MoSold <- as.factor(data$MoSold)
data$MSSubClass <- as.factor(data$MSSubClass)

#Grage type chnage the wrong number
data$GarageYrBlt[2593] <- 2007
#6.Feature engineering 
#create the total bathrooms
data$TotBathrooms <- data$FullBath + (data$HalfBath*0.5) + data$BsmtFullBath + (data$BsmtHalfBath*0.5)
data$Remod <- ifelse(data$YearBuilt==data$YearRemodAdd, 0, 1)
data$Age <- as.numeric(data$YrSold)-data$YearRemodAdd
data$NeighRich[data$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
data$NeighRich[!data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
data$NeighRich[data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
data$TotalSqFeet <- data$GrLivArea + data$TotalBsmtSF
#Turn year into factor
data$YrSold <- as.factor(data$YrSold)
data$YearBuilt <- as.factor(data$YearBuilt)





                                        #6. Dropping correlated variables
dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')
data <- data[,!(names(data) %in% dropVars)]

glimpse(data)
set.seed(2018)
quick_RF <- randomForest(x=data[1:1458,-74], y=data$SalePrice[1:1458], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + theme(legend.position="none")


                                          #7. Fix skewness
numericVars<-data[which(sapply(data,is.numeric))]
glimpse(numericVars)
numericVars
skew <- sapply(names(numericVars),function(x){skewness(numericVars[[x]],na.rm = T)})
skew
#
skew <- skew[abs(skew) > 0.8]
count(skew)
skew
#remove SalepPrice
new_skew <- skew[-33]
new_skew
a <- names(new_skew)
a
#
for(i in a){
  data[i] <- log(data[i] + 1)
}
glimpse(data)



numericVars<-names(data[which(sapply(data,is.numeric))])
numericVars
glimpse(numericVars)
names(data) %in% numericVars
DFnumeric <- data[,names(data) %in% numericVars]
glimpse(DFnumeric)
#column 1 is the Id, column 49 is the SalePrice
DFnumeric <- DFnumeric[,-c(49,1)]
PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)
DFnorm <- predict(PreNum, DFnumeric)
DFnorm$SalePrice <- data$SalePrice
DFnorm$Id <- data$Id
glimpse(DFnorm)

DFfactors <- data[, !(names(data) %in% numericVars)]

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

                                      #8.remove low variance dummies
dummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(dummies)
#test dataset starts from 1459,identify these levels only have value 0 and remove them
Test.level.zero <- which(colSums(dummies[1459:2917,])==0)

colnames(dummies[Test.level.zero])
dummies <- dummies[,-Test.level.zero]
#training dataset starts from 1, identify these levels only have value 0 and remove them
Train.level.zero <- which(colSums(dummies[1:1458,])==0)
colnames(dummies[,Train.level.zero])

dummies <- dummies[,-Train.level.zero]
#choose the dummies who have fewer than 10 records from training dataset and remove them
less.ten <- which(colSums(dummies[1:1458,])<10)
colnames(dummies[,less.ten])
dummies <- dummies[,-less.ten]
dim(dummies)

                                #9. combined data together and split train and test
combined <- cbind(DFnorm, dummies)
df.model <- combined[1:1458,]
prediction <- combined[1459:2917,]
combined$SalePrice[1450:1470]
tail(df.model$SalePrice)
head(prediction$SalePrice)









                                    #10.multivariate regression

lm.base <- lm(log(SalePrice) ~ OverallQual*TotRmsAbvGrd + GrLivArea + GarageCars +
                + X1stFlrSF + FullBath + TotRmsAbvGrd, df.model)
summary(lm.base)
par(mfrow=c(1,4))
plot(lm.base)
plot(density(residuals(lm.base)))
predicted.value <- predict(lm.base, prediction)
predicted.value
prediction$SalePrice <- exp(predicted.value)
head(prediction$SalePrice,20)
Predictions <- data.frame(Id = prediction$Id, SalePrice = prediction$SalePrice)
head(Predictions)
write.csv(Predictions, file = "linear.csv", row.names = FALSE)


                                        #11. stepwise regression
empty=lm(log(SalePrice)~1, data=df.model)
full=lm(log(SalePrice)~ .-Id , data=df.model) 
lm.for <- step(empty, scope=list(start=empty, upper=full), direction="forward")
summary(lm.for)
plot(lm.for)
plot(density(residuals(lm.base)))
par(mfrow=c(1,1))
lm.pred <- predict(lm.for,prediction)
Predictions2 <- data.frame(Id = prediction$Id, SalePrice = exp(lm.pred))
head(Predictions2)
write.csv(Predictions2, file = "price_step.csv", row.names = FALSE)                       


                                      #12.Lasso
LASSO_formula <- as.formula( log(SalePrice)~.-Id)
x <- model.matrix(LASSO_formula, df.model)
y <- log(df.model$SalePrice)
lasso.fit<-glmnet(x = x, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")
set.seed(123456789)
lm.lasso <- cv.glmnet(x, y, alpha=1,standardize=TRUE)
summary(lm.lasso)
plot(lm.lasso)

lm.lasso$lambda.min


prediction$SalePrice <- 1
test_x <- model.matrix(LASSO_formula, prediction)
lm.pred <- predict(lm.lasso, newx = test_x, s = "lambda.min")

res <- data.frame(Id = prediction$Id, SalePrice = exp(lm.pred))
head(res)
write.csv(res, file = "price_lasso.csv", row.names = FALSE)

