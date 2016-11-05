setwd("C:/Users/Raj Anand/Documents/Rwd/House_Price")

train_ori = read.csv(file = "train.csv", header = TRUE)
test = read.csv(file = "test.csv", header = TRUE)
### Removing the variables with large number of NA values
train_id = train_ori$Id
train_ori$Id = NULL
train_ori$Fence = NULL
train_ori$MiscFeature = NULL
train_ori$BedroomAbvGr = NULL
train_ori$Alley = NULL
train_ori$PoolQC = NULL
train_ori$FireplaceQu = NULL
train_ori$Utilities = NULL

test_id = test$Id
test$Id = NULL
test$Fence = NULL
test$MiscFeature = NULL
test$BedroomAbvGr = NULL
test$Alley = NULL
test$PoolQC = NULL
test$FireplaceQu = NULL
test$Utilities = NULL

### Converting the date variable
train_ori$YearBuilt = train_ori$YearBuilt - min(train_ori$YearBuilt, na.rm = TRUE)
train_ori$YearRemodAdd = train_ori$YearRemodAdd - min(train_ori$YearRemodAdd, na.rm = TRUE)
train_ori$GarageYrBlt = train_ori$GarageYrBlt - min(train_ori$GarageYrBlt, na.rm = TRUE)
train_ori$YrSold = train_ori$YrSold - min(train_ori$YrSold, na.rm = TRUE)

test$YearBuilt = test$YearBuilt - min(test$YearBuilt, na.rm = TRUE)
test$YearRemodAdd = test$YearRemodAdd - min(test$YearRemodAdd, na.rm = TRUE)
test$GarageYrBlt = test$GarageYrBlt - min(test$GarageYrBlt, na.rm = TRUE)
test$YrSold = test$YrSold - min(test$YrSold, na.rm = TRUE)

##Coverting
train_ori$LandSlope = as.numeric(train_ori$LandSlope)
train_ori$ExterQual = factor(train_ori$ExterQual, levels = c("Fa", "TA", "Gd", "Ex"))
train_ori$ExterQual = as.numeric(train_ori$ExterQual)
train_ori$BsmtQual = factor(train_ori$BsmtQual, levels = c("Fa", "TA", "Gd", "Ex"))
train_ori$BsmtQual = as.numeric(train_ori$BsmtQual)
train_ori$BsmtCond = factor(train_ori$BsmtCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_ori$BsmtCond = as.numeric(train_ori$BsmtCond)
train_ori$ExterCond = factor(train_ori$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_ori$ExterCond = as.numeric(train_ori$ExterCond)
train_ori$BsmtExposure = factor(train_ori$BsmtExposure, levels = c("NA", "No", "Mn", "Av", "Gd"))
train_ori$BsmtExposure = as.numeric(train_ori$BsmtExposure)
train_ori$BsmtFinType1 = factor(train_ori$BsmtFinType1, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
train_ori$BsmtFinType1 = as.numeric(train_ori$BsmtFinType1)
train_ori$BsmtFinType2 = factor(train_ori$BsmtFinType2, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
train_ori$BsmtFinType2 = as.numeric(train_ori$BsmtFinType2)
train_ori$HeatingQC = factor(train_ori$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_ori$HeatingQC = as.numeric(train_ori$HeatingQC)
train_ori$KitchenQual = factor(train_ori$KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_ori$KitchenQual = as.numeric(train_ori$KitchenQual)
train_ori$GarageQual = factor(train_ori$GarageQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_ori$GarageQual = as.numeric(train_ori$GarageQual)
train_ori$GarageCond = factor(train_ori$GarageCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
train_ori$GarageCond = as.numeric(train_ori$GarageCond)



test$LandSlope = as.numeric(test$LandSlope)
test$ExterQual = factor(test$ExterQual, levels = c("Fa", "TA", "Gd", "Ex"))
test$ExterQual = as.numeric(test$ExterQual)
test$BsmtQual = factor(test$BsmtQual, levels = c("Fa", "TA", "Gd", "Ex"))
test$BsmtQual = as.numeric(test$BsmtQual)
test$BsmtCond = factor(test$BsmtCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test$BsmtCond = as.numeric(test$BsmtCond)
test$ExterCond = factor(test$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test$ExterCond = as.numeric(test$ExterCond)
test$BsmtExposure = factor(test$BsmtExposure, levels = c("NA", "No", "Mn", "Av", "Gd"))
test$BsmtExposure = as.numeric(test$ExterCond)
test$BsmtFinType1 = factor(test$BsmtFinType1, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
test$BsmtFinType1 = as.numeric(test$BsmtFinType1)
test$BsmtFinType2 = factor(test$BsmtFinType2, levels = c("NA", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
test$BsmtFinType2 = as.numeric(test$BsmtFinType2)
test$HeatingQC = factor(test$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test$HeatingQC = as.numeric(test$HeatingQC)
test$KitchenQual = factor(test$KitchenQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test$KitchenQual = as.numeric(test$KitchenQual)
test$GarageQual = factor(test$GarageQual, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test$GarageQual = as.numeric(test$GarageQual)
test$GarageCond = factor(test$GarageCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"))
test$GarageCond = as.numeric(test$GarageCond)

#### Imputation of the NA datas

library(missForest)
Imputed = missForest(train_ori[,-73], ntree = 500, decreasing = TRUE)
Comp_train = as.data.frame(Imputed$ximp)
Comp_train = cbind(Comp_train, "SalePrice" = train_ori$SalePrice)
write.csv(Comp_train, file = "Imputed_train.csv", row.names = FALSE)

Imputed_test = missForest(test, ntree = 500, decreasing = TRUE)
comp_test = as.data.frame(Imputed_test$ximp)
write.csv(comp_test, file = "Imputed_test.csv", row.names = FALSE)
