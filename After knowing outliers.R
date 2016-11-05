setwd("C:/Users/Raj Anand/Documents/Rwd/House_Price")

imp_train = read.csv(file="dummy_imp_train.csv")
imp_test = read.csv(file = "dummy_imp_test.csv")

### Training a basic model
mod1 = lm(response ~., data = imp_train) ### R is 0.9046
mod2 = lm(response ~ 1, data = imp_train)

### We know that forward selection works best 
library(MASS)
for_step = step(mod2, direction = "forward", scope = list(lower = mod2, upper = mod1)) ### AIC 29697.7


#### Applying the condition number technique
imp_train = apply(imp_train,2,as.numeric)
imp_test = apply(imp_test, 2, as.numeric)
library(car)
library(corrplot)
library(perturb)
cond_no = colldiag(for_step, scale = TRUE, center = TRUE) 
### No variables are removed by condition numberf

#### Applying the VIF to check for multicollinearity
for_vif = vif(for_step)
str1  = variable.names(for_step)
str1 = paste(str1,collapse = "+")
str1 = paste("response~",str1,collapse = "")
str1 = as.formula(str1)
model_new = lm(str1,imp_train)
new_vif =vif(model_new)
new_cond = colldiag(model_new, scale = TRUE, center = TRUE)
res = studres(model_new)
#### Plotting fitted value against predicted values
plot(model_new$fitted.values, res)
###  Calculating the leverage statistics
lev_stat = hatvalues(model_new)
#### Removing the outliers from the common from the residual and lev_stat
out_lev = which(lev_stat > 3*mean(lev_stat))
out_res = which(abs(res) > 3)
out_end = intersect(out_lev, out_res)
### Removing the outlier from the training data 
imp_train = imp_train[-out_end, ]


#### Now the outliers are removed and we will now again train the model from scratch 
str1  = variable.names(for_step)
str1 = str1[-1]
str1 = str1[-which(str1 == "Condition2PosN")]
str1 = str1[-which(str1 == "HouseStyle1.5Unf")]
str1 = str1[-which(str1 == "Exterior2ndOther")]
str1 = str1[-which(str1 == "MSSubClass")]
str1 = str1[-which(str1 == "LandContourLvl")]
str1 = str1[-which(str1 == "LotShapeIR2")]
str1 = str1[-which(str1 == "Exterior1stImStucc")]
str1 = str1[-which(str1 == "ExterCond")]
str1 = str1[-which(str1 == "BldgTypeTwnhs")]
str1 = str1[-which(str1 == "Exterior2ndImStucc")]
str1 = str1[-which(str1 == "BldgTypeDuplex")]
str1 = str1[-which(str1 == "SaleTypeCon")]
str1 = str1[-which(str1 == "KitchenAbvGr")]
str1 = str1[-which(str1 == "Fireplaces")]
str1 = paste(str1,collapse = "+")
str1 = paste("response~",str1,collapse = "")
str1 = as.formula(str1)
model_new = lm(str1,imp_train)
new_vif =vif(model_new)
new_cond = colldiag(model_new, scale = TRUE, center = TRUE)
res = studres(model_new)
#### Plotting fitted value against predicted values
plot(model_new$fitted.values, res)


imp_test$Condition2RRAe = 0
imp_test$Condition2RRAn =0
imp_test$Condition2RRNn =0
imp_test$HouseStyle2.5Fin =0
imp_test$RoofMatlClyTile=0
imp_test$RoofMatlMembran=0
imp_test$RoofMatlMetal=0
imp_test$RoofMatlRoll=0
imp_test$Exterior1stImStucc=0
imp_test$Exterior1stStone=0
imp_test$Exterior2ndOther=0
imp_test$HeatingFloor =0
imp_test$HeatingOthW =0
imp_test$ElectricalMix =0

test_id = read.csv(file = "test.csv")
test_id = test_id$Id
pred1 = predict(model_new, newdata = imp_test)
submit = as.data.frame(cbind("Id" = test_id, "SalePrice" = pred1))
write.csv(submit, file = "Submission2.csv", row.names = FALSE)
