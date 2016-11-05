setwd("C:/Users/Raj Anand/Documents/Rwd/House_Price")

imp_train = read.csv(file="dummy_imp_train.csv")
imp_test = read.csv(file = "dummy_imp_test.csv")

### Training a basic model
mod1 = lm(response ~., data = imp_train) ### R is 0.9046
mod2 = lm(response ~ 1, data = imp_train)

library(MASS)
for_step = step(mod2, direction = "forward", scope = list(lower = mod2, upper = mod1)) ### AIC 29697.7
# bac_step = step(mod1, direction = "backward") ### AIC is 29634.78
# both_step = step(mod1, direction = "both", scope = list(upper = mod1)) ###  
### Forward is giving best interpretation so taking this forward

## Making dummy variable for the train and test data
# num_var = names(imp_train[which(sapply(imp_train, is.numeric))])
# cat_var = names(imp_train[which(sapply(imp_train, is.factor))])
# response = imp_train[,73]
# imp_train = imp_train[,-73]
# for(i in cat_var){
#      str1 = paste("~",i,"-1",collapse = "")
#      form = as.formula(str1)
#      model.matrix(form,data = imp_train)
#      ind = which(names(imp_train) %in% i)
#      imp_train = cbind(imp_train[,-ind],
#                        as.data.frame(model.matrix(form,data = imp_train)))
# }
# imp_train = cbind(imp_train,response)
# 
# num_var = names(imp_test[which(sapply(imp_test, is.numeric))])
# cat_var = names(imp_test[which(sapply(imp_test, is.factor))])
# for(i in cat_var){
#      str1 = paste("~",i,"-1",collapse = "")
#      form = as.formula(str1)
#      model.matrix(form,data = imp_test)
#      ind = which(names(imp_test) %in% i)
#      imp_test = cbind(imp_test[,-ind],
#                        as.data.frame(model.matrix(form,data = imp_test)))
# }
# 
# write.csv(imp_train, file = "dummy_imp_train.csv", row.names = FALSE)
# write.csv(imp_test, file = "dummy_imp_test.csv", row.names = FALSE)


### Checking MUlticollinearity with condition number
imp_train = apply(imp_train,2,as.numeric)
imp_test = apply(imp_test, 2, as.numeric)
library(car)
library(corrplot)
library(perturb)
cond_no = colldiag(for_step, scale = TRUE, center = TRUE) 
### No variables are removed by condition numberf
for_vif = vif(for_step)

### Bldgtype and MSSubClass are removed from VIF
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

###  Calculating the leverage statistics
lev_stat = hatvalues(model_new)

#### Removing the outliers from the common from the residual and lev_stat
out_lev = which(lev_stat > 3*mean(lev_stat))
out_res = which(abs(res) > 3)
out_end = intersect(out_lev, out_res)

### Removing the outlier from the training data 
imp_train = imp_train[-out_end, ]
model_new = lm(str1,imp_train)
new_vif =vif(model_new)
new_cond = colldiag(model_new, scale = TRUE, center = TRUE)


