#whole dataset odds accuracy 0.7542373


library(car)
library(performance)
library(leaps)
library(caret)
library(lmtest)
library(sandwich)
library(broom)
library(MASS)





test_accuracy = function(test_prediction){
  pred = ifelse(test_prediction > 0.5, 1, 0)
  true_pred = pred == test$Win
  table(true_pred)[2]/(table(true_pred)[2] + table(true_pred)[1])
}

train_accuracy = function(test_prediction){
  pred = ifelse(test_prediction > 0.5, 1, 0)
  true_pred = pred == train$Win
  table(true_pred)[2]/(table(true_pred)[2] + table(true_pred)[1])
}








df = read.csv('../data/ruud_fdf.csv')
summary(df)

sdf = df[, !(names(df) %in% c('Odd_Ruud', 'Odd_Opp', 'Odd_Acc'))]
summary(sdf)





set.seed(1)
training_index = createDataPartition(sdf$Win, p=0.7, list = FALSE) 
train = sdf[training_index,]
test = sdf[-training_index,]


# LINEAR REGRESSION

linreg = lm(Win ~ .  - Hard - Rank_Ruud - Rank_Opp - Odd_Pred, data = train)
summary(linreg)
vif(linreg)
check_model(linreg)

#heteroscedastic errors
robust_fit = coeftest(linreg, vcov = sandwich)
robust_fit


ols_test_predictions = predict.lm(linreg,newdata = test)

test_accuracy(ols_test_predictions)
# 0.6857143  

ols_train_predictions = predict.lm(linreg,newdata = train)
train_accuracy(ols_train_predictions)
# 0.7650602 

mean((test$Win - predict.lm(linreg,newdata = test))^2)
# test mse 0.2348946
mean((train$Win - predict.lm(linreg,newdata = train))^2)
# train mse 0.1669494






#best subset selection
regfit.full=regsubsets(Win ~.  - Hard - Rank_Ruud - Rank_Opp - Odd_Pred,data=train, nvmax=14)
reg.summary=summary(regfit.full)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
plot(regfit.full,scale="Cp")


linreg2 = lm(Win ~ Log_Rank_Ruud + Log_Rank_Opp + Clay + Indoor + Opp_Court_Perc + H2H_Ruud, data = train)
summary(linreg2)
vif(linreg2)
check_model(linreg2)

ols_test_predictions2 = predict.lm(linreg2,newdata = test)

test_accuracy(ols_test_predictions2)
# 0.7 

ols_train_predictions2 = predict.lm(linreg2,newdata = train)
train_accuracy(ols_train_predictions2)
# 0.753012  



mean((test$Win - predict.lm(linreg2,newdata = test))^2)
# test mse 0.2252251
mean((train$Win - predict.lm(linreg2,newdata = train))^2)
# train mse 0.172421






raw_res = residuals(linreg2)


hist(raw_res, breaks = 30, main = "Histogram of Raw Residuals", xlab = "Raw Residuals")

cooks_dist = cooks.distance(linreg2)
plot(cooks_dist, raw_res, main = "Cook's Distance vs. Raw Residuals",
     xlab = "Cook's Distance", ylab = "Raw Residuals")



# LOGISTIC REGRESSION





logistic = glm(Win ~ .- Hard - Rank_Ruud - Rank_Opp - Odd_Pred, data = train, family = binomial(link = 'logit'))
tidy(logistic)
summary(logistic)

hist(fitted(logistic))

logistic_test_predictions = predict(logistic, newdata = test, type = "response")


test_accuracy(logistic_test_predictions)
# 0.6714286  
 

logistic_train_predictions = predict(logistic, newdata = train, type = "response")
train_accuracy(logistic_train_predictions)
# 0.7771084   


mean((test$Win - predict(logistic,newdata = test, type = "response"))^2)
# test mse 0.2290769
mean((train$Win - predict(logistic,newdata = train, type = "response"))^2)
# train mse 0.1632111



vif(logistic)
check_model(logistic)




# heteroscedasticity


robust_se = sqrt(diag(vcovHC(logistic)))
z_values = coef(logistic) / robust_se
p_values = 2 * (1 - pnorm(abs(z_values)))


results = data.frame(
  Coefficients = coef(logistic),
  Robust_SE = robust_se,
  Z_value = z_values,
  P_value = p_values
)

get_significance_code <- function(p_value) {
  if (p_value < 0.001) return("***")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  if (p_value < 0.1) return(".")
  return(" ")
}


results$Significance <- sapply(results$P_value, get_significance_code)

results
print('Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1')


# best subset selection
logistic2 = glm(Win ~ Log_Rank_Ruud + Log_Rank_Opp + Clay + Indoor + Opp_Court_Perc + H2H_Ruud, data = train, family = binomial(link = 'logit'))
tidy(logistic2)
summary(logistic2)





hist(fitted(logistic2))

logistic2_test_predictions = predict(logistic2, newdata = test, type = 'response')

test_accuracy(logistic2_test_predictions)
# 0.7   

logistic2_train_predictions = predict(logistic2, newdata = train, type = 'response')
train_accuracy(logistic2_train_predictions)
# 0.753012   


mean((test$Win - predict(logistic2,newdata = test, type = "response"))^2)
# test mse 0.2192666
mean((train$Win - predict(logistic2,newdata = train, type = "response"))^2)
# train mse 0.1678491



vif(logistic2)
check_model(logistic2)


cooks_dist_logit = cooks.distance(logistic)
#plot(cooks_dist_logit, raw_res_logit, main = "Cook's Distance vs. Raw Residuals",
 #   xlab = "Cook's Distance", ylab = "Raw Residuals")




# ROBUST OLS


ols_robust = rlm(Win ~ .  - Hard - Rank_Ruud - Rank_Opp - Odd_Pred, data = train, se_type = "HC2")
summary(ols_robust)


robust_test_predictions = predict.lm(ols_robust,newdata = test)

test_accuracy(robust_test_predictions)
# 0.7  

robust_train_predictions = predict.lm(ols_robust,newdata = train)
train_accuracy(robust_train_predictions)
# 0.7831325  

mean((test$Win - predict.lm(ols_robust,newdata = test))^2)
# test mse 0.2425618
mean((train$Win - predict.lm(ols_robust,newdata = train))^2)
# train mse 0.1689589




ols_robust2 = rlm(Win ~ Log_Rank_Ruud + Log_Rank_Opp + Clay + Indoor + Opp_Court_Perc + H2H_Ruud, data = train, se_type = "HC2")
summary(ols_robust2)


robust2_test_predictions = predict.lm(ols_robust2,newdata = test)

test_accuracy(robust2_test_predictions)
# 0.7 

robust2_train_predictions = predict.lm(ols_robust2,newdata = train)
train_accuracy(robust2_train_predictions)
# 0.753012  


mean((test$Win - predict.lm(ols_robust2,newdata = test))^2)
# test mse 0.2312195
mean((train$Win - predict.lm(ols_robust2,newdata = train))^2)
# train mse 0.1738739



plot(predict.lm(linreg2,newdata = sdf))
plot(predict(logistic2,newdata = sdf, type = "response"))
plot(predict.lm(ols_robust2,newdata = sdf))


confrontation = data.frame(
  True = df$Win,
  Odd = df$Odd_Pred,
  Ols = predict.lm(linreg2,newdata = sdf),
  Logit = predict(logistic2,newdata = sdf, type = "response"),
  Robust = predict.lm(ols_robust2,newdata = sdf)
)
head(confrontation)

confrontation$Ols <- ifelse(confrontation$Ols < 0.5, 0, 1)
confrontation$Logit <- ifelse(confrontation$Logit < 0.5, 0, 1)
confrontation$Robust <- ifelse(confrontation$Robust < 0.5, 0, 1)


confrontation_test = data.frame(
  True = test$Win,
  Odd = test$Odd_Pred,
  Ols = predict.lm(linreg2,newdata = test),
  Logit = predict(logistic2,newdata = test, type = "response"),
  Robust = predict.lm(ols_robust2,newdata = test)
)

confrontation_test$Ols <- ifelse(confrontation_test$Ols < 0.5, 0, 1)
confrontation_test$Logit <- ifelse(confrontation_test$Logit < 0.5, 0, 1)
confrontation_test$Robust <- ifelse(confrontation_test$Robust < 0.5, 0, 1)



head(confrontation_test)


confrontation_wrong_odd = confrontation_test[confrontation_test$Odd != confrontation_test$True, ]
confrontation_wrong_odd


count_equal = colSums(confrontation_wrong_odd == confrontation_wrong_odd$True)
count_equal


ce = colSums(confrontation_test== confrontation_test$True)
ce
ceperc = ce/ce[1]
ceperc






# RG 2023

rg = read.csv('../data/ruud_fdf_rg.csv')
rg

plot(predict.lm(linreg2,newdata = rg))
plot(predict(logistic2,newdata = rg, type = "response"))
plot(predict.lm(ols_robust2,newdata = rg))




confrontation_rg = data.frame(
  True = rg$Win,
  Odd = rg$Odd_Pred,
  Ols = predict.lm(linreg2,newdata = rg),
  Logit = predict(logistic2,newdata = rg, type = "response"),
  Robust = predict.lm(ols_robust2,newdata = rg)
)

confrontation_rg

confrontation_rg$Ols <- ifelse(confrontation_rg$Ols < 0.5, 0, 1)
confrontation_rg$Logit <- ifelse(confrontation_rg$Logit < 0.5, 0, 1)
confrontation_rg$Robust <- ifelse(confrontation_rg$Robust < 0.5, 0, 1)



confrontation_rg



count_equal_rg = colSums(confrontation_rg == confrontation_rg$True)
count_equal_rg



ceperc_rg = count_equal_rg/count_equal_rg[1]
ceperc_rg








