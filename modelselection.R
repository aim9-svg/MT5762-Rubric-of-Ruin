library(multcomp)
library(car) 
library(MASS) 
library(leaps)
library(effects)

# Explore the difference of 'baby_wt' in different 'm_race' groups
# Convert grouping columns to factor variables  
babies_data$m_race <- factor(babies_data$m_race)
# QQ-plot checks whether the data conforms to normal distribution
qqPlot(lm(baby_wt~m_race, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
# Bartlett test is used to test homogeneity of variance 
# p > 0.05 indicates homogeneity of variance
# p-value = 0.7052
bartlett.test(baby_wt~m_race, data = babies_data)
# One-Way analysis of variance (ANOVA) is performed when the hypothesis of the test is satisfied
# p value is much lower than 0.05 level, p.value = 0.000283 ***
# The results showed significant differences in the weight of children born from mothers who are different races
fit <- aov(baby_wt~m_race, data = babies_data)
summary(fit)

# Explore the difference of 'baby_wt' in different 'd_race' groups
# Repeat the process above
babies_data$d_race <- factor(babies_data$d_race)
qqPlot(lm(baby_wt~d_race, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
bartlett.test(baby_wt~d_race, data = babies_data)
fit2 <- aov(baby_wt~d_race, data = babies_data)
# p value = 3.24e-05 ***, There are significant differences between groups
summary(fit2)

# Explore the difference of 'baby_wt' in different 'm_edu' groups
# Repeat the process above
babies_data$m_edu <- factor(babies_data$m_edu)
qqPlot(lm(baby_wt~m_edu, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
bartlett.test(baby_wt~m_edu, data = babies_data)
fit3 <- aov(baby_wt~m_edu, data = babies_data)
# p value = 0.39, There was no significant difference between groups
summary(fit3)

# Explore the difference of 'baby_wt' in different 'd_edu' groups
# Repeat the process above
babies_data$d_edu <- factor(babies_data$d_edu)
qqPlot(lm(baby_wt~d_edu, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
bartlett.test(baby_wt~d_edu, data = babies_data)
fit4 <- aov(baby_wt~d_edu, data = babies_data)
# p value = 0.0417 *, There was no more significant difference between groups
summary(fit4)

# Explore the difference of 'baby_wt' in different 'smoke' groups
# Repeat the process above
babies_data$smoke <- factor(babies_data$smoke)
qqPlot(lm(baby_wt~smoke, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
bartlett.test(baby_wt~smoke, data = babies_data)
fit5 <- aov(baby_wt~smoke, data = babies_data)
# p value = 1.28e-07 ***, There are significant differences between groups
summary(fit5)

# Explore the difference of 'baby_wt' in different 'smoke_time' groups
# Repeat the process above
babies_data$smoke_time <- factor(babies_data$smoke_time)
qqPlot(lm(baby_wt~smoke_time, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
bartlett.test(baby_wt~smoke_time, data = babies_data)
fit6 <- aov(baby_wt~smoke_time, data = babies_data)
# p value = 1.23e-06 ***, There are significant differences between groups
summary(fit6)

# Explore the difference of 'baby_wt' in different 'cig_number' groups
# Repeat the process above
babies_data$cig_number <- factor(babies_data$cig_number)
qqPlot(lm(baby_wt~cig_number, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
bartlett.test(baby_wt~cig_number, data = babies_data)
fit7 <- aov(baby_wt~cig_number, data = babies_data)
# p value = 1.66e-05 ***, There are significant differences between groups
summary(fit7)

# Explore the difference of 'baby_wt' in different 'income' groups
# Repeat the process above
babies_data$income <- factor(babies_data$income)
qqPlot(lm(baby_wt~income, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
bartlett.test(baby_wt~income, data = babies_data)
fit8 <- aov(baby_wt~income, data = babies_data)
# p value = 0.764, There is no significant difference between groups
summary(fit8)

# Given that the contents of the two variables 'smoke' and 'smoke_time' overlap, choose 'smoke' from them
# smoke and cig_number
# two-way analysis of variance(ANOVA) is used to investigate whether 'smoke' and 'cig_number' had a significant effect on 'baby_wt'
# Repeat the process above for two factors
babies_data$smoke <- factor(babies_data$smoke)
babies_data$cig_number <- factor(babies_data$cig_number)
par(mfrow = c(1, 2))
qqPlot(lm(baby_wt~cig_number, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
qqPlot(lm(baby_wt~smoke, data = babies_data), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
par(mfrow = c(1,1))
bartlett.test(baby_wt~smoke, data = babies_data)
bartlett.test(baby_wt~cig_number, data = babies_data)
fit9 <- aov(baby_wt~smoke*cig_number, data = babies_data)
# The results show that only 'smoke' has a significant effect on 'baby_wt', 
# and the interaction between 'smoke' and 'cig_number' is not significant 
summary(fit9)

# According to the test results, remove insignificant items
lmData <- babies_data %>% dplyr::select(-m_edu, -d_edu, -income, -cig_number, -smoke_time)
lmData

# Establish a linear regression model of all variables
lm1 <- lm(baby_wt ~ ., data = lmData )
summary(lm1)

# Stepwise regression analysis

## 1. Based on Akaike Information Criterion (AIC)
## AIC can be used to compare models, taking into account the statistical fit of the model and the number of parameters used to fit
## The model with a smaller AIC value should be selected optimally, which means that the model has obtained sufficient fitting degree with fewer parameters
lms1 <- stepAIC(lm1)
## Get the model formula of linear regression 
lms1$call
#formula.AIC = baby_wt ~ gestation + m_ht + d_race + d_wt + smoke + previous_preg

## Show the detailed results of the fitting
summary(lms1)

## 2. All-subsets regression
## It attempts to model all the combinations of features and then selects the best model
lms1.full <- regsubsets(baby_wt~.,data=lmData,nvmax = 12) 
## 3. Forward stepwise regression
## Add a predictive variable to the model one at a time until the addition of the variable does not improve the model
lms1.fwd <- regsubsets(baby_wt~., data=lmData,nvmax = 12,method = "forward")
## 4. Backward stepwise regression
## Build a model that contains all the predictive variables, then delete one variable at a time until the model quality is reduced
lms1.bwd <- regsubsets(baby_wt~., data=lmData,nvmax = 12,method = "backward")

## The results of stepwise regression analysis for 'All-subsets', 'Forward stepwise', 'Backward stepwise' regression   
lms1.full.summary <- summary(lms1.full)
lms1.fwd.summary <- summary(lms1.fwd)
lms1.bwd.summary <- summary(lms1.bwd)

# Plot the changes of Bayesian Information Criterion(BIC) and Adjusted R-squared according to the number of variables
## The smaller the BIC value, the better the fit of the model
## Adjusted R-squared can measure the goodness of fit of the model, the bigger it is, the better the fit of the model is

### 1.For All-subsets regression
### The BIC value is the smallest when the number of variables is 7
plot(lms1.full.summary$bic,xlab="Number of Variables",ylab="BIC",type = "l")
which.min(lms1.full.summary$bic)
points(which.min(lms1.full.summary$bic),lms1.full.summary$bic[7],col="red",cex=2,pch=20)
### The Adjusted R-squared value is the smallest when the number of variables is 9
plot(lms1.full.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type = "l")
which.max(lms1.full.summary$adjr2)
points(which.max(lms1.full.summary$adjr2),lms1.full.summary$adjr2[9],col="red",cex=2,pch=20)

### 2.For Forward stepwise regression
### The BIC value is the smallest when the number of variables is 7
plot(lms1.fwd.summary$bic,xlab="Number of Variables",ylab="BIC",type = "l")
which.min(lms1.fwd.summary$bic)
points(which.min(lms1.fwd.summary$bic),lms1.fwd.summary$bic[7],col="red",cex=2,pch=20)
### The Adjusted R-squared value is the smallest when the number of variables is 9
plot(lms1.fwd.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type = "l")
which.max(lms1.fwd.summary$adjr2)
points(which.max(lms1.fwd.summary$adjr2),lms1.fwd.summary$adjr2[9],col="red",cex=2,pch=20)

### 3.For Backward stepwise regression
### The BIC value is the smallest when the number of variables is 7
plot(lms1.bwd.summary$bic,xlab="Number of Variables",ylab="BIC",type = "l")
which.min(lms1.bwd.summary$bic)
points(which.min(lms1.bwd.summary$bic),lms1.bwd.summary$bic[7],col="red",cex=2,pch=20)
### The Adjusted R-squared value is the smallest when the number of variables is 10
plot(lms1.bwd.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type = "l")
which.max(lms1.bwd.summary$adjr2)
points(which.max(lms1.bwd.summary$adjr2),lms1.bwd.summary$adjr2[10],col="red",cex=2,pch=20)

## Get the model formula of linear regression 
coef(lms1.full,7)
### formula.full.7 = baby_wt~gestation+m_race+m_ht+d_race+d_wt+smoke+previous_preg
                   
coef(lms1.full,9)
### formula.full.9 = baby_wt~gestation+m_race+m_ht+m_wt+d_race+d_wt+smoke+previous_preg

coef(lms1.fwd,7)
### formula.fwd.7 = baby_wt~gestation+m_race+m_ht+d_race+d_wt+smoke+previous_preg
coef(lms1.fwd,9)
### formula.fwd.9 = baby_wt~gestation+m_race+m_ht+m_wt+d_race+d_wt+smoke+previous_preg
coef(lms1.bwd,7) 
### formula.bwd.7 = baby_wt~gestation+m_ht+d_race+d_wt+smoke+previous_preg
              
coef(lms1.bwd,10)
### formula.bwd.7 = baby_wt~gestation+m_race+m_ht+m_wt+d_race+d_wt+smoke+previous_preg
                    
    
# According to the above results 
# a regression model is obtained according to AIC
# a total of six regression models are obtained according to the stepwise regression method
# Remove duplicate models
# Finally get three models:
# baby_wt~gestation+m_race+m_ht+d_race+d_wt+smoke+previous_preg
# baby_wt~gestation+m_race+m_ht+m_wt+d_race+d_wt+smoke+previous_preg
# baby_wt~gestation+m_ht+d_race+d_wt+smoke+previous_preg

lm.f1 <- lm(baby_wt~gestation+m_ht+d_race+d_wt+smoke+previous_preg, data = lmData)

lm.f2 <- lm(baby_wt~gestation+m_race+m_ht+d_race+d_wt+smoke+previous_preg, data = lmData )

lm.f3 <- lm(baby_wt~gestation+m_race+m_ht+m_wt+d_race+d_wt+smoke+previous_preg , data = lmData )

# Consider first-order interactions as part of pool of models
#1. Consider all first-order interactions in 'lm.f1' model
lm.f1.foi <- lm(baby_wt~(gestation+m_ht+d_race+d_wt+smoke+previous_preg)^2, data = lmData)
lmsf1 <- step(lm.f3.foi)
summary(lmsf1)
# With the effect () function in the effects package
# it can graphically display the results of the interaction items
plot(effect("gestation:m_race", lmsf1), multiline=TRUE)
plot(effect("gestation:m_wt", lmsf1), multiline=TRUE)
plot(effect("gestation:previous_preg", lmsf1), multiline=TRUE)
plot(effect("m_wt:d_wt", lmsf1), multiline=TRUE)

#2. Consider all first-order interactions in 'lm.f2' model
lm.f2.foi <- lm(baby_wt~(gestation+m_race+m_ht+d_race+d_wt+smoke+previous_preg)^2, data = lmData )
lmsf2 <- step(lm.f2.foi)
summary(lmsf2)
# Plot the results of the interactions
plot(effect("gestation:m_race", lmsf2), multiline=TRUE)
plot(effect("gestation:previous_preg", lmsf2), multiline=TRUE)

#2. Consider all first-order interactions in 'lm.f3' model
lm.f3.foi <- lm(baby_wt~(gestation+m_race+m_ht+m_wt+d_race+d_wt+smoke+previous_preg)^2, data = lmData )
# Use stepwise method to improve the model
lmsf3 <- step(lm.f3.foi)
summary(lmsf3)
# Plot the results of the interactions 
plot(effect("gestation:m_race", lmsf3), multiline=TRUE)
plot(effect("gestation:m_wt", lmsf3), multiline=TRUE)
plot(effect("gestation:previous_preg", lmsf3), multiline=TRUE)
plot(effect("m_wt:d_wt", lmsf3), multiline=TRUE)

# Finally, remove duplicate models('lmsf1' and 'lmsf3' are the same, keep one of them)
# Keep 'lmsf1'('lmsf3') and 'lmsf2' 
# We get two final models
lm.final1 <- lm(baby_wt ~ gestation + m_race + m_ht + m_wt + d_wt + 
                  smoke + previous_preg + gestation:m_race + gestation:m_wt + 
                  gestation:previous_preg + m_wt:d_wt, data = lmData) 

lm.final2 <- lm(baby_wt ~ gestation + m_race + m_ht + d_wt + smoke + 
                  previous_preg + gestation:m_race + gestation:previous_preg, 
                data = lmData)
summary(lm.final1)
summary(lm.final2)
