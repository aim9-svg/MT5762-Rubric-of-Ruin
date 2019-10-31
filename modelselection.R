library(multcomp)
library(car) 
library(MASS) 
library(leaps)

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
qqPlot(lm(baby_wt~cig_number, data = test_cig), simulate = TRUE, main = 'QQ Plot', labels = FALSE)
bartlett.test(baby_wt~cig_number, data = test_cig)
fit7 <- aov(baby_wt~cig_number, data = test_cig)
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

lm1 <- lm(baby_wt ~ ., data = lmData )
summary(lm1)
lms1 <- stepAIC(lm1,direction="backward")
summary(lms1)

lms1.full <- regsubsets(baby_wt~.,data=lmData,nvmax = 12) 
lms1.fwd <- regsubsets(baby_wt~., data=lmData,nvmax = 12,method = "forward")
lms1.bwd <- regsubsets(baby_wt~., data=lmData,nvmax = 12,method = "backward")

lms1.full.summary <- summary(lms1.full)
plot(lms1.full.summary$bic,xlab="Number of Variables",ylab="BIC",type = "l")
plot(lms1.full.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type = "l")

coef(lms1.full,7)
coef(lms1.fwd,7)                      
coef(lms1.bwd,7)  

# lm.f1 based on AIC
lm.f1 <- lms1
# lm.f2 generated by 'lms1.full'
lm.f2 <- lm(baby_wt~gestation + m_ht + smoke + m_race + d_race + previous_preg + d_wt, data = lmData )
# lm.f5 generated by 'lm.bwd'
lm.f3 <- lm(baby_wt~gestation + m_ht + smoke + d_race + previous_preg + d_wt , data = lmData )
# other efforts 
lm.f4 <- lm(baby_wt~gestation + m_ht + smoke + m_race + d_race + previous_preg + d_wt + m_wt, data = lmData )
lm.f5 <- lm(baby_wt~gestation + m_ht + smoke + m_wt + d_race + previous_preg + d_wt , data = lmData )


