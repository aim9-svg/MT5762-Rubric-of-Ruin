install.packages("caret")
install.packages("boot")
library(caret)
library(boot)

### Cross-Validation

## Prediction to a validation set
set.seed(20191101)
training_samples <- babies_data$baby_wt %>%
  createDataPartition(p = 0.8, list = FALSE)
train_data <- babies_data[training_samples, ]
test_data <- babies_data[-training_samples, ]

# Make prediction based lm.final1
lm.final1_train <- lm(baby_wt ~ gestation + m_race + m_ht + m_wt + d_wt + 
                        smoke + previous_preg + gestation:m_race + gestation:m_wt + 
                        gestation:previous_preg + m_wt:d_wt, data = train_data)
prediction1 <- lm.final1_train %>% predict(test_data)
data.frame(R2 = R2(prediction1, test_data$baby_wt),
           RMSE = RMSE(prediction1, test_data$baby_wt),
           MSE = (RMSE(prediction1, test_data$baby_wt))^2)

# Make prediction based lm.final2
lm.final2_train <- lm(baby_wt ~ gestation + m_race + m_ht + d_wt + smoke + 
                        previous_preg + gestation:m_race + gestation:previous_preg, 
                      data = train_data)
prediction2 <- lm.final2_train %>% predict(test_data)
data.frame(R2 = R2(prediction2, test_data$baby_wt),
           RMSE = RMSE(prediction2, test_data$baby_wt),
           MSE = (RMSE(prediction2, test_data$baby_wt))^2)

## 5-fold cross-validation



### Boostrapping 

model_coef <- function(data, index) {
  coef(lm(baby_wt ~ gestation + m_race + m_ht + m_wt + d_wt + 
            smoke + previous_preg + gestation:m_race + gestation:m_wt + 
            gestation:previous_preg + m_wt:d_wt, babies_data, subset = index))
}
results <- boot(babies_data, model_coef, 1000)

# plot histogram for the first 5 terms 
plot(results, index = 1)
plot(results, index = 2)
plot(results, index = 3)
plot(results, index = 4)
plot(results, index = 5)

# Confidence intervals via boot.ci() function
# note: bca = bias-corrected, accelerated (adjusted bootstrap percentile)
intercept_ci <- as.numeric(boot.ci(results, type = "bca", index = 1)$bca)
gestation_ci <- as.numeric(boot.ci(results, type = "bca", index = 2)$bca)
m_raceblack_ci <- as.numeric(boot.ci(results, type = "bca", index = 3)$bca)
m_racemex_ci <- as.numeric(boot.ci(results, type = "bca", index = 4)$bca)
m_racemixed_ci <- as.numeric(boot.ci(results, type = "bca", index = 5)$bca)
m_racewhite_ci <- as.numeric(boot.ci(results, type = "bca", index = 6)$bca)
m_ht_ci <- as.numeric(boot.ci(results, type = "bca", index = 7)$bca)
m_wt_ci <- as.numeric(boot.ci(results, type = "bca", index = 8)$bca)
dwt_ci <- as.numeric(boot.ci(results, type = "bca", index = 9)$bca)
smokeonce_did_not_now_ci <- as.numeric(boot.ci(results, type = "bca", index = 10)$bca)
smokesmokes_now_ci <- as.numeric(boot.ci(results, type = "bca", index = 11)$bca)
smokeuntil_current_pregnancy_ci <- as.numeric(boot.ci(results, type = "bca", index = 12)$bca)
previous_preg_ci <- as.numeric(boot.ci(results, type = "bca", index = 13)$bca)
gestation_m_raceblack_ci <- as.numeric(boot.ci(results, type = "bca", index = 14)$bca)
gestation_m_racemex_ci <- as.numeric(boot.ci(results, type = "bca", index = 15)$bca)
gestation_m_racemixed_ci <- as.numeric(boot.ci(results, type = "bca", index = 16)$bca)
gestation_m_racewhite_ci <- as.numeric(boot.ci(results, type = "bca", index = 17)$bca)
gestation_m_wt_ci <- as.numeric(boot.ci(results, type = "bca", index = 18)$bca)
gestation_previous_preg_ci <- as.numeric(boot.ci(results, type = "bca", index = 19)$bca)
m_wt_d_wt_ci <- as.numeric(boot.ci(results, type = "bca", index = 20)$bca)

boot_coef <- as.data.frame(matrix(c(intercept_ci[4], intercept_ci[5], gestation_ci[4], gestation_ci[5],
                                    m_raceblack_ci[4], m_raceblack_ci[5], m_racemex_ci[4], m_racemex_ci[5], 
                                    m_racemixed_ci[4], m_racemixed_ci[5], m_racewhite_ci[4], m_racewhite_ci[5], 
                                    m_ht_ci[4], m_ht_ci[5], m_wt_ci[4], m_wt_ci[5], dwt_ci[4], dwt_ci[5],
                                    smokeonce_did_not_now_ci[4], smokeonce_did_not_now_ci[5],
                                    smokesmokes_now_ci[4], smokesmokes_now_ci[5], smokeuntil_current_pregnancy_ci[4], 
                                    smokeuntil_current_pregnancy_ci[5], previous_preg_ci[4], previous_preg_ci[5], 
                                    gestation_m_raceblack_ci[4], gestation_m_raceblack_ci[5], gestation_m_racemex_ci[4],
                                    gestation_m_racemex_ci[5], gestation_m_racemixed_ci[4], gestation_m_racemixed_ci[5],
                                    gestation_m_racewhite_ci[4], gestation_m_racewhite_ci[5], gestation_m_wt_ci[4],
                                    gestation_m_wt_ci[5], gestation_previous_preg_ci[4], gestation_previous_preg_ci[5],
                                    m_wt_d_wt_ci[4], m_wt_d_wt_ci[5]), ncol = 2, byrow = TRUE))
colnames(boot_coef) <- c("boot_upper","boot_lower")

## Confidence intervals via summary(lm()) function
lm_coef <- as.data.frame(confint(lm.final1))

# Compare two CIs
comparison <- cbind(as.data.frame(summary(lm.final1)$coefficient[, 1]), lm_coef, boot_coef)
comparison <- round(comparison, 5)

### Reference
# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r
# http://www.sthda.com/english/articles/38-regression-model-validation/156-bootstrap-resampling-essentials-in-r
# https://www.statmethods.net/advstats/bootstrapping.html