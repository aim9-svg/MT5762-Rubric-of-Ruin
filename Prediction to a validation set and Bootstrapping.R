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
set.seed(20191102)
results <- boot(babies_data, model_coef, 1000)

# plot histogram for the first 2 terms 
plot(results, index = 1)
plot(results, index = 2)

# Confidence intervals via boot.ci() function
# note: bca = bias-corrected, accelerated (adjusted bootstrap percentile)
get_bootcoef <- function(x) {
  boot_coef <- data.frame(matrix(NA, ncol = 2, nrow = 20))
  colnames(boot_coef) <- c("boot_upper","boot_lower")
  for (i in 1:20) {
    ci <- as.numeric(boot.ci(results, type = "bca", index = i)$bca)
    boot_coef[i, ] <- (c(ci[4], ci[5]))
  }
  return(boot_coef) 
}

boot_coef <- get_bootcoef()

## Confidence intervals via summary(lm()) function
lm_coef <- as.data.frame(confint(lm.final1))

# Compare two CIs
comparison <- cbind(as.data.frame(summary(lm.final1)$coefficient[, 1]), lm_coef, boot_coef)
comparison <- round(comparison, 5)

### Practical and statistical Significance

babies_smoke <- clean_babies %>%
  filter(smoke != 9) %>%
  group_by(smoke) %>%
  summarise(bbwt_mean = mean(baby_wt, na.rm = TRUE))

### Reference
# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r
# http://www.sthda.com/english/articles/38-regression-model-validation/156-bootstrap-resampling-essentials-in-r
# https://www.statmethods.net/advstats/bootstrapping.html