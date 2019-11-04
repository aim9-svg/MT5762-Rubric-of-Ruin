install.packages("caret")
install.packages("boot")
library(caret)
library(boot)
library(tidyr)
library(dplyr)
library(bootstrap)

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

# Validation MSE values
MSE1 = matrix(data=rep(0), nrow = 1, ncol = 2)
colnames(MSE1) = c('final1', 'final2')

#choose lm.final1 and lm.final2 models
#final1
lm.final1.response = test_data %>%
  dplyr::select(baby_wt)
lm.final1.predictor = test_data %>%
  dplyr::select(gestation,m_race,m_ht,m_wt,d_wt, smoke,previous_preg)

pred_response = predict(lm.final1, newdata = lm.final1.predictor, se = T)
lm.final1.response$pred = pred_response$fit

#adding data into the datable
MSE1[1,1] = mean((lm.final1.response$baby_wt - lm.final1.response$pred)^2)

#final2
lm.final2.response = test_data %>% dplyr::select(baby_wt)
lm.final2.predictor = test_data %>% dplyr::select(gestation, m_race, m_ht, d_wt, smoke,
                                                    previous_preg)
pred_response_final2 = predict(lm.final2, newdata = lm.final2.predictor, se = T)
lm.final2.response$pred = pred_response_final2$fit
#adding data into the datable
MSE1[1,2] = mean((lm.final2.response$baby_wt - lm.final2.response$pred)^2)


#5-fold cross validation
MSE = matrix(0, nrow = 2, ncol = 2)
colnames(MSE) = c('final1', "final2")
rownames(MSE) = c('5-fold MSE', 'validation dataset MSE')

MSE[2, ] = MSE1[1, ]

#creating k-fold validation function
k_fold <- function(fit, k = 5, x, y){
  set.seed(123)
  fit_model <- function(x, y){lsfit(x, y)}
  fit_predict <- function(fit, x){cbind(1, x) %*% 
      as.matrix(fit$coefficients)}
  results <- crossval(x, y, fit_model, fit_predict, ngroup = k)
  
  #create matrix to put the response values in
  outcome <- matrix(rep(0), nrow = nrow(x), ncol = 2)
  colnames(outcome) <- c('observed', 'predicted')
  outcome[ , 1] <- y
  
  #Fit of cross-validation fo individual observations
  outcome[ , 2] <- results$cv.fit
  
  #Determine MSE
  MSE <- mean((outcome[ , 1] - outcome[ , 2])^2)
  cat(k, "-fold Cross-Validated MSE = ", MSE, "\n")
  return(MSE)
}

# lm.final1 MODEL
#redoing data modification to keep factor values for the spreading later on
babies_validation <- readxl::read_xlsx("Original Data.xlsx")
clean_babies_validation <- babies_validation %>%
  dplyr::select(-id, -pluralty, -outcome, -date, -sex)

#creating data to test lm.final1
x_final1 <- clean_babies_validation %>% dplyr::select(gestation, smoke, ht, drace, parity, dht)
y_final1 <- clean_babies_validation %>% dplyr::select(wt...7)
#insert new index column to avoid duplicates in order to spread the data
x_final1$index <- seq.int(nrow(x_final1))
x_final1 <- spread(x_final1, key = smoke, value = smoke, sep = '')
x_final1 <- spread(x_final1, key = drace, value = drace, sep = '')
x_final1 <- spread(x_final1, key = parity, value = parity, sep = '')
x_final1 <- x_final1 %>% dplyr::select(gestation, smoke1, smoke2, smoke3,ht, drace1, drace2, drace3, 
                                drace4, drace5, drace6, drace7, drace8, drace9,
                                parity1, parity10, parity11, parity2, parity3, 
                                parity4, parity5, parity6, parity7, parity9, dht)
x_final1[is.na(x_final1)] <- 0
for(i in 1:481){
  if(x_final1$smoke2[i] == 2) x_final1$smoke2[i] <- 1
  if(x_final1$smoke3[i] == 3) x_final1$smoke3[i] <- 1
  if(x_final1$drace2[i] == 2) x_final1$drace2[i] <- 1
  if(x_final1$drace3[i] == 3) x_final1$drace3[i] <- 1
  if(x_final1$drace4[i] == 4) x_final1$drace4[i] <- 1
  if(x_final1$drace5[i] == 5) x_final1$drace5[i] <- 1
  if(x_final1$drace6[i] == 6) x_final1$drace6[i] <- 1
  if(x_final1$drace7[i] == 7) x_final1$drace7[i] <- 1
  if(x_final1$drace8[i] == 8) x_final1$drace8[i] <- 1
  if(x_final1$drace9[i] == 9) x_final1$drace9[i] <- 1
  if(x_final1$parity10[i] == 10) x_final1$parity10[i] <- 1
  if(x_final1$parity11[i] == 11) x_final1$parity11[i] <- 1
  if(x_final1$parity2[i] == 2) x_final1$parity2[i] <- 1
  if(x_final1$parity3[i] == 3) x_final1$parity3[i] <- 1
  if(x_final1$parity4[i] == 4) x_final1$parity4[i] <- 1
  if(x_final1$parity5[i] == 5) x_final1$parity5[i] <- 1
  if(x_final1$parity6[i] == 6) x_final1$parity6[i] <- 1
  if(x_final1$parity7[i] == 7) x_final1$parity7[i] <- 1
  if(x_final1$parity9[i] == 9) x_final1$parity9[i] <- 1
}
str(x_final1)

#variables are put in a matrix
x_final1 <- as.matrix(x_final1)
y_final1 <- as.matrix(y_final1)
#calculate the 5-fold value of the updated final1 model
MSE[1, 1] <- k_fold(lm.final1, k=5, x = x_final1, y = y_final1)  

# lm.final2 MODEL
# spread the model data to fit the matrix calculation
x_final2 <- clean_babies_validation %>% dplyr::select(gestation, parity, ht, drace, dht, time)
y_final2 <- clean_babies_validation %>% dplyr::select(wt...7)
#insert new index column to avoid duplicates in order to spread the data
x_final2$index <- seq.int(nrow(x_final2))
x_final2 <- x_final2 %>% distinct()
x_final2 <- spread(x_final2, key = parity, value = parity, sep = '')
x_final2 <- spread(x_final2, key = drace, value = drace, sep = '')
x_final2 <- spread(x_final2, key = time, value = time, sep = '')
x_final2 <- x_final2 %>% dplyr::select(gestation, parity1, parity10, parity11, parity2,
                                  parity3, parity4, parity5, parity6, parity7, parity9, ht,
                                  drace1, drace2, drace3, drace4, drace5, drace6, drace7, 
                                  drace8, drace9, dht, time1, time2, time3, time4, 
                                  time5, time6, time7, time8, time9)
x_final2[is.na(x_final2)] <- 0

for(i in 1:471){
  if(x_final2$parity10[i] == 10) x_final2$parity10[i] <- 1
  if(x_final2$parity11[i] == 11) x_final2$parity11[i] <- 1
  if(x_final2$parity2[i] == 2) x_final2$parity2[i] <- 1
  if(x_final2$parity3[i] == 3) x_final2$parity3[i] <- 1
  if(x_final2$parity4[i] == 4) x_final2$parity4[i] <- 1
  if(x_final2$parity5[i] == 5) x_final2$parity5[i] <- 1
  if(x_final2$parity6[i] == 6) x_final2$parity6[i] <- 1
  if(x_final2$parity7[i] == 7) x_final2$parity7[i] <- 1
  if(x_final2$parity9[i] == 9) x_final2$parity9[i] <- 1
  if(x_final2$drace2[i] == 2) x_final2$drace2[i] <- 1
  if(x_final2$drace3[i] == 3) x_final2$drace3[i] <- 1
  if(x_final2$drace4[i] == 4) x_final2$drace4[i] <- 1
  if(x_final2$drace5[i] == 5) x_final2$drace5[i] <- 1
  if(x_final2$drace6[i] == 6) x_final2$drace6[i] <- 1
  if(x_final2$drace7[i] == 7) x_final2$drace7[i] <- 1
  if(x_final2$drace8[i] == 8) x_final2$drace8[i] <- 1
  if(x_final2$drace9[i] == 9) x_final2$drace9[i] <- 1
  if(x_final2$time2[i] == 2) x_final2$time2[i] <- 1
  if(x_final2$time3[i] == 3) x_final2$time3[i] <- 1
  if(x_final2$time4[i] == 4) x_final2$time4[i] <- 1
  if(x_final2$time5[i] == 5) x_final2$time5[i] <- 1
  if(x_final2$time6[i] == 6) x_final2$time6[i] <- 1
  if(x_final2$time7[i] == 7) x_final2$time7[i] <- 1
  if(x_final2$time8[i] == 8) x_final2$time8[i] <- 1
  if(x_final2$time9[i] == 9) x_final2$time9[i] <- 1
}
str(x_final2)

#variables are put in a matrix
x_final2 <- as.matrix(x_final2)
y_final2 <- as.matrix(y_final2)
#calculate the 5-fold value of the updated final2 model
MSE[1, 2] <- k_fold(lm.final2, k=5, x = x_final2, y = y_final2)


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