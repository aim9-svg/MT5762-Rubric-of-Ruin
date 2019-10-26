### MT5762 Assignment 2 - Rubric of Ruin

## Install/library required packages
install.packages("tidyverse")
install.packages("randomForest")
install.packages("mice")
install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(mice)
library(randomForest)
library(ggplot2)

## Import data 
babies <- readxl::read_xlsx("Original Data.xlsx")

## Clean data
clean_babies <- babies %>%
  select(-id, -pluralty, -outcome, -date, -sex) %>%
  rename(baby_wt = wt...7, previous_preg = parity, m_race = race, m_age = age,
         m_edu = ed, m_ht = ht, m_wt = wt...13, d_race = drace, d_age = dage, d_edu = ded,
         d_ht = dht, d_wt = dwt, income = inc, smoke_time = time, cig_number = number)

## Handling missing values
# Firstly Convert the missing value in each variable (9/99/999) to NA.
clean_babies$gestation[clean_babies$gestation == 999] = NA
clean_babies$m_race[clean_babies$m_race > 9] = NA
clean_babies$d_race[clean_babies$d_race > 9] = NA
clean_babies$m_age[clean_babies$m_age == 99] = NA
clean_babies$d_age[clean_babies$d_age == 99] = NA
clean_babies$m_edu[clean_babies$m_edu > 5] = NA
clean_babies$d_edu[clean_babies$d_edu > 5] = NA
clean_babies$m_ht[clean_babies$m_ht == 99] = NA
clean_babies$d_ht[clean_babies$d_ht == 99] = NA
clean_babies$m_wt[clean_babies$m_wt == 999] = NA
clean_babies$d_wt[clean_babies$d_wt == 999] = NA
clean_babies$income[clean_babies$income > 9] = NA
clean_babies$smoke[clean_babies$smoke == 9] = NA
clean_babies$smoke_time[clean_babies$smoke_time > 8] = NA
clean_babies$cig_number[clean_babies$cig_number > 8] = NA

# Count the number of NA in each column
na_flag <- apply(is.na(clean_babies), 2, sum)
# Check which columns contain NA and which do not
na_col = na_flag[na_flag > 0] %>% names()
full_col = setdiff(names(clean_babies), na_col)
# Gets all rows that contain NA
na_df = clean_babies[!complete.cases(clean_babies),]
# Gets all rows that do not contain NA
full_df = na.omit(clean_babies)
# Reorder the variables
clean_babies <- clean_babies[,c(na_col,full_col)]
# Fill in the missing value with mice interpolation based on random forest model
miceMod <- mice(clean_babies[, !names(clean_babies) %in% "medv"], method="rf") 
# Generate complete data
babies_data <- complete(miceMod)  
# Check for missing values in the dataset
anyNA(babies_data)



##  Explore data 
# relationship: baby weight and gestation
ggplot(babies_data,aes(gestation,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_gestation <-cor.test(babies_data$gestation,babies_data$baby_wt)

# relationship: baby weight and m_race
ggplot(babies_data,aes(m_race,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_m_race <-cor.test(babies_data$m_race,babies_data$baby_wt) 

# relationship: baby weight and m_age
ggplot(babies_data,aes(m_age,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_m_age <-cor.test(babies_data$m_age,babies_data$baby_wt) 

# relationship: baby weight and m_edu
ggplot(babies_data,aes(m_edu,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_m_edu <-cor.test(babies_data$m_edu,babies_data$baby_wt) 

# relationship: baby weight and m_ht
ggplot(babies_data,aes(m_ht,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_m_ht <-cor.test(babies_data$m_ht,babies_data$baby_wt) 

# relationship: baby weight and m_wt
ggplot(babies_data,aes(m_wt,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_m_wt <-cor.test(babies_data$m_wt,babies_data$baby_wt) 

# relationship: baby weight and d_race
ggplot(babies_data,aes(d_race,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_d_race <-cor.test(babies_data$d_race,babies_data$baby_wt) 

# relationship: baby weight and d_age
ggplot(babies_data,aes(d_age,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_d_age <-cor.test(babies_data$d_age,babies_data$baby_wt) 

# relationship: baby weight and d_edu
ggplot(babies_data,aes(d_edu,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_d_edu <-cor.test(babies_data$d_edu,babies_data$baby_wt) 

# relationship: baby weight and d_ht
ggplot(babies_data,aes(d_ht,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_d_ht <-cor.test(babies_data$d_ht,babies_data$baby_wt) 

# relationship: baby weight and d_wt
ggplot(babies_data,aes(d_wt,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_d_wt <-cor.test(babies_data$d_wt,babies_data$baby_wt) 

# relationship: baby weight and income
ggplot(babies_data,aes(income,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_income <- cor.test(babies_data$income,babies_data$baby_wt) 

# relationship: baby weight and smoke
bbwt_smoke <- babies_data %>%
  group_by(smoke) %>%
  summarise(bbwt_mean = mean(baby_wt, na.rm = TRUE))
bbwt_smoke <- kruskal.test(babies_data$baby_wt~babies_data$smoke,data=babies_data)

# relationship: baby weight and smoke_time
bbwt_smoke_time <- babies_data %>%
  group_by(smoke_time) %>%
  summarise(bbwt_mean = mean(baby_wt, na.rm = TRUE))
bbwt_smoke_time <- kruskal.test(babies_data$baby_wt~babies_data$smoke_time,data=babies_data)

# relationship: baby weight and cig_number
ggplot(babies_data,aes(cig_number,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_cig_number <-cor.test(babies_data$cig_number,babies_data$baby_wt) 

# relationship: baby weight and previous_preg
ggplot(babies_data,aes(previous_preg,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_previous_preg <-cor.test(babies_data$previous_preg,babies_data$baby_wt) 

# relationship: baby weight and marital
bbwt_marital <- babies_data %>%
  group_by(marital) %>%
  summarise(bbwt_mean = mean(baby_wt, na.rm = TRUE))
bbwt_marital <- kruskal.test(babies_data$baby_wt~babies_data$marital,data=babies_data)


