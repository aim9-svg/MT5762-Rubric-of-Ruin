### MT5762 Assignment 2 - Rubric of Ruin

## Install/library required packages
install.packages("tidyverse")
install.packages("randomForest")
install.packages("mice")
install.packages("ggplot2")
install.packages("Rmisc")
library(tidyverse)
library(dplyr)
library(mice)
library(randomForest)
library(ggplot2)
library(Rmisc)


## Import data 
babies <- readxl::read_xlsx("Original Data.xlsx")

## Clean data
# delete some variables which are independent of the response variable baby_wt.
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
clean_babies$marital[clean_babies$marital == 0] = NA

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

##--------------first method: fill the missing value-----------------------------##
# Mice(Multivariate Imputation by Chained Equations) method first uses mice function modeling and then USES complete function to generate complete data.The mice(df) operation returns multiple complete copies of the df, each with a different value for the missing data.The complete() function returns one (default) or more of these data sets.

# Fill in the missing value with mice interpolation based on random forest model
#miceMod <- mice(clean_babies[, !names(clean_babies) %in% "baby"], method="rf") 
# Generate complete data
#babies_data <- complete(miceMod)  
# Check for missing values in the dataset
#anyNA(babies_data)
##--------------------------------------------------------------------------------##

# mice method of filling in missing values is the most reasonable one we can find, 
# but there is still a chance of filling in the wrong values, 
# so we decided to delete the missing values.


##-------------second method: delete the missing value----------------------------##
# If the data set to be regressed has a large number of observations enough to be used to build the model, 
# it is a reasonable method to delete the observations containing missing values, 
# as long as enough sample points can be used for modeling after the observations are deleted.

babies_data <- na.omit(clean_babies)
# Check for missing values in the dataset
anyNA(babies_data)

##--------------------------------------------------------------------------------##


## Redefine some factor variables(divided by group) to creat dummy variables
# mother's race
babies_data$m_race[babies_data$m_race < 6] = "white"
babies_data$m_race[babies_data$m_race == 6] = "mex"
babies_data$m_race[babies_data$m_race == 7] = "black"
babies_data$m_race[babies_data$m_race == 8] = "asian"
babies_data$m_race[babies_data$m_race == 9] = "mixed"
# father's race
babies_data$d_race[babies_data$d_race < 6] = "white"
babies_data$d_race[babies_data$d_race == 6] = "mex"
babies_data$d_race[babies_data$d_race == 7] = "black"
babies_data$d_race[babies_data$d_race == 8] = "asian"
babies_data$d_race[babies_data$d_race == 9] = "mixed"

# mother's education
babies_data$m_edu[babies_data$m_edu == 0] = "less than 8th"
babies_data$m_edu[babies_data$m_edu == 1] = "8th-12th"
babies_data$m_edu[babies_data$m_edu == 2] = "HS"
babies_data$m_edu[babies_data$m_edu == 3] = "HS+trade"
babies_data$m_edu[babies_data$m_edu == 4] = "HS+college"
babies_data$m_edu[babies_data$m_edu == 5] = "College graduate"
# father's education
babies_data$d_edu[babies_data$d_edu == 0] = "less than 8th"
babies_data$d_edu[babies_data$d_edu == 1] = "8th-12th"
babies_data$d_edu[babies_data$d_edu == 2] = "HS"
babies_data$d_edu[babies_data$d_edu == 3] = "HS+trade"
babies_data$d_edu[babies_data$d_edu == 4] = "HS+college"
babies_data$d_edu[babies_data$d_edu == 5] = "College graduate"

# income 
babies_data$income[babies_data$income == 0] = "$0-$2499"
babies_data$income[babies_data$income == 1] = "$2500-$4999"
babies_data$income[babies_data$income == 2] = "$4999-$7499"
babies_data$income[babies_data$income == 3] = "$7500-$9999"
babies_data$income[babies_data$income == 4] = "$10000-$12499"
babies_data$income[babies_data$income == 5] = "$12500-$14999"
babies_data$income[babies_data$income == 6] = "$15000-$17499"
babies_data$income[babies_data$income == 7] = "$17500-$19999"
babies_data$income[babies_data$income == 8] = "$20000-$22499"
babies_data$income[babies_data$income == 9] = "$22500+"

# marital
babies_data$marital[babies_data$marital == 1] = "married"
babies_data$marital[babies_data$marital == 2] = "legally separated"
babies_data$marital[babies_data$marital == 3] = "divorced"
babies_data$marital[babies_data$marital == 4] = "widowed"
babies_data$marital[babies_data$marital == 5] = "never married"

# smoke
babies_data$smoke[babies_data$smoke == 0] = "never"
babies_data$smoke[babies_data$smoke == 1] = "smokes now"
babies_data$smoke[babies_data$smoke == 2] = "until current pregnancy"
babies_data$smoke[babies_data$smoke == 3] = "once did not now"

# smoke_time (If mother quit, how long ago?)
babies_data$smoke_time[babies_data$smoke_time == 0] = "never smoked"
babies_data$smoke_time[babies_data$smoke_time == 1] = "still smokes"
babies_data$smoke_time[babies_data$smoke_time == 2] = "during preg"
babies_data$smoke_time[babies_data$smoke_time == 3] = "within 1 yr"
babies_data$smoke_time[babies_data$smoke_time == 4] = "1 to 2 years"
babies_data$smoke_time[babies_data$smoke_time == 5] = "2 to 3 yr"
babies_data$smoke_time[babies_data$smoke_time == 6] = "3 to 4 yrs"
babies_data$smoke_time[babies_data$smoke_time == 7] = "5 to 9yrs"
babies_data$smoke_time[babies_data$smoke_time == 8] = "10+yrs"

# cig_number
babies_data$cig_number[babies_data$cig_number == 0] = "0"
babies_data$cig_number[babies_data$cig_number == 1] = "1-4"
babies_data$cig_number[babies_data$cig_number == 2] = "5-9"
babies_data$cig_number[babies_data$cig_number == 3] = "10-14"
babies_data$cig_number[babies_data$cig_number == 4] = "15-19"
babies_data$cig_number[babies_data$cig_number == 5] = "20-29"
babies_data$cig_number[babies_data$cig_number == 6] = "30-39"
babies_data$cig_number[babies_data$cig_number == 7] = "40-60"
babies_data$cig_number[babies_data$cig_number == 8] = "60+"


##  Explore relationships between each predictor variable and the response variable.
# relationship: baby weight and gestation
p_wt_ges <- ggplot(babies_data,aes(gestation,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Gestation")+ylab("Baby Weight")+ggtitle("Relationship between gestation and baby weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_gestation <-cor.test(babies_data$gestation,babies_data$baby_wt)

# relationship: baby weight and m_race
#p_wt_mrace <- ggplot(babies_data,aes(m_race,baby_wt))+
  #geom_point()+
  #geom_smooth(method = "lm")
p_wt_mrace <- ggplot(babies_data,aes(m_race,baby_wt, fill = m_race))+
  geom_boxplot(alpha = 0.7)+
  xlab("Mother's race")+ylab("Baby weight")+ggtitle("Relationship between mother's race and Baby weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))+
  scale_fill_discrete(name="Mother's race")

# relationship: baby weight and m_age
p_wt_mage <- ggplot(babies_data,aes(m_age,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Mother's age")+ylab("Baby Weight")+ggtitle("Relationship between mother's age and baby weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_m_age <-cor.test(babies_data$m_age,babies_data$baby_wt) 

# relationship: baby weight and m_edu
p_wt_medu <- ggplot(babies_data,aes(m_edu,baby_wt, fill = m_edu))+
  geom_boxplot(alpha = 0.7)+
  xlab("Mother's education")+ylab("Baby weight")+ggtitle("Relationship between mother's education and Baby weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 7))+
  scale_fill_discrete(name="Mother's education")

# relationship: baby weight and m_ht
p_wt_mht <- ggplot(babies_data,aes(m_ht,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Mother's height")+ylab("Baby weight")+ggtitle("Relationship between mother's height and Baby weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_m_ht <-cor.test(babies_data$m_ht,babies_data$baby_wt) 

# relationship: baby weight and m_wt
p_wt_mwt <- ggplot(babies_data,aes(m_wt,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Mother's weight")+ylab("Baby weight")+ggtitle("Relationship between mother's weight and baby weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_m_wt <-cor.test(babies_data$m_wt,babies_data$baby_wt) 

# relationship: baby weight and d_race
p_wt_drace <- ggplot(babies_data,aes(d_race,baby_wt, fill = d_race))+
  geom_boxplot(alpha = 0.7)+
  xlab("Father's race")+ylab("Baby weight")+ggtitle("Relationship between father's race and baby weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))+
  scale_fill_discrete(name="Father's race")

# relationship: baby weight and d_age
p_wt_dage <- ggplot(babies_data,aes(d_age,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Father's Age")+ylab("Baby Weight")+ggtitle("Relationship between Father's Age and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_d_age <-cor.test(babies_data$d_age,babies_data$baby_wt) 

# relationship: baby weight and d_edu
p_wt_dedu <- ggplot(babies_data,aes(d_edu,baby_wt, fill = d_edu))+
  geom_boxplot(alpha = 0.7)+
  xlab("Father's Education")+ylab("Baby Weight")+ggtitle("Relationship between Father's Education and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 7))+
  scale_fill_discrete(name="Father's education")

# relationship: baby weight and d_ht
p_wt_dht <- ggplot(babies_data,aes(d_ht,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Father's Height")+ylab("Baby Weight")+ggtitle("Relationship between Father's Height and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_d_ht <-cor.test(babies_data$d_ht,babies_data$baby_wt) 

# relationship: baby weight and d_wt
p_wt_dwt <- ggplot(babies_data,aes(d_wt,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Father's Weight")+ylab("Baby Weight")+ggtitle("Relationship between Father's Weight and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_d_wt <-cor.test(babies_data$d_wt,babies_data$baby_wt) 

# relationship: baby weight and income
p_wt_inc <- ggplot(babies_data,aes(income,baby_wt, fill = income))+
  geom_boxplot(alpha = 0.7)+
  xlab("Income")+ylab("Baby Weight")+ggtitle("Relationship between Income and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 5))+
  scale_fill_discrete(name="Income")

# relationship: baby weight and smoke
p_wt_smo <- ggplot(babies_data,aes(smoke,baby_wt, fill = smoke))+
  geom_boxplot(alpha = 0.7)+
  xlab("Smoking Status")+ylab("Baby Weight")+ggtitle("Relationship between Smoking status and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 7))+
  scale_fill_discrete(name="Smoking Status")

# relationship: baby weight and smoke_time
p_wt_smot <- ggplot(babies_data,aes(smoke_time,baby_wt, fill = smoke_time))+
  geom_boxplot(alpha = 0.7)+
  xlab("Quitting Time")+ylab("Baby Weight")+ggtitle("Relationship between Quitting Time and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 6))+
  scale_fill_discrete(name="Quitting Time")

# relationship: baby weight and cig_number
p_wt_cig <- ggplot(babies_data,aes(cig_number, baby_wt, fill = cig_number))+
  geom_boxplot(alpha = 0.7)+
  xlab("Number of cigarettes smoked per day")+ylab("Baby Weight")+ggtitle("Relationship between Number of cigarettes smoked per day and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))+
  scale_fill_discrete(name="Number")

# relationship: baby weight and previous_preg
p_wt_preg <- ggplot(babies_data,aes(previous_preg,baby_wt))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Previous pregnancies")+ylab("Baby Weight")+ggtitle("Relationship between Previous pregnancies and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 11))
# Calculate the correlation coefficient and test whether the correlation is significant
bbwt_previous_preg <-cor.test(babies_data$previous_preg,babies_data$baby_wt) 

# relationship: baby weight and marital
p_wt_marital <- ggplot(babies_data,aes(marital,baby_wt, fill = marital))+
  geom_boxplot(alpha = 0.7)+
  xlab("Marital")+ylab("Baby Weight")+ggtitle("Relationship between Marital and Baby Weight")+
  theme_bw()+
  theme(plot.title = element_text(size = 13, face = "bold"),
        text = element_text(size = 12),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(size = 9))+
  scale_fill_discrete(name="Marital")


# Put the plots together(in report)
p_wt_ges
p_wt_inc
p_wt_preg
p_wt_marital
multiplot(p_wt_mrace,p_wt_medu,cols = 2)
multiplot(p_wt_mage,p_wt_mht,p_wt_mwt,cols = 3)
multiplot(p_wt_drace,p_wt_dedu,cols = 2)
multiplot(p_wt_dage,p_wt_dht,p_wt_dwt,cols = 3)
multiplot(p_wt_smo,p_wt_smot,p_wt_cig,cols = 3)










