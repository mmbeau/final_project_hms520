###########################################################################################################################
#-----------------------------------Final Project-------------------------------------------------------------------------#
#-----------------------------------Meera Beauchamp-----------------------------------------------------------------------#
#-----Step 3: Model Creation and evaluation: In the last script, statistically significant variables were identified -----#
#------------This script explores different models using those variables and produces metrics to evaluate model fit-------#
###########################################################################################################################
rm(list = ls())
#set working directory
setwd("~/Documents/Classes/hms520/final_project_hms520")
#Load packages and functions
library(dplyr)
library(readr)
source("functions.R")
#read in data
teen <- read_csv("teen.csv",show_col_types = FALSE)

#---------------------------------------------------------------------------------------
#Create models--------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#Statistically sig variables: age, poverty_lvl, hrs_sleep, num_fruit_veg
#Test multivariate lms adding variables by lowest p.val from previous step
#Look at lm summary and residual plot, if R^2 increases add another variable
m1<-lm(bmi_p ~ age + poverty_lvl , data = filter(teen, test==0)) 
plot_summary_func(m1, "Model 1")

m2<-lm(bmi_p ~ age + poverty_lvl + hrs_sleep , data = filter(teen, test==0)) 
plot_summary_func(m2, "Model 2")

m3<-lm(bmi_p ~ age + poverty_lvl + hrs_sleep + num_fruit_veg, data = filter(teen, test==0)) 
plot_summary_func(m3, "Model 3")

#---------------------------------------------------------------------------------------
#Create columns for the models predicted bmi and absolute difference from actual bmi
#---------------------------------------------------------------------------------------
teen <- teen %>%
  mutate(bmi_m1_pred = predict(m1, teen),
         bmi_m2_pred = predict(m2, teen),
         bmi_m3_pred = predict(m3, teen),
         m1_diff = abs(teen$bmi_p-bmi_m1_pred),
         m2_diff = abs(teen$bmi_p-bmi_m2_pred),
         m3_diff = abs(teen$bmi_p-bmi_m2_pred))

#---------------------------------------------------------------------------------------
# summarize fit ------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
get_rsme(teen$bmi_p, teen$bmi_m1_pred, 'm1')
get_rsme(teen$bmi_p, teen$bmi_m2_pred, 'm2')
get_rsme(teen$bmi_p, teen$bmi_m3_pred, 'm3')
#All models have similar results, model 3 slightly better than the rest

#save out data
write_csv(teen, "teen_predictions.csv")





