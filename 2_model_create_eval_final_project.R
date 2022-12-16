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
teen <- read_csv("teen1.csv",show_col_types = FALSE)

#---------------------------------------------------------------------------------------
#Create models--------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#Statistically sig variables: age, poverty_lvl, hrs_sleep, num_fruit_veg
#Test multivariate lms adding variables by lowest p.val from previous step
#Look at lm summary and residual plot, if R^2 increases add another variable
#Plot the residuals from the model

m1<-lm(bmi_p ~ age + poverty_lvl , data = filter(teen, test==0)) 
plot_summary_func(m1, "Model 1: Age + Poverty lvl")


m2<-lm(bmi_p ~ age + poverty_lvl + hrs_sleep , data = filter(teen, test==0)) 
plot_summary_func(m2, "Model 2: Age + Poverty lvl + Hrs sleep")

m3<-lm(bmi_p ~ age + poverty_lvl + hrs_sleep + num_fruit_veg, data = filter(teen, test==0)) 
plot_summary_func(m3, "Model 3: Age + Poverty lvl + Hrs sleep + # Fruit/Veg")

#Model 0 - trying bc from background knowledge these seem like relevant variables
m0<-lm(bmi_p ~ age + num_fruit_veg , data = filter(teen, test==0)) 
plot_summary_func(m0, "Model 0: Age + # fruit/veg")

#---------------------------------------------------------------------------------------
#Create columns for the models predicted bmi and absolute difference from actual bmi
#---------------------------------------------------------------------------------------
teen <- teen %>%
  mutate(bmi_m1_pred = predict(m1, teen),
         bmi_m2_pred = predict(m2, teen),
         bmi_m3_pred = predict(m3, teen),
         bmi_m0_pred = predict(m0, teen),
         m1_diff = abs(teen$bmi_p-bmi_m1_pred),
         m2_diff = abs(teen$bmi_p-bmi_m2_pred),
         m3_diff = abs(teen$bmi_p-bmi_m2_pred),
         m0_diff = abs(teen$bmi_p-bmi_m0_pred))

#---------------------------------------------------------------------------------------
# summarize fit ------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#Create list of columns with predicted values from models
models<-c('bmi_m0_pred','bmi_m1_pred','bmi_m2_pred','bmi_m3_pred')
#Apply function to calculate RSME of model
lapply(models, get_rsme)

#save out data with predictions
write_csv(teen, "teen_predictions.csv")





