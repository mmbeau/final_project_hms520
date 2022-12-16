##################################################################################################
#-----------------------------Final Project------------------------------------------------------#
#----------------------------Meera Beauchamp-----------------------------------------------------#
#-----Step 1: Variable Exploration: Explore correlation between possible predictors visually-----#
#-------------and statistical significance through uni-variate regressions-----------------------#
##################################################################################################
rm(list = ls())
#set working directory
setwd("~/Documents/Classes/hms520/final_project_hms520")
#Load packages and functions
library(dplyr)
library(ggplot2)
source("functions.R")
#---------------------------------------------------------------------------------------
#Read in data, create vector with variables of interest---------------------------------
#---------------------------------------------------------------------------------------
teen <- read_csv("teen.csv",show_col_types = FALSE)

vars_to_try<-c( "srsex", "poverty_lvl",
                "fruit_veg_5","hrs_sleep","age",
                "num_fruit_veg")

#---------------------------------------------------------------------------------------
#Create scatter plots of variables of interest and bmi, our dependent variable----------
#---------------------------------------------------------------------------------------
#Use lapply to loop through variables of interest and create scatter plots
lapply(vars_to_try, scatter_plots_func)

#---------------------------------------------------------------------------------------
#Explore variable relationship further and look for statistical significance -----------
#through uni-variate regressions--------------------------------------------------------
#---------------------------------------------------------------------------------------
#Create univariate linear models
lm_results <- lapply(vars_to_try, function(col){
  lm_formula <- as.formula(paste0("bmi_p ~ ", col))
  lm(lm_formula, data = teen)
})

#Look at full summary
lapply(lm_results, function(x) summary(x))

#Don't include the variables that aren't statistically significant
#Variables not statistically significant = srsex, fruit_veg_5
#Remove variables that aren't stat sig from vector
vars_to_try<-vars_to_try[! vars_to_try %in% c('srsex', 'fruit_veg_5')]

#save out data
write_csv(teen, "teen1.csv")



