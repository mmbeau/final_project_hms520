##############################################################################
#------------------------Final Project---------------------------------------#
#-----------------------Meera Beauchamp--------------------------------------#
#-----Step 0: Data prep: create train/test split, variable creation etc.-----#
##############################################################################
rm(list = ls())
#set working directory
setwd("~/Documents/Classes/hms520")
#Load packages
library(dplyr)
library(readr)
library(haven)

#Read in data
teen_2017 <- read_dta("teen_2017_stata/TEEN.dta")
teen_2018 <- read_dta("teen_2018_stata/TEEN.dta")

#find common columns between datasets
common_cols <- intersect(colnames(teen_2017), colnames(teen_2018))
#Concat datasets
teen<-rbind(
  subset(teen_2017, select = common_cols), 
  subset(teen_2018, select = common_cols)
)

#Create list of columns of interest
teen_col<-c('srsex',  'slph', 'povll', 'fv5day',
            'bmi_p', 'srage_p',  'te4_p1', 'te6_p1')
#Subset df to columns of interest
teen<-subset(teen, select = teen_col)

#Create training and testing split
seed <- 104L #set seed to be replicable
set.seed(seed)

n <- nrow(teen)
#Set 10% of data to test
n_test <- as.integer(0.1 * n) 
i_test <- sample.int(n, size = n_test)
teen$test <- 0
teen[i_test, "test"] <- 1


#Variable creation
teen$num_fruit_veg<-teen$te4_p1+ teen$te6_p1 #num fruits + num veg consumed a day

#Drop columns
teen<-subset(teen, select = -c(te4_p1, te6_p1))

#rename columns to something more readable
names(teen)[names(teen) == "povll"] <- "poverty_lvl" #poverty level
names(teen)[names(teen) == "slph"] <- "hrs_sleep" #HOURS SLEPT ON WEEKNIGHTS
names(teen)[names(teen) == "fv5day"] <- "fruit_veg_5" #5+ fruit veg per day
names(teen)[names(teen) == "srage_p"] <- "age"

#save out data
write_csv(teen, "final_project_hms520/teen.csv")





