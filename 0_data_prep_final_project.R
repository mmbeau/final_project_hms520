#######################################
#--------Final Project----------------#
#-------Meera Beauchamp---------------#
#######################################
#set working directory
setwd("~/Documents/Classes/hms520")
#Load packages
library(haven)
library(dplyr)
library(ggplot2)

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
teen_col<-c( 'tc42b',  'srsex',  
            'slph', 'povll',
            'bmi_p', 'srage_p',  'te4_p1', 'te6_p1')

#Possible dependent variables
y<-c("bmi_p" )#'BODY MASS INDEX (PUF RECODE)')


#Subset df to columns of interest and replace spaces with underscore
teen<-subset(teen, select = teen_col)

#Create training and testing split
seed <- 104L
set.seed(seed)

n <- nrow(teen)
#Set 10% of data to test
n_test <- as.integer(0.1 * n) 

i_test <- sample.int(n, size = n_test)
teen$test <- 0
teen[i_test, "test"] <- 1

#save out data
write_csv(teen, "iris.csv")


#####################Create new script here
#Variable creation
teen$num_fruit_veg<-teen$te4_p1+ teen$te6_p1

#Drop columns
teen<-subset(teen, select = -c(td27_p1, td30_p1, te4_p1, te6_p1))

#rename columns to something more readable
names(teen)[names(teen) == "povll"] <- "poverty_lvl" #poverty level
names(teen)[names(teen) == "slph"] <- "hrs_sleep" #HOURS SLEPT ON WEEKNIGHTS
names(teen)[names(teen) == "srage_p"] <- "age"

ggplot(teen, aes(x=bmi_p, y=time_sitting_activity)) + 
  geom_point()


vars_to_try<-c( "srsex", "poverty_lvl",
               "fruit_veg_5","hrs_sleep","age",
               "num_fruit_veg")

scatter_plots_func <- function(xvar){
  plot_scat<- ggplot(teen, aes_(x=as.name(xvar) ,y=teen$bmi_p)) +
    geom_point() 
  # Need to use print() to actually produce the plot
  print(plot_scat)
  # Save to PDF
  ggsave(sprintf("%s.pdf", xvar))
}
#xvar<-"park_win_30min"
lapply(vars_to_try, scatter_plots_func)
ggplot(teen, aes_(x=teen$bmi_p, y=xvar, color = sex, shape= age)) +
  geom_point() 
ggsave("xvars.pdf")

##################Create new script here
vars_to_try<-c( "srsex", "poverty_lvl",
                "fruit_veg_5","hrs_sleep","age",
                "num_fruit_veg")

lm_results <- lapply(vars_to_try, function(col){
  lm_formula <- as.formula(paste0("bmi_p ~ ", col))
  lm(lm_formula, data = teen)
})
#Look at just R^2
lapply(lm_results, function(x) summary(x)$r.squared)
#Look at full summary
lapply(lm_results, function(x) summary(x))
#Look at summary and don't include the ones that aren't stat sig
#Variables not stat sig = srsex, fruit_veg_5
#Remove variables that aren't stat sig from vector
vars_to_try<-vars_to_try[! vars_to_try %in% c('srsex', 'fruit_veg_5')]

#Test multivariate lms adding variables by lowest p.val
#Look at lm summary and residual plot
#If Rsquared increases add another variable
m1<-lm(bmi_p ~ age + poverty_lvl , data = filter(teen, test==0)) 
plot_summary_func(m1)

m2<-lm(bmi_p ~ age + poverty_lvl + hrs_sleep , data = filter(teen, test==0)) 
plot_summary_func(m1)

m3<-lm(bmi_p ~ age + poverty_lvl + hrs_sleep + num_fruit_veg, data = filter(teen, test==0)) 
plot_summary_func(m1)

#Use the models to predict bmi and absolute difference from actual
teen <- teen %>%
  mutate(bmi_m1_pred = predict(m1, teen),
         bmi_m2_pred = predict(m2, teen),
         bmi_m3_pred = predict(m3, teen),
         m1_diff = abs(teen$bmi_p-bmi_m1_pred),
         m2_diff = abs(teen$bmi_p-bmi_m2_pred),
         m3_diff = abs(teen$bmi_p-bmi_m2_pred))


# summarize fit -----------------------------------------------------------
##Put in functions:
get_rsme <- function(y, y_pred, mod, na_rm = FALSE) {
  outsample_rsme <- with(
    filter(teen, test == 1),
    sqrt(mean((y - y_pred)^2, na.rm = na_rm))
  )
  insample_rsme <- with(
    filter(teen, test == 0),
    sqrt(mean((y - y_pred)^2, na.rm = na_rm))
  )
  print(paste(mod, "outsample rsme", outsample_rsme))
  print(paste(mod, "insample rsme", insample_rsme))
}


get_rsme(teen$bmi_p, teen$bmi_m1_pred, 'm1')
get_rsme(teen$bmi_p, teen$bmi_m2_pred, 'm2')
get_rsme(teen$bmi_p, teen$bmi_m3_pred, 'm3')
#All models have similar results, model 3 slightly better than the rest


#######Put this is function script!
plot_summary_func<- function(x) {
  print(summary(x))
  plot(fitted(x), resid(x)) %>% abline(0,0)
}


