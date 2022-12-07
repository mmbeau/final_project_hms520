#######################################
#--------Final Project----------------#
#-------Meera Beauchamp---------------#
#######################################
#set working directory
setwd("~/Documents/Classes/hms520")
#Load packages
library(haven)
library(sjlabelled)
library(dplyr)
library(ggplot2)

teen_2017 <- read_dta("teen_2017_stata/TEEN.dta")
teen_2018 <- read_dta("teen_2018_stata/TEEN.dta")

#colnames(teen_2017)<-colnames(label_to_colnames(teen_2017))
#colnames(teen_2018)<-colnames(label_to_colnames(teen_2018))

list_17<-colnames(teen_2017)
list_18<-colnames(teen_2018)

#find common columns between datasets
common_cols <- intersect(colnames(teen_2017), colnames(teen_2018))
#Concat datasets
teen<-rbind(
  subset(teen_2017, select = common_cols), 
  subset(teen_2018, select = common_cols)
)

#Create list of columns of interest
teen_col<-c( 'tc42b',  'srsex',  
            'povll','slph',  'ahedtc_p1', 
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
names(teen)[names(teen) == "ahedtc_p1"] <- "adult_edu"#ADULT EDUCATIONAL ATTAINMENT (PUF 1 YR RECODE)
names(teen)[names(teen) == "srage_p"] <- "age"

ggplot(teen, aes(x=bmi_p, y=time_sitting_activity)) + 
  geom_point()


vars_to_try<-c( "srsex", "poverty_lvl",
               "fruit_veg_5","hrs_sleep","adult_edu" ,"age",
               "num_fruit_veg")

scatter_plots_func <- function(xvar){
  plot_scat<- ggplot(teen, aes_(x=as.name(xvar) ,y=teen$bmi_p)) +
    geom_point() #color by poverty or edu attain, shape by sex?
  # Need to use print() to actually produce the plot
  print(plot_scat)
  
  # Save to PDF
  ggsave(sprintf("%s.pdf", xvar))
}
#xvar<-"park_win_30min"
lapply(vars_to_try, scatter_plots_func)
ggplot(teen, aes_(x=teen$bmi_p, y=xvar)) +
  geom_point() 
ggsave("xvars.pdf")

##################Create new script here

m1<-lm(bmi_p ~ srsex, data = filter(teen, test==0)) #not stat sig
coef(m1)
vcov(m1)
summary(m1)

m3<-lm(bmi_p ~ poverty_lvl, data = filter(teen, test==0)) #stat sig and neg correlation
#coef(m1)
#vcov(m1)
summary(m3)

m4<-lm(bmi_p ~ fruit_veg_5, data = filter(teen, test==0)) #not stat sig
summary(m4)

m5<-lm(bmi_p ~ hrs_sleep, data = filter(teen, test==0))#stat sig + negcorrelation
# coef(m1)
# vcov(m1)
summary(m5)

m6<-lm(bmi_p ~ adult_edu, data = filter(teen, test==0))#stat sig + negcorrelation
summary(m6)

m7<-lm(bmi_p ~ age, data = filter(teen, test==0))#stat sig + pos correlation
summary(m7)

m8<-lm(bmi_p ~ num_fruit_veg, data = filter(teen, test==0)) #stat sig + negcorrelation
summary(m8)


