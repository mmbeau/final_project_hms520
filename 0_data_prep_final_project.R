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

colnames(teen_2017)<-colnames(label_to_colnames(teen_2017))
colnames(teen_2018)<-colnames(label_to_colnames(teen_2018))

list_17<-colnames(teen_2017)
list_18<-colnames(teen_2018)

#find common columns between datasets
common_cols <- intersect(colnames(teen_2017), colnames(teen_2018))
#Concat datasets
teen<-rbind(
  subset(teen_2017, select = common_cols), 
  subset(teen_2018, select = common_cols)
)

df<-head(teen)
write.csv(df,"teen.csv", row.names = FALSE)

#Create list of columns of interest
teen_col<-c('rbmi', 'tc42b',  'srsex', 'td38', 'td39', 
            'povll','fv5day', 'slph', 'ovrwt2', 'ahedtc_p1', 
            'bmi_p', 'srage_p', 'tc25_p1', 'tc28b_p1', 'td27_p1', 'td30_p1', 'te4_p1', 'te6_p1')
teen_cols<-c('AGE AND GENDER SPECIFIC BMI (4 LEVELS)','PARK/PLAYGRND WITHIN 30 MINS WALKING DISTANCE FROM HOME',
             'RULES IN HOME FOR TURNING OFF OR PUTTING AWAY ELECTRONICS','SELF-REPORTED GENDER',
             'TIME DOING SITTING ACTIVITIES (TV, COMP GAMES, TALK. W/ FRIENDS) AFTER SCH.',
             'TIME DOING SITTING ACTIVITIES (TV, COMP GAMES, TALK. W/ FRIENDS) WKNDS.','POVERTY LEVEL',
             'CURRENTLY INSURED','UNINSURED IN PAST 12 MOS','CURRENT ASTHMA','5+ FRUIT/VEGS. A DAY','HOURS SLEPT ON WEEKNIGHTS',
             'OVERWEIGHT OR OBESE (CDC 2010 RECOMMENDATIONS)','ADULT EDUCATIONAL ATTAINMENT (PUF 1 YR RECODE)','BODY MASS INDEX (PUF RECODE)',
             'AGE (PUF RECODE)','NEARBY PARK/PLAYGROUND SAFE DURING DAY (PUF 1 YR RECODE)','# CANS OF SWEET FRUIT/SPORT DRUNK YESTERDAY (PUF 1 YR RECODE)',
             '# DAYS WALK HOME FROM SCHOOL PAST WK (PUF 1 YR RECODE)','# DAYS BIKE/SKATE HOME FROM SCHOOL PAST WK (PUF 1 YR RECODE)',
             '# OF SERVINGS OF FRUIT ATE YESTERDAY (PUF 1 YR RECODE)','# OF SERVINGS OF VEGETABLES ATE YESTERDAY (PUF 1 YR RECODE)')
##
teen_cols<-c('PARK/PLAYGRND WITHIN 30 MINS WALKING DISTANCE FROM HOME',
             'RULES IN HOME FOR TURNING OFF OR PUTTING AWAY ELECTRONICS','SELF-REPORTED GENDER',
             'POVERTY LEVEL',
             'CURRENTLY INSURED','UNINSURED IN PAST 12 MOS','CURRENT ASTHMA','5+ FRUIT/VEGS. A DAY','HOURS SLEPT ON WEEKNIGHTS',
             'ADULT EDUCATIONAL ATTAINMENT (PUF 1 YR RECODE)',
             'AGE (PUF RECODE)','NEARBY PARK/PLAYGROUND SAFE DURING DAY (PUF 1 YR RECODE)','# CANS OF SWEET FRUIT/SPORT DRUNK YESTERDAY (PUF 1 YR RECODE)')
             

#Possible dependent variables
y<-c("rbmi", #'AGE AND GENDER SPECIFIC BMI (4 LEVELS)',
     "ovrwt2" , #'OVERWEIGHT OR OBESE (CDC 2010 RECOMMENDATIONS)',
     "bmi_p" )#'BODY MASS INDEX (PUF RECODE)')


#Subset df to columns of interest and replace spaces with underscore
teen<-subset(teen, select = teen_col)

#Variable creation
teen$time_sitting_activity <- teen$td38+teen$td39
teen$days_active_commute<-teen$td27_p1+teen$td30_p1
teen$num_fruit_veg<-teen$te4_p1+ teen$te6_p1

#Drop columns
teen<-subset(teen, select = -c(td38, td39, td27_p1, td30_p1, te4_p1, te6_p1))

#rename columns to something more readable
names(teen)[names(teen) == "tc42b"] <- "park_win_30min" #PARK/PLAYGRND WITHIN 30 MINS WALKING DISTANCE FROM HOME
names(teen)[names(teen) == "povll"] <- "poverty_lvl" #poverty level
names(teen)[names(teen) == "astcur"] <- "asthma" #current asthma
names(teen)[names(teen) == "fv5day"] <- "fruit_veg_5" #5+ fruit veg per day
names(teen)[names(teen) == "slph"] <- "hrs_sleep" #HOURS SLEPT ON WEEKNIGHTS
names(teen)[names(teen) == "ahedtc_p1"] <- "adult_edu"#ADULT EDUCATIONAL ATTAINMENT (PUF 1 YR RECODE)
names(teen)[names(teen) == "srage_p"] <- "age"
names(teen)[names(teen) == "tc25_p1"] <- "park_near_safe" #NEARBY PARK/PLAYGROUND SAFE DURING DAY (PUF 1 YR RECODE)
names(teen)[names(teen) == "tc28b_p1"] <- "num_sweet_bev"## CANS OF SWEET FRUIT/SPORT DRUNK YESTERDAY (PUF 1 YR RECODE)

ggplot(teen, aes(x=bmi_p, y=time_sitting_activity)) + 
  geom_point()


vars_to_try<-c(park_win_30min, srsex, poverty_lvl)#,
               # "fruit_veg_5","hrs_sleep","adult_edu" ,"age", "park_near_safe",
               # "num_sweet_bev", "time_sitting_activity", "num_fruit_veg",
               # "days_active_commute")

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
ggplot(teen, aes_(x=teen$bmi_p, y=xvar)) +
  geom_point() 
ggsave("xvars.pdf")
