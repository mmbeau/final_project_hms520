##############################################################################
#------------------------Final Project---------------------------------------#
#-----------------------Meera Beauchamp--------------------------------------#
#----------------Purpose: functions used in final project scripts------------#
##############################################################################

#Function to create scatter plots of bmi and possible predictors
scatter_plots_func <- function(xvar){
  plot_scat<- ggplot(teen, aes_(x=as.name(xvar) ,y=teen$bmi_p)) +
    labs(title = paste0('BMI by ', xvar)) +
    ylab('bmi') +
    geom_point() 
  # Need to use print() to actually produce the plot
  print(plot_scat)
  # Save to PDF
  ggsave(sprintf("graphs/%s.pdf", xvar))
}

#Function to plot the residuals of model
plot_summary_func<- function(x, model_name) {
  print(summary(x))
  plot(fitted(x), resid(x), main=paste(model_name, ": Residual plot"),  ylab="Residual") %>% 
    abline(0,0)
}

#Function to calculate RMSE of model and print
get_rsme <- function(y_pred, na_rm = FALSE) {
  outsample_rsme <- with(
    filter(teen, test == 1),
    sqrt(mean((bmi_p - get(y_pred))^2, na.rm = na_rm))
  )
  insample_rsme <- with(
    filter(teen, test == 0),
    sqrt(mean((bmi_p - get(y_pred))^2, na.rm = na_rm))
  )
  print(paste(y_pred,"outsample rsme", outsample_rsme))# 
  print(paste(y_pred,"insample rsme", insample_rsme))# 
}


