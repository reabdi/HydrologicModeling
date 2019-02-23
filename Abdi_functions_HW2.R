
##################################################

flood_100_year <- function(year_index){
  ln_annual_max <- log(annual_max)
  y_average <- mean(ln_annual_max)
  y_sd <- sd(ln_annual_max)
  d6 <- dim(ln_annual_max)
  y_1 <- sum((ln_annual_max[1:d6[1]] - y_average)^3)
  z_p <- qnorm(1-(1/year_index))
  gamma_y <- ((d6[1])*y_1)/((d6[1]-1)*(d6[1]-2)*(y_sd^3))   
  g_Stedinger_Tasker <- (1+(6/d6[1]))*gamma_y
  k_p <- (2/g_Stedinger_Tasker)*((1+(((z_p)*g_Stedinger_Tasker)/6)
          -((g_Stedinger_Tasker^2)/36))^3)-(2/g_Stedinger_Tasker)
  x_p <- exp(y_average+(k_p*y_sd))
  return(x_p)
}

##################################################

lowflow_7Q10_function <- function(year_index2){
  
  x_1 <- min(mean_7Q_min)
  x_n <- max(mean_7Q_min)
  x_median <- median(mean_7Q_min)
  z_p2 <- qnorm(1/year_index2)
  
  if (((x_1)+(x_n))-(2*(x_median)) < 0 ) {
      ln_mean_7Q_min <- log(mean_7Q_min)
      average_7Q_2 <- mean(ln_mean_7Q_min)
      sd_7Q_2 <- sd(ln_mean_7Q_min)
      lowflow_7Q10 <- exp((average_7Q_2)+((z_p2)*(sd_7Q_2)))
  }
  if (((x_1)+(x_n))-(2*(x_median)) >= 0 ) {
       lower_bound <- (((x_1*x_n)-(x_median^2))/((x_1+x_n)-(2*x_median)))
       lower_bound_mean_7Q_min <- mean_7Q_min - lower_bound
       ln_lower_bound_mean_7Q_min <- log(lower_bound_mean_7Q_min)
       average_7Q_3 <- mean(ln_lower_bound_mean_7Q_min)
       sd_7Q_3 <- sd(ln_lower_bound_mean_7Q_min)
       lowflow_7Q10 <- lower_bound + exp((average_7Q_3)+((z_p2)*(sd_7Q_3)))
  }
  return(lowflow_7Q10)
}

##################################################



