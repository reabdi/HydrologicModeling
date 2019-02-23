monthly_avg_predicted <- function(number_of_days_table){
  monthly_averages_predicted <- array(0,length(number_of_days_table))
  j <- 0
  for(i in seq(1,length(monthly_averages_predicted))){
    monthly_averages_predicted[i] <- mean(discharge_predicted_cfs[(j+1):(j+number_of_days_table[i])])
    j <- j + number_of_days_table[i]
  }

  month_numbers_predicted <- matrix(0,12,length(year_type))
  for (i in seq(1,12)){
    month_numbers_predicted[i,] <- seq(i,length(monthly_averages_predicted),12)
    i <- i + 1
  }

  month_averages_2_predicted <- matrix(0,12,length(year_type))
  for (i in seq(1,12)){
    month_averages_2_predicted[i,] <- monthly_averages_predicted[month_numbers_predicted[i,]]
    i <- i + 1
  }

  total_avg_predicted <- array(0,12)
  for (i in seq(1,12)){
    total_avg_predicted[i] <- mean(month_averages_2_predicted[i,])
  }
  total_avg_predicted_round <- round(total_avg_predicted)
  required_results_monthly_flow <- list(total_avg_predicted_round,monthly_averages_predicted)
  return(required_results_monthly_flow)
}