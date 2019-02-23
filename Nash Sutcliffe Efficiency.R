# #################
# Nash Sutcliffe Efficiency - Bias:
Nash_Sutcliffe_Bias <- function(predicted_flows_for_every_month){
  sum_annual_bias <- 0
  for (i in seq(1, length(mean1))){
    sum_annual_bias <- sum_annual_bias + (annual_discharge_predicted_cfs[i] - 
                                          mean1[i])
  }
  bias_annual <- sum_annual_bias / length(mean1)
  
  sum_monthly_bias <- 0
  for (i in seq(1, length(predicted_flows_for_every_month))){
    sum_monthly_bias <- sum_monthly_bias + (predicted_flows_for_every_month[i] - 
                                            monthly_averages[i])  
  }
  bias_monthly <- sum_monthly_bias / length(predicted_flows_for_every_month)
  
  bias_values <- list(bias_annual, bias_monthly)
  return(bias_values)
}

# #################
# Nash Sutcliffe Efficiency - NSE:
Nash_Sutcliffe_NSE <- function(Average_daily_flow_total_yrs){
  sum_annual_NSE_1 <- 0
  sum_annual_NSE_2 <- 0
  for (i in seq(1, length(mean1))){
    sum_annual_NSE_1 <- sum_annual_NSE_1 + (annual_discharge_predicted_cfs[i] - 
                                            mean1[i])^2
    sum_annual_NSE_2 <- sum_annual_NSE_2 + (mean1[i] - Average_daily_flow_total_yrs)^2
  }
  NSE_annual <- 1 - (sum_annual_NSE_1 / sum_annual_NSE_2)
  
  sum_monthly_NSE_1 <- 0
  sum_monthly_NSE_2 <- 0
  for (i in seq(1, length(monthly_averages))){
    sum_monthly_NSE_1 <- sum_monthly_NSE_1 + (predicted_flows_for_every_month[i] 
                                              - monthly_averages[i])^2 
    sum_monthly_NSE_2 <- sum_monthly_NSE_2 + (monthly_averages[i] - Average_daily_flow_total_yrs)^2                                        
  }
  NSE_monthly <- 1 - (sum_monthly_NSE_1 / sum_monthly_NSE_2)
  NSE_values <- list(NSE_annual, NSE_monthly)
  return(NSE_values)
}
