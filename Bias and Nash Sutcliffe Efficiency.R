# #################
# Bias:
# In this function, I am going to calculte Bias. I also calculated the required 
# parameteres in the function instead the main body of the code.

Bias <- function(predicted_and_observed_annual_monthly_flows){
  # at first, the annual average flows is calculating:
  input <- read.csv(file="daily_Data_West_Branch.csv")
  
  date <- as.Date(input[,1], format="%m/%d/%Y")
  
  D <- data.frame(year=as.numeric(format(date,format="%Y")),
                  month=as.numeric(format(date,format="%m")),
                  day=as.numeric(format(date,format="%d")))
  D[,4] <- data.frame(discharge=as.numeric(format(input[,2])))
  
  d2 <- dim(D)
  A <- D[1,1]
  B <- D[d2[1],1]
  C <- c((A+1):B)
  year_type <- ifelse((C %% 4 ==0 & C %% 100 != 0) | C %% 400 == 0, 366, 365)
  
  mean1 <- array(0,length(year_type))
  d <- 0
  for (i in seq(1,length(mean1))){
    mean1[i] <- mean(input[(d+1):(d+year_type[i]),2])
    d <- d + year_type[i]
  }
  # Now we have the annual average flows for each year: mean1
  # The other issue is calculating the annual discharges based on predicted values
  discharge_predicted_annual <- array(0,length(year_type))
  d <- 0
  for (i in seq(1,length(mean1))){
    discharge_predicted_annual[i] <- mean(discharge_predicted[(d+1):(d+year_type[i])])
    d <- d + year_type[i]
  }
  
  # Next step is calculating annual bias using prepared data.
  sum_annual_bias <- 0
  for (i in seq(1, length(mean1))){
    sum_annual_bias <- sum_annual_bias + (discharge_predicted_annual[i] - 
                                          mean1[i])
  }
  bias_annual <- sum_annual_bias / length(mean1)
  
  # After calculating the annual bias, next, the monthly bias would be calculated
  # At first, I need to calculate the monthly discharges based on observerd flows:
  
  date_start <- input[1,1]
  date_start_R <- as.Date(date_start,format="%m/%d/%Y")
  date_end <- input[(d2[1]),1]
  date_end_R <- as.Date(date_end,format="%m/%d/%Y")
  number_of_days <- diff(seq(as.Date(date_start_R), as.Date(date_end_R+1), by = "month"))
  number_of_days_table <- ts(number_of_days, start = c(D[1,1], D[1,2]), freq = 12)
  
  monthly_averages <- array(0,length(number_of_days_table))
  j <- 0
  for(i in seq(1,length(monthly_averages))){
    monthly_averages[i] <- mean(input[(j+1):(j+number_of_days_table[i]),2])
    j <- j + number_of_days_table[i]
  }
  
  # Then, I am going to calculate monthly averages for the predicted data
  
  predicted_flows_for_every_month <- array(0,length(number_of_days_table))
  j <- 0
  for(i in seq(1,length(monthly_averages))){
    predicted_flows_for_every_month[i] <- mean(discharge_predicted[(j+1):(j+number_of_days_table[i])])
    j <- j + number_of_days_table[i]
  }
  
  # And finally the monthly bias would be calculated:
  
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
# In this section, we will calculate annual and monthly averages for observed and
# predicted discharges to use in NSE equation. 

Nash_Sutcliffe_NSE <- function(predicted_and_observed_annual_monthly_flows){
  
  # The average amount of the discharges for all of the records:
  input <- read.csv(file="daily_Data_West_Branch.csv")
  d1=dim(input)
  Average_daily_flow_total_yrs <- (sum(input[,2]))/d1[1]
  
  # The annual average flows:
  
  date <- as.Date(input[,1], format="%m/%d/%Y")
  
  D <- data.frame(year=as.numeric(format(date,format="%Y")),
                  month=as.numeric(format(date,format="%m")),
                  day=as.numeric(format(date,format="%d")))
  D[,4] <- data.frame(discharge=as.numeric(format(input[,2])))
  
  d2 <- dim(D)
  A <- D[1,1]
  B <- D[d2[1],1]
  C <- c((A+1):B)
  year_type <- ifelse((C %% 4 ==0 & C %% 100 != 0) | C %% 400 == 0, 366, 365)
  
  mean1 <- array(0,length(year_type))
  d <- 0
  for (i in seq(1,length(mean1))){
    mean1[i] <- mean(input[(d+1):(d+year_type[i]),2])
    d <- d + year_type[i]
  }
  
  # The other issue is calculating the annual discharges based on predicted values
  discharge_predicted_annual <- array(0,length(year_type))
  d <- 0
  for (i in seq(1,length(mean1))){
    discharge_predicted_annual[i] <- mean(discharge_predicted[(d+1):(d+year_type[i])])
    d <- d + year_type[i]
  }
  
  # Then, we shoud have monthly average based on (1) observed data:
  
  date_start <- input[1,1]
  date_start_R <- as.Date(date_start,format="%m/%d/%Y")
  date_end <- input[(d2[1]),1]
  date_end_R <- as.Date(date_end,format="%m/%d/%Y")
  number_of_days <- diff(seq(as.Date(date_start_R), as.Date(date_end_R+1), by = "month"))
  number_of_days_table <- ts(number_of_days, start = c(D[1,1], D[1,2]), freq = 12)
  
  monthly_averages <- array(0,length(number_of_days_table))
  j <- 0
  for(i in seq(1,length(monthly_averages))){
    monthly_averages[i] <- mean(input[(j+1):(j+number_of_days_table[i]),2])
    j <- j + number_of_days_table[i]
  }
  
  # and (2) based on predicted or modeled data:
  
  predicted_flows_for_every_month <- array(0,length(number_of_days_table))
  j <- 0
  for(i in seq(1,length(monthly_averages))){
    predicted_flows_for_every_month[i] <- mean(discharge_predicted[(j+1):(j+number_of_days_table[i])])
    j <- j + number_of_days_table[i]
  }
  
  # Now, I am going to calculate monthly and annual NSE:
  
  sum_annual_NSE_1 <- 0
  sum_annual_NSE_2 <- 0
  for (i in seq(1, length(mean1))){
    sum_annual_NSE_1 <- sum_annual_NSE_1 + (discharge_predicted_annual[i] - 
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
