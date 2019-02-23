monthly_observed_streamflow <- function(total_avg_monthly){
  input <- read.csv(file="daily_Data_West_Branch.csv")
  
  # Calculating the average daily streamflow across all of the period
  
  d1=dim(input)
  Average_daily_flow_total_yrs <- (sum(input[,2]))/d1[1]
  
  # Calculating the average daily streamflow for each of the water years
  
  date <- as.Date(input[,1], format="%m/%d/%Y")
  
  # creating a data frame for locating the year, month and day separately:
  
  D <- data.frame(year=as.numeric(format(date,format="%Y")),
                  month=as.numeric(format(date,format="%m")),
                  day=as.numeric(format(date,format="%d")))
  D[,4] <- data.frame(discharge=as.numeric(format(input[,2])))
  
  # Here I am trying to distinguish the first and the last year of the period and 
  # determine the leap years.
  
  d2 <- dim(D)
  A <- D[1,1]
  B <- D[d2[1],1]
  C <- c((A+1):B)
  year_type <- ifelse((C %% 4 ==0 & C %% 100 != 0) | C %% 400 == 0, 366, 365)
  
  # I am going to calculate the numbers of days for the months for each year:
  
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
  
  month_numbers <- matrix(0,12,length(year_type))
  for (i in seq(1,12)){
    month_numbers[i,] <- seq(i,length(monthly_averages),12)
    i <- i + 1
  }
  
  month_averages_2 <- matrix(0,12,length(year_type))
  for (i in seq(1,12)){
    month_averages_2[i,] <- monthly_averages[month_numbers[i,]]
    i <- i + 1
  }
  
  total_avg_monthly <- array(0,12)
  for (i in seq(1,12)){
    total_avg_monthly[i] <- mean(month_averages_2[i,])
  }
  
  # For preparing the required data for part e of the assignment, I need to have daily streamflow
  # for 1960 which is a leap year. So, I can wrire:
  
  observed_daily_1960 <- c(input[(6941:7305),2])
  results <- list(total_avg_monthly,observed_daily_1960)
  
  return(results)
}