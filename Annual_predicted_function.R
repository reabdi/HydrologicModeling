annual_modeled_streamflow <- function(predicted_observation){
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
  
  # calculating the annual average flow for all of the water years.
  
  predicted_observation <- array(0,length(year_type))
  d3 <- 0
  for (i in seq(1,length(predicted_observation))){
    predicted_observation[i] <- mean(streamflow[(d3+1):(d3+year_type[i])])
    d3 <- d3+year_type[i]
  }
  return(predicted_observation)
}
