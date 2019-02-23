# Reza Abdi
# Feb. 20, 2016
# ERE798
# HW #3

rm(list=ls(all=T))

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

mean1 <- array(0,length(year_type))
d3 <- 0
for (i in seq(1,length(mean1))){
  mean1[i] <- mean(input[(d3+1):(d3+year_type[i]),2])
  d3 <- d3+year_type[i]
}

# calculating the 7-day annual minimum streamflow

# At first, I am going to divide the discharges in a matrix based on the water 
# year. Each column is the data for a water year.

data_frame1 <- matrix(0,366,length(year_type))
d4 <- 0
for (i in seq(1,length(mean1))){
  if (year_type[i] == 365) {
    data_frame1[(1:365),i] <- (input[(d4+1):(d4+year_type[i]),2])
    d4 <- d4+year_type[i]
  } else {
    data_frame1[(1:366),i]  <- (input[(d4+1):(d4+year_type[i]),2])
    d4 <- d4+year_type[i]
  }
}

# In this section I want to calculate all of the possible 7Qs for each water year.
# Considering the leap years causes to create a if/else situation.

mean_7Q_min1 <- matrix(0,360,length(year_type))
d5 <- dim(data_frame1)
z <- 1
for (j in seq(1,d5[2])){
  if (data_frame1[366,j] == 0){
    for (z in seq(1,359)){ 
      mean_7Q_min1[z,j] <- ((data_frame1[z,j]+data_frame1[(z+1),j]+
                               data_frame1[(z+2),j]+data_frame1[(z+3),j]+
                               data_frame1[(z+4),j]+data_frame1[(z+5),j]+
                               data_frame1[(z+6),j])/7)
      
    }
  }
  else {
    for(z in seq(1,360)){ 
      mean_7Q_min1[z,j] <- ((data_frame1[z,j]+data_frame1[(z+1),j]+
                               data_frame1[(z+2),j]+data_frame1[(z+3),j]+
                               data_frame1[(z+4),j]+data_frame1[(z+5),j]+
                               data_frame1[(z+6),j])/7)
      
    }
  }
}

# At the last part, I want to distinguish the minimum values for each water year.
# I tried to make a differece between ordinary an leap years beasue of the zero 
# in the last row of the ordinary years.

mean_7Q_min <- array(0,length(year_type))
for (i in seq(1,length(mean1))){
  if (mean_7Q_min1[360,i] == 0){
    mean_7Q_min[i] <- min(mean_7Q_min1[(1:359),i])
  } else {
    mean_7Q_min[i] <- min(mean_7Q_min1[,i])
  }
}

# calculating the annual MAX

annual_max <- array(0, length(year_type))
d4 <- 0
for (i in seq(1, length(year_type))) {
  annual_max[i] <- max(D$discharge[(d4+1):(d4+year_type[i])])
  d4 <- d4+year_type[i]
}

# Calculate the 7-day 10-year low streamflow (7Q10) and Q100 without the function,
# I want to use the result in the histogram, so I calculated it directly
# and without function.

# flood_100_year

year_index <- 100
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

# calculating the 7Q10:

year_index2 <- 10
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

# ##################  NEW PARTS RELATED TO HW3  #####################
# Question 1
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

y_1st <- round(monthly_averages[1:12],0)
y_2nd <- round(monthly_averages[13:24],0)
y_3rd <- round(monthly_averages[25:36],0)
y_4th <- round(monthly_averages[37:48],0)
months_list <- c("Oct.","Nov.","Dec.","Jan.","Feb.","Mar.","Apr.","May.","Jun.",
                 "Jul.","Aug.","Sep.")

month_nombers <- matrix(0,12,length(year_type))
for (i in seq(1,12)){
  month_nombers[i,] <- seq(i,length(monthly_averages),12)
  i <- i + 1
}

month_averages_2 <- matrix(0,12,length(year_type))
for (i in seq(1,12)){
  month_averages_2[i,] <- monthly_averages[month_nombers[i,]]
  i <- i + 1
}

total_avg <- array(0,12)
for (i in seq(1,12)){
  total_avg[i] <- mean(month_averages_2[i,])
}
total_avg_r <- round(total_avg,0)

question_1 <- data.frame(months_list,y_1st,y_2nd,y_3rd,y_4th,total_avg_r)

# #################################################
# Question 2

plot(C,annual_max,log="y",type="b", main="Scatter graph, HW3-Q2",xlab="Year",
     ylab="Discharge(cfs)-log", col="red", pch=15,xlim=c(1941,1960),
     ylim=c(1,100000), xaxp=c(1941,1960,19))
points(C,mean1,type="b",col="blue",pch=16)
points(C,mean_7Q_min,type="b",col="green",pch=9)
legend("bottomright", pch = c(15, 16, 9),col = c("red", "blue", "green"), 
legend = c("Annual_Max", "Annual_Mean","Minimum_7Q"),cex=0.6)

# #################################################
# Question 3

hist(mean_7Q_min,ylim=c(0,10),col=8 ,xlab=" 7Q minimum (cfs)",
     main="Histogram graph, HW3-Q3")
abline(v=lowflow_7Q10 ,lwd=2, col="black",lty=5)
text(lowflow_7Q10, 10, labels = round(lowflow_7Q10,0) , adj = NULL)
legend("topright",legend=c("Legend:   |: 7Q10"),cex=0.7)

# #################################################
# Question 4

hist(annual_max,xlim=c(5000,30000),ylim=c(0,7), breaks = seq(5000, 30000, 
     by = 2500), col=3 ,xlab="annual MAX streamflows (cfs)",
     main="Histogram graph, HW3-Q4")
abline(v=x_p ,lwd=3, col="black",lty=5)
text(x_p, 7, labels = round(x_p,0) , adj = NULL)
legend("top",legend = c("Legend:  |  : 100 year flood"),cex=0.5)

# #################################################
# Question 5
# I modified the function of HW2 to use in HW3-question5

input_2 <- read.csv(file="Peak_Flow.csv")
source("discharge_100yrs.R")
flood_100_year(year_index)

# ##################  #################  #####################
# preparing the output file


write.table(question_1,"Abdi_HW3.out",quote=FALSE,append=FALSE,sep="  |  ",row.names=FALSE)

write("   ","Abdi_HW3.out",append=TRUE)
write("The_100_year_Peak_flow_using_Log_Pearson_III:","Abdi_HW3.out",append=TRUE)
write(flood_100_year(year_index),"Abdi_HW3.out", append=TRUE)
write("   ","Abdi_HW3.out",append=TRUE)
write("The_100_year_daily_flow_using_Log_Pearson_III:","Abdi_HW3.out",append=TRUE)
write(x_p,"Abdi_HW3.out", append=TRUE)


    



