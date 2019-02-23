# Reza Abdi
# Feb. 20, 2016
# ERE798
# HW #4

rm(list=ls(all=T))
pmt <- proc.time()

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

# ##################  #################  #####################
# ##################        HW4          #####################
# ##################  #################  #####################

input_3 <- read.csv(file="HW4.csv")


# I am going to interpolate the -9999 values. 
# At first, I consider the TMIN, which has some -9999 values:

nines_TMIN <- which(input_3$TMIN == -9999)
if (length(nines_TMIN) != 0){
rr <- rle(nines_TMIN - seq_along(nines_TMIN))
rr$values <- seq_along(rr$values)
s <- split(nines_TMIN, inverse.rle(rr)) 

ss <- array(0,length(s))    
for (i in seq(1,length(s))){
  ss[i] <- length(s[[i]])
  } 


temp_MIN_HW4 <- input_3$TMIN
for (i in seq(1,length(s))){
  if (length(s[[i]]) == 1){
    temp_MIN_HW4[s[[i]]] <- (temp_MIN_HW4[s[[i]]-1] + temp_MIN_HW4[s[[i]]+1])/2
  } else {
    for (j in seq(1,length(s[[i]]))){
      temp_MIN_HW4[s[[i]][j]] <- temp_MIN_HW4[(s[[i]][1])-1] + 
                                 (j * (temp_MIN_HW4[(s[[i]][1])+length(s[[i]])] 
                                  - temp_MIN_HW4[(s[[i]][1])-1]))/(ss[[i]]+1)
      }
    } 
  }
}


# Now, I would like to do same process at TMAX,

nines_TMAX <- which(input_3$TMAX == -9999)
if (length(nines_TMAX) != 0){
  rr_2 <- rle(nines_TMAX - seq_along(nines_TMAX))
  rr_2$values <- seq_along(rr_2$values)
  s_2 <- split(nines_TMAX, inverse.rle(rr_2)) 
  
  ss_2 <- array(0,length(s_2))    
  for (i in seq(1,length(s_2))){
    ss_2[i] <- length(s_2[[i]])
  }
  
  
  temp_MAX_HW4 <- input_3$TMAX
  for (i in seq(1,length(s_2))){
    if (length(s_2[[i]]) == 1){
      temp_MAX_HW4[s_2[[i]]] <- (temp_MAX_HW4[s_2[[i]]-1] + temp_MAX_HW4[s_2[[i]]+1])/2
    } else {
      for (j in seq(1,length(s_2[[i]]))){
        temp_MAX_HW4[s_2[[i]][j]] <- temp_MAX_HW4[(s_2[[i]][1])-1] + 
          (j * (temp_MAX_HW4[(s_2[[i]][1])+length(s_2[[i]])] 
                - temp_MAX_HW4[(s_2[[i]][1])-1]))/(ss_2[[i]]+1)
      }
    } 
  }
}

# Currently, I have the updated data for MAX and MIN temperatures,
# Now, I want to correct the preciptation data serie and set 0 instead of -9999

prcp_HW4 <- as.numeric(input_3$PRCP)
for (i in seq(1,length(prcp_HW4))){
  if (prcp_HW4[i] == -9999){
    prcp_HW4[i] <- 0
  }
}

# In this section, I am going to find the missed day,

date_temp_preci <- input_3$DATE
yrs_HW4 <- substr(date_temp_preci,1,4)
months_HW4 <- substr(date_temp_preci,5,6)
days_HW4 <- substr(date_temp_preci,7,8)
dates_HW4 <- paste0(yrs_HW4,"-",months_HW4,"-",days_HW4)

day_series <-(seq(as.Date("1940-10-1"), as.Date("1960-09-30"), "days"))

for (i in seq(1,length(day_series))){
  if (day_series[i] != dates_HW4[i]){
    missed_day <- (day_series[i])
    print(missed_day)
    break
  }
}

missed_day_before <- which(dates_HW4 == missed_day-1)
missed_day_after <- which(dates_HW4 == missed_day+1)

prcp_missed <- (prcp_HW4[missed_day_before]+prcp_HW4[missed_day_after])/2
temp_MAX_HW4_missed <- (temp_MAX_HW4[missed_day_before]+temp_MAX_HW4[missed_day_after])/2
temp_MIN_HW4_missed <- (temp_MIN_HW4[missed_day_before]+temp_MIN_HW4[missed_day_after])/2

prcp_HW4 <- append(prcp_HW4,as.character(prcp_missed),missed_day_before)
temp_MAX_HW4 <- append(temp_MAX_HW4,as.character(temp_MAX_HW4_missed),missed_day_before)
temp_MIN_HW4 <- append(temp_MIN_HW4,as.character(temp_MIN_HW4_missed),missed_day_before)

dates_HW4 <- append(dates_HW4,as.character(missed_day),missed_day_before)

# In this section, I want to change the units of temperatures and precipitations:

prcp_HW4 <- as.numeric(prcp_HW4)
prcp_HW4 <- prcp_HW4 / 10
temp_MAX_HW4 <- as.numeric(temp_MAX_HW4)
temp_MAX_HW4 <- temp_MAX_HW4 / 10
temp_MIN_HW4 <- as.numeric(temp_MIN_HW4)
temp_MIN_HW4 <- temp_MIN_HW4 / 10

temp_prec_HW4 <- data.frame(dates_HW4,prcp_HW4,temp_MIN_HW4,temp_MAX_HW4)

# Now, after preparing my data, I want to calculate E (potential evapotranspiration)
# for considering the Ns, number of hours of daylight, I want to add the number
# of months to my data frame.

months_for_daylight <- substr(dates_HW4,6,7)
temp_prec_HW4[,5] <- months_for_daylight

temp_maen_HW4 <- (temp_MIN_HW4+temp_MAX_HW4)/2
est_factor <- 6.108 * exp((17.27 * temp_maen_HW4)/(237.3 + temp_maen_HW4))
temp_prec_HW4$V5 <- as.numeric(temp_prec_HW4$V5)

potential_evapotranspiration <- array(0,length(temp_maen_HW4))
for (i in seq(1,length(temp_maen_HW4))){
  if (temp_maen_HW4[i] > 0){
    if (temp_prec_HW4$V5[i] == 10){
      potential_evapotranspiration[i] <- (0.021 * (10.9^2) * est_factor[i])/(temp_maen_HW4[i] + 273)
    }
    if (temp_prec_HW4$V5[i] == 11){
      potential_evapotranspiration[i] <- (0.021 * (9.7^2) * est_factor[i])/(temp_maen_HW4[i] + 273)
    }
    if (temp_prec_HW4$V5[i] == 12){
      potential_evapotranspiration[i] <- (0.021 * (9.0^2) * est_factor[i])/(temp_maen_HW4[i] + 273)
    }
    if (temp_prec_HW4$V5[i] == 01){
      potential_evapotranspiration[i] <- (0.021 * (9.3^2) * est_factor[i])/(temp_maen_HW4[i] + 273)
    }
    if (temp_prec_HW4$V5[i] == 02){
      potential_evapotranspiration[i] <- (0.021 * (10.4^2) * est_factor[i])/(temp_maen_HW4[i] + 273)
    }
    if (temp_prec_HW4$V5[i] == 03){
      potential_evapotranspiration[i] <- (0.021 * (11.7^2) * est_factor[i])/(temp_maen_HW4[i] + 273)
    }
    if (temp_prec_HW4$V5[i] == 04){
      potential_evapotranspiration[i] <- (0.021 * (13.1^2) * est_factor[i])/(temp_maen_HW4[i] + 273)
    }
    if (temp_prec_HW4$V5[i] == 05){
      potential_evapotranspiration[i] <- (0.021 * (14.3^2) * est_factor[i])/(temp_maen_HW4[i] + 273) 
    }
    if (temp_prec_HW4$V5[i] == 06){
      potential_evapotranspiration[i] <- (0.021 * (15^2) * est_factor[i])/(temp_maen_HW4[i] + 273) 
    }
    if (temp_prec_HW4$V5[i] == 07){
      potential_evapotranspiration[i] <- (0.021 * (14.6^2) * est_factor[i])/(temp_maen_HW4[i] + 273) 
    }
    if (temp_prec_HW4$V5[i] == 08){
      potential_evapotranspiration[i] <- (0.021 * (13.6^2) * est_factor[i])/(temp_maen_HW4[i] + 273) 
    }
    if (temp_prec_HW4$V5[i] == 09){
      potential_evapotranspiration[i] <- (0.021 * (12.3^2) * est_factor[i])/(temp_maen_HW4[i] + 273) 
    }
  }
}
 

prcp_cm <- (temp_prec_HW4$prcp_HW4) / 10
discharge_predicted <- prcp_cm - potential_evapotranspiration

# the required section to change the units from cm/day to cfs:

discharge_predicted_cfs <- discharge_predicted * (1541 * (1000^2) * (3.28084^2))/(2.54 * 12 * 86400) 

annual_discharge_predicted_cfs <- array(0,length(year_type))
d3 <- 0
for (i in seq(1,length(annual_discharge_predicted_cfs))){
  annual_discharge_predicted_cfs[i] <- mean(discharge_predicted_cfs[(d3+1):(d3+year_type[i])])
  d3 <- d3+year_type[i]
}

# defining the monthly average discharges for the predicted stream daily flows

source("function_monthly_predicted_discharges.R")
monthly_avg_predicted(number_of_days_table)
predicted_monthly_streamflow <- as.numeric(monthly_avg_predicted(number_of_days_table)[[1]])

# calculating the Nash Sutcliffe Efficiency - Bias & NSE values:

predicted_flows_for_every_month <- as.numeric(monthly_avg_predicted(number_of_days_table)[[2]])
source("Nash Sutcliffe Efficiency.R")

Nash_Sutcliffe_Bias(predicted_flows_for_every_month)
bias_annual_value <- as.numeric(Nash_Sutcliffe_Bias(predicted_flows_for_every_month)[[1]])
bias_monthly_value <- as.numeric(Nash_Sutcliffe_Bias(predicted_flows_for_every_month)[[2]])

Nash_Sutcliffe_NSE(Average_daily_flow_total_yrs)
NSE_annual_value <- as.numeric(Nash_Sutcliffe_NSE(Average_daily_flow_total_yrs)[[1]])
NSE_monthly_value <- as.numeric(Nash_Sutcliffe_NSE(Average_daily_flow_total_yrs)[[2]])

# Preparing the 1st plot, predicted  discharge vs observed discharge

library(plotrix)
require(plotrix)

plot(C,mean1,type="b", main="Predicted VS Observed annual average daily discharges",xlab="Year",ylab="Discharge(cfs)",ylim=c(1,2000), col="blue",pch=10, xaxp=c(1941,1960,19))
points(C,annual_discharge_predicted_cfs,type="b",col="red",pch=15)
legend("topleft", pch = c(10, 15),col = c("green", "red"),legend = c("Predicted", "Observation"),cex = 1.0)

plot(predicted_monthly_streamflow,type="b", main="Predicted VS Observed Monthly average discharges",xlab="Month",ylab="Discharge(cfs)",ylim=c(-200,2500), col="blue",pch=12, xaxp=c(1941,1960,19))
points(total_avg_monthly,type="b",col="red",pch=18)
legend("topright", pch = c(12,18),col = c("blue", "red"),legend = c("Predicted", "Observation"))
axis(1, at=1:12, lab=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))


# I would like to prepare a table including the predicted and observed streamflow
# for annual and monthly conditions. More ever, Nash Sutcliffe bias & NSE will be
# added to the output.

predicted_annual_flow <- round(annual_discharge_predicted_cfs, 0)
observed_annual_flow <- round(mean1, 0)
annual_results <- data.frame(predicted_annual_flow, observed_annual_flow)
bias_annual_value_rounded <- round(bias_annual_value, 0)
NSE_annual_value_rounded <- round(NSE_annual_value, 2)
bias_monthly_value_rounded <- round(bias_monthly_value, 0)
NSE_monthly_value_rounded <- round(NSE_monthly_value, 2)

predicted_monthly_flow <- round(predicted_monthly_streamflow, 0)
observed_monthly_flow <-round(total_avg_monthly, 0)
monthly_results <- data.frame(predicted_monthly_flow, observed_monthly_flow)

write("Average Annual Discharge for 20 Years-Predicted VS observed:","Abdi_HW4.out")
write.table(annual_results,"Abdi_HW4.out",quote=FALSE,append=T,sep="  |  ",row.names=T,col.names=T)
write("   ","Abdi_HW4.out",append=TRUE)
write("Average Monthly Discharge for 20 Years-Predicted VS observed:","Abdi_HW4.out",append=TRUE)
write.table(monthly_results,"Abdi_HW4.out",quote=FALSE,append=T,sep="  |  ",row.names=T,col.names=T)
write("   ","Abdi_HW4.out",append=TRUE)
write("Bias for Annual Average:","Abdi_HW4.out",append=TRUE)
write(bias_annual_value_rounded,"Abdi_HW4.out",append=TRUE)
write("NSE for Annual Average:","Abdi_HW4.out",append=TRUE)
write(NSE_annual_value_rounded,"Abdi_HW4.out",append=TRUE)
write("Bias for Monthly Average:","Abdi_HW4.out",append=TRUE)
write(bias_monthly_value_rounded,"Abdi_HW4.out",append=TRUE)
write("NSE for Monthly Average:","Abdi_HW4.out",append=TRUE)
write(NSE_monthly_value_rounded,"Abdi_HW4.out",append=TRUE)

time <- proc.time() - pmt
time[]



