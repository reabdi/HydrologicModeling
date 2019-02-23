# Reza Abdi
# Mar. 24, 2016
# ERE798
# HW #5

rm(list=ls(all=T))
pmt <- proc.time()
input <- read.csv(file="precipitation.csv")

# During last assignment I found that how to modify the data including temperature and
# precipitation statistics. I changed the value of -9999 and find interpolated amounts
# for required places.
# In this assignment I will use an output file from previous work including the modified
# precipitation values and the dates. 
# Now, for the fist step, I am going to calculate the amount of AMs,
# AMi = 5-day precipitation prior to runoff at day number i.
# we will use AMi values to calculate modifeid amount of CNs as CN1 or CN3.
# At first, I am going to calculate AM1 through AM5 separately, then I will use a 
# pattern for the rest of AMs.

dim_input <- dim(input)
AM5 <- array(0,dim_input[1])

AM5[1] <- 0
AM5[2] <- input$precp[1]
AM5[3] <- input$precp[1]+input$precp[2]
AM5[4] <- input$precp[1]+input$precp[2]+input$precp[3]
AM5[5] <- input$precp[1]+input$precp[2]+input$precp[3]+input$precp[4]


for (i in seq(1,(dim_input[1]-5))){
  i1<-i+5
  i2<-i1-1
  i3<-i1-2
  i4<-i1-3
  i5<-i1-4
  i6<-i1-5
  AM5[i1]<-input$precp[i2]+input$precp[i3]+input$precp[i4]+input$precp[i5]+input$precp[i6]
}


# Now, the next step would be calculating CNs using initial amount of CN which has
# calculated using the information of assignment and CN1 and CN3.
# According to the assignment, dormant seasons are: 1, 2, 3, 4, 11 & 12. Therefore,
# other months considered as growing months.

CN2 <- 72
CN<-array(0,dim_input[1])
CN1<-CN2/(2.334-0.01334*CN2)
CN3<-CN2/(0.4036+0.0059*CN2)

for (i in seq(1,dim_input[1])) {
  if(((input$month[i])==1)|((input$month[i])==2)|((input$month[i])==3)|
    ((input$month[i])==4)|((input$month[i])==11)|((input$month[i])==12)){
    if(AM5[i]<1.3){
      CN[i]<-CN1+((CN2-CN1)/(1.3-0))*AM5[i]
    }
    if((1.3<=AM5[i]) & (AM5[i])<=2.8){
      CN[i]<-CN2+((CN3-CN2)/(2.8-1.3))*(AM5[i]-1.3)
    }
    if(AM5[i]>2.8){
      CN[i]=CN3
    }
  } 
  else 
    if(((input$month[i])==5)|((input$month[i])==6)|((input$month[i])==7)|
      ((input$month[i])==8)|((input$month[i])==9)|((input$month[i])==10)){
      if(AM5[i]<3.6){
        CN[i]<-CN1+((CN2-CN1)/(3.6-0))*AM5[i]
      }
      if((3.6<=AM5[i])&(AM5[i])<=5.3){
        CN[i]<-CN2+((CN3-CN2)/(5.3-3.6))*(AM5[i]-3.6)
      }
      if(AM5[i]>5.3){
        CN[i]=CN3
      }
  }
  CN<-round(CN,digits=1)
}

# After caculating the CNs, now the next issue is S:
# S: Potential Retention
# S will be calculated in SI:

S <- (2540/CN)-25.4

# Surface runoff would be calculated based on equation for two different conditions:

surface_runoff <- array(0,dim_input[1])
for (i in seq(1,dim_input[1])){
  if (input$precp[i] > (0.2*S[i])){
    surface_runoff[i] <- ((input$precp[i]-(0.2*S[i]))^2)/(input$precp[i]+(0.8*S[i]))
    surface_runoff <- round(surface_runoff,digits=1)
  } 
}

# Now, the next part
# calculating SS (subsurface storage)

Subsurface_storage <- array(0,dim_input[1])

for (i in seq(1,(dim_input[1]-1))){
  if(Subsurface_storage[i] < input$ET[i]){
    input$ET[i] <- Subsurface_storage[i]
  } 
    Subsurface_storage[i+1]<-(Subsurface_storage[i]+input$precp[i]-
    surface_runoff[i]-input$ET[i]-((1-0.9)*(Subsurface_storage[i]-input$ET[i])))
}

# Subsurface storage is calculated and it has controlled if the evapotranspiration
# is more than storage water.
# According to the assignment, the subsurface discharge should be calculated:

Subsurface_discharge <- array(0,dim_input[1])
for (i in seq(1,dim_input[1])){
    Subsurface_discharge[i] <- ((1-.9)*(Subsurface_storage[i]-input$ET[i]))
}

# After calculating the surface runoff and subsurface discharge, the final step is 
# calculating the streamflow which is combination 

discharge_predicted <- array(0,dim_input[1])
discharge_predicted <- surface_runoff + Subsurface_discharge 

# After calculating the values of streamflow, the units must be changed to cfs:

discharge_predicted <- discharge_predicted * 
                       ((1541*100000*100000/((2.54*12)^3))/(24*3600))

# #########################################################
# Assignment: part a
# Next steps are calculating and preparing the required cases on the assignment
# At first, for part a in the assignment, Bias and NSE will be calculatd using 
# prepared function which calculate monthly and annual averages for observed and
# predicted data and then evaluate the amounts of Bias and NSE:

source("Bias and Nash Sutcliffe Efficiency.R")

Bias(predicted_and_observed_annual_monthly_flows)
bias_annual_value <- as.numeric(Bias(predicted_and_observed_annual_monthly_flows)[[1]])
bias_monthly_value <- as.numeric(Bias(predicted_and_observed_annual_monthly_flows)[[2]])

Nash_Sutcliffe_NSE(predicted_and_observed_annual_monthly_flows)
NSE_annual_value <- as.numeric(Nash_Sutcliffe_NSE(predicted_and_observed_annual_monthly_flows)[[1]])
NSE_monthly_value <- as.numeric(Nash_Sutcliffe_NSE(predicted_and_observed_annual_monthly_flows)[[2]])

# #########################################################
# Assignment: part b
# I am going to adjust the CN2 value to maximize the NSE of monthly average streamflows.
# Therefore, I created a new function containing all of the previous steps, 
# I defined an array for 10 different CN and asked R to calculate monthly NES for each to 
# find the MAX monthly NSE.

# source("NSE_monthly_MAX_function.R")
# Max_NSE_monthly_average(CN_test_values)
# monthly_NSE_MAX <- as.numeric(Max_NSE_monthly_average(CN_test_values)[[2]])

# #########################################################
# Assignment: part c
# First, preparing annual observation streamflows:

source("Annual_observation_function.R")
annual_observed_streamflow(annual_observation)
annual_observed_discharge <- as.numeric(annual_observed_streamflow(annual_observation)[[1]])
Year <- as.numeric(annual_observed_streamflow(annual_observation)[[2]])
source("Annual_predicted_function.R")
annual_modeled_streamflow(predicted_observation)
annual_predicted_discharge <- as.numeric(annual_modeled_streamflow(predicted_observation))
# A scatter plot containing two different predicted fows and observed streamflow vs water years

library(plotrix)
require(plotrix)

plot(Year,annual_predicted_discharge,type="b", main="Predicted and Observed annual average daily streamflow (CN2=72)",xlab="Year",ylab="Discharge(cfs)",ylim=c(1,2000), col="green",pch=9, xaxp=c(1941,1960,19))
points(Year,annual_observed_discharge,type="b",col="red",pch=12)
legend("topleft", pch = c(9, 12),col = c("green", "red"),legend = c("Model", "Observation"))

# #########################################################
# Assignment: part D

source("monthly_observed_streamflow.R")
monthly_observed_streamflow(total_avg_monthly)
monthly_observed_discharge <- as.numeric(monthly_observed_streamflow(total_avg_monthly)[[1]])
source("monthly_predicted_streamflow.R")
monthly_predicted_streamflow(total_avg_monthly_predicted)
monthly_predicted_discharge <- as.numeric(monthly_predicted_streamflow(total_avg_monthly_predicted))

plot(monthly_predicted_discharge,type="b", main="Predicted and Observed Monthly average streamflow (CN2=72) ",xlab="Month",ylab="Discharge(cfs)",ylim=c(-200,2500), col="blue",pch=9, xaxp=c(1941,1960,19))
points(monthly_observed_discharge,type="b",col="green",pch=12)
legend("topright", pch = c(9, 12,1),col = c("blue", "green"),legend = c("Model", "Observation"))
axis(1, at=1:12, lab=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))

# #########################################################
# Assignment: part e

observed_daily_1960 <- as.numeric(monthly_observed_streamflow(total_avg_monthly)[[2]])
predicted_daily_1960 <- c(discharge_predicted[6941:7305])
plot(predicted_daily_1960,type="l",lwd=2, main="1960 Daily streamflow : Observation vs Model (CN2=72)",xlab="Day",ylab="Discharge(cfs)",ylim=c(-200,25000), col="blue", xaxp=c(1,366,365))
points(observed_daily_1960,type="l",lwd=2, col="red")
legend("topleft", pch = c(20, 20),col = c("blue", "red"),legend = c("Observation","Model"))

time <- proc.time() - pmt
time[]

