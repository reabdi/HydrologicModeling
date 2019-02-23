# Reza Abdi
# Apr. 18, 2016
# ERE798
# HW #6

rm(list=ls(all=T))
pmt <- proc.time()
input <- read.csv(file="precipitation.csv")

# In this assignment, I want to use more functions. So for the first stem, I am going to 
# use a function for correcting the temperature data.

 
# Correcting_temperatures(temp_Ave)
# temp_average <- as.numeric(Correcting_temperatures(temp_Ave))

# Now, using the corrected averate temperature, I am going to calculate snow accumulation:
# #########################################################
# Estimating Surface Runoff, Snow pack and Snowmelt
# #########################################################
source("Functions.R")

dim_input <- dim(input)
temp_average <- array(0,dim_input[1])
for (i in seq(1:dim_input[1])){
  temp_average[i] <- (input[i,8]+input[i,9])/(2*10)
}
input[,10] <- temp_average


rain <- array(0,dim_input[1])
snow_melt <- array(0,dim_input[1])
snow_accumulation_SN <- array(0,dim_input[1])

for (i in seq(1:dim_input[1])) {
    if (input$V10[i]<=0) {
       snow_accumulation_SN[i+1]<-snow_accumulation_SN[i]+input$precp[i]
       snow_melt[i] <- 0} 
    else {
      rain[i]<-input$precp[i]    
      snow_melt[i]<-min(snow_accumulation_SN[i],(0.45*input$V10[i]))
      snow_accumulation_SN[i+1]<-snow_accumulation_SN[i]-snow_melt[i]
    }
}

precipitation_AM5 <- rain + snow_melt

# Now, I want to use the function for calculating the AM5s

AM5 <- as.numeric(Five_day_antecedent_precipitation(precipitation_AM5))

# Now, the next step would be calculating CNs using initial amount of CN which has
# calculated using the information of assignment and CN1 and CN3.
# According to the assignment, dormant seasons are: 1, 2, 3, 4, 11 & 12. Therefore,
# other months considered as growing months.
# And then, calculating SS (subsurface storage)
# All of these issues located in a function:

surface_runoff_SR <- surface_runoff(CN2)

# The infiltration is estimated as the quantity of rain and snowmelt  that is not surface runoff

# #########################################################
# Infiltration, Soil Moisture, Evapotranspiration and Percolation
# #########################################################


Infiltration <- precipitation_AM5 - surface_runoff_SR

# I modified a function to calculate the actual evapotranspiration:

Actual_evapotranspiration(potential_evapotranspiration)
Et <- as.numeric(Actual_evapotranspiration(potential_evapotranspiration)[[1]])
ETt <- as.numeric(Actual_evapotranspiration(potential_evapotranspiration)[[2]])
PERC <- as.numeric(Actual_evapotranspiration(potential_evapotranspiration)[[3]])

# #########################################################
# Groundwater Discharge
# #########################################################

# Using percolation, Kb and subsurface discharge, I would be able to calculate the saturated 
# zone (groundwater) discharge

subsurface_discharge(saturated_zone)
SAT <- as.numeric(subsurface_discharge(saturated_zone)[[1]])
SD <- as.numeric(subsurface_discharge(saturated_zone)[[2]])

# #########################################################
# Streamflow
# #########################################################
# The final estimated streamflow (Qt) on day t is estimated as a combination of surface
# runoff and groundwater discharge:


streamflow <- ((SD + surface_runoff_SR)*(1541*100000*100000/((2.54*12)^3))/(24*3600))

# #########################################################
# Part b: Monthly and Annual Bias and NSE:
# #########################################################

source("Bias and Nash Sutcliffe Efficiency.R")

Bias(predicted_and_observed_annual_monthly_flows)
bias_annual_value <- as.numeric(Bias(predicted_and_observed_annual_monthly_flows)[[1]])
bias_monthly_value <- as.numeric(Bias(predicted_and_observed_annual_monthly_flows)[[2]])

Nash_Sutcliffe_NSE(predicted_and_observed_annual_monthly_flows)
NSE_annual_value <- as.numeric(Nash_Sutcliffe_NSE(predicted_and_observed_annual_monthly_flows)[[1]])
NSE_monthly_value <- as.numeric(Nash_Sutcliffe_NSE(predicted_and_observed_annual_monthly_flows)[[2]])

# #########################################################
# Part d: A scatter plot containing the predicted & observed annual average daily streamflow
# #########################################################

# First, preparing annual observation streamflows:

source("Annual_observation_function.R")
annual_observed_discharge <- as.numeric(annual_observed_streamflow(annual_observation)[[1]])
Year <- as.numeric(annual_observed_streamflow(annual_observation)[[2]])
source("Annual_predicted_function.R")
annual_predicted_discharge <- as.numeric(annual_modeled_streamflow(predicted_observation))
# A scatter plot containing two different predicted fows and observed streamflow vs water years

library(plotrix)
require(plotrix)

plot(Year,annual_predicted_discharge,type="b", main="Predicted and Observed annual average daily streamflow (CN2=65)",xlab="Year",ylab="Discharge(cfs)",ylim=c(1,2000), col="green",pch=9, xaxp=c(1941,1960,19))
points(Year,annual_observed_discharge,type="b",col="red",pch=12)
legend("topleft", pch = c(9, 12),col = c("green", "red"),legend = c("Model", "Observation"))

# #########################################################
# Part e: A scatter plot containing the predicted & observed monthly average daily streamflow
# #########################################################

source("monthly_observed_streamflow.R")
monthly_observed_discharge <- as.numeric(monthly_observed_streamflow(total_avg_monthly)[[1]])
source("monthly_predicted_streamflow.R")
monthly_predicted_discharge <- as.numeric(monthly_predicted_streamflow(total_avg_monthly_predicted))

plot(monthly_predicted_discharge,type="b", main="Predicted and Observed Monthly average streamflow (CN2=65) ",xlab="Month",ylab="Discharge(cfs)",ylim=c(-200,2500), col="blue",pch=9, xaxp=c(1941,1960,19))
points(monthly_observed_discharge,type="b",col="green",pch=12)
legend("topright", pch = c(9, 12,1),col = c("blue", "green"),legend = c("Model", "Observation"))
axis(1, at=1:12, lab=c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep"))

# #########################################################
# Part e: A line plot containing the predicted & observed daily streamflow for the 1960 water year
# #########################################################

observed_daily_1960 <- as.numeric(monthly_observed_streamflow(total_avg_monthly)[[2]])
predicted_daily_1960 <- c(streamflow[6941:7305])
plot(predicted_daily_1960,type="l",lwd=2, main="1960 Daily streamflow : Observation vs Model (CN2=65)",xlab="Day",ylab="Discharge(cfs)",ylim=c(-200,25000), col="blue", xaxp=c(1,366,365))
points(observed_daily_1960,type="l",lwd=2, col="red")
legend("topleft", pch = c(20, 20),col = c("blue", "red"),legend = c("Observation","Model"))

time <- proc.time() - pmt
time[]

