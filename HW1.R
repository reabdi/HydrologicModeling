# Reza Abdi
# January 31, 2016
# ERE798
# HW #1

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

# Here I am trying to distinguish the first and the last year of the period and 
# determine the leap years.

d2 <- dim(D)
A <- D[1,1]
B <- D[d2[1],1]
C <- c((A+1):B)
year_type <- ifelse((C %% 4 ==0 & C %% 100 != 0) | C %% 400 == 0, 366, 365)

# adding the discharges to the prepared data frame.

D[,4] <- data.frame(discharge=as.numeric(format(input[,2])))

# calculating the annual average flow for all of the water years.

mean1 <- array(0,length(year_type))
d3 <- 0
for (i in seq(1,length(mean1))){
    mean1[i]=mean(input[(d3+1):(d3+year_type[i]),2])
    d3 <- d3+year_type[i]
}

# creating the output file.

output <-  data.frame(Year=as.numeric(format(C)),
                     Leap_Year=as.numeric(format(year_type)),
                     Annual_Q=as.numeric(format(mean1)))

output$Annual_Q <- round(output$Annual_Q,digits=2)

write.table(output,"Abdi_HW1.out",quote=FALSE,append=FALSE,sep="  ",row.names=FALSE)
write("   ","Abdi_HW1.out",append=TRUE)
write("Average_Discharge = ","Abdi_HW1.out",append=TRUE)
Average_daily_flow_total_yrs=round(Average_daily_flow_total_yrs,digits=2)
write(Average_daily_flow_total_yrs,"Abdi_HW1.out",append=TRUE)
  
output

    



