# Reza Abdi
# Feb. 09, 2016
# ERE798
# HW #2

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

##################################################
# Question 1
# calculating the annual MAX

annual_max <- array(0, length(year_type))
d4 <- 0
for (i in seq(1, length(year_type))) {
    annual_max[i] <- max(D$discharge[(d4+1):(d4+year_type[i])])
    d4 <- d4+year_type[i]
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

##################################################
# Quastion 2
# Calculating the mean and standard deviation for annual average, max daily flow
# and 7-day minimum streamflows for all of the period:

question2 <- matrix(0,3,2)
rownames(question2) <- c("annual_Q","annual_max","7Q")
colnames(question2) <- c("mean","SD")

question2[1,1] <- mean(mean1)
question2_1 <- round(question2[1,1],0)
question2[2,1] <- mean(annual_max)
question2_2 <- round(question2[2,1],0)
question2[3,1] <- mean(mean_7Q_min)
question2_3 <- round(question2[3,1],0)
question2[1,2] <- sd(mean1)
question2_4 <- round(question2[1,2],2)
question2[2,2] <- sd(annual_max)
question2_5 <- round(question2[2,2],2)
question2[3,2] <- sd(mean_7Q_min)
question2_6 <- round(question2[3,2],2)

##################################################
# useing the functions:

source("Abdi_functions_HW2.R")

##################################################
# Question 3
# Calculating the 100 year flood using a Log-Pearson III.

source("Abdi_functions_HW2.R")
year_index <- 100
flood_100_year(year_index)

##################################################
# Question 4
# Calculate the 7-day 10-year low streamflow (7Q10)

year_index2 <- 10
lowflow_7Q10_function(year_index2)

##################################################
# preparing the output file

flood_100_year_1 <- round (flood_100_year(year_index), 0)
lowflow_7Q10_1 <- round(lowflow_7Q10_function(year_index2), 0)
mean1 <- round(mean1, digits = 0)
mean_7Q_min <- round(mean_7Q_min, digits = 0)
output <-  data.frame(Year=as.numeric(format(C)),
                     Leap_Year=as.numeric(format(year_type)),
                     Annual_Q=as.numeric(format(mean1)),
                     Annual_MAX=as.numeric(format(annual_max)),
                     seven_day_Q_min=as.numeric(format(mean_7Q_min)))

write.table(output,"Abdi_HW2.out",quote=FALSE,append=FALSE,sep="  |  ",row.names=FALSE)

write("   ","Abdi_HW2.out",append=TRUE)
write("Mean_of_annual_average:","Abdi_HW2.out",append=TRUE)
write(question2_1,"Abdi_HW2.out", append=TRUE)
write("Mean_of_annual_MAX:","Abdi_HW2.out",append=TRUE)
write(question2_2,"Abdi_HW2.out", append=TRUE)
write("Mean_of_MIN_7Q:","Abdi_HW2.out",append=TRUE)
write(question2_3,"Abdi_HW2.out", append=TRUE)
write("Standard_deviation_of_annual_average:","Abdi_HW2.out",append=TRUE)
write(question2_4,"Abdi_HW2.out", append=TRUE)
write("Standard_deviation_of_annual_MAX:","Abdi_HW2.out",append=TRUE)
write(question2_5,"Abdi_HW2.out", append=TRUE)
write("Standard_deviation_of_MIN_7Q:","Abdi_HW2.out",append=TRUE)
write(question2_6,"Abdi_HW2.out", append=TRUE)
write("   ","Abdi_HW2.out",append=TRUE)

write("The_100_year_flood_using_Log_Pearson_III:","Abdi_HW2.out",append=TRUE)
write(flood_100_year_1,"Abdi_HW2.out", append=TRUE)
write("   ","Abdi_HW2.out",append=TRUE)

write("The_7Q10_using_3_parameter_lognormal:","Abdi_HW2.out",append=TRUE)
write(lowflow_7Q10_1 ,"Abdi_HW2.out", append=TRUE) 


    



