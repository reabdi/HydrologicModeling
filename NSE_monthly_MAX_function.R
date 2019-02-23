# Using this function, I want to recalculate whole the process for a list of CNs to get 
# MAX value for NSE monthly average.

Max_NSE_monthly_average <- function(CN_test_values){
  # I have the input file and consider it as the first step
  
  input_function <- read.csv(file="precipitation.csv") 
  
  dim_input_function <- dim(input_function)
  
  # preparing the AM5 values using precipitation data.
  
  AM5 <- array(0,dim_input_function[1]) 
  AM5[1] <- 0
  AM5[2] <- input$precp[1]
  AM5[3] <- input$precp[1]+input$precp[2]
  AM5[4] <- input$precp[1]+input$precp[2]+input$precp[3]
  AM5[5] <- input$precp[1]+input$precp[2]+input$precp[3]+input$precp[4]
    for (i in seq(6,dim_input[1],1)){
    i1<-i+5
    i2<-i1-1
    i3<-i1-2
    i4<-i1-3
    i5<-i1-4
    i6<-i1-5
    AM5[i1]<-input$precp[i2]+input$precp[i3]+input$precp[i4]+input$precp[i5]+input$precp[i6]
  }
  
  # After defining the AM5 values, I would like to calculate NSEs for each CN
  
  CN_test_values <- c(90,80,70,60,50,40,30,20,10,5)
  CN <- matrix(0,dim_input_function[1],10)
  CN1 <- array(0,10)
  CN2 <- array(0,10)
  CN3 <- array(0,10)
  for (i in seq(1:10)){
    CN2[i] <- CN_test_values[i]
    CN1[i]<-CN2[i]/(2.334-0.01334*CN2[i])
    CN3[i]<-CN2[i]/(0.4036+0.0059*CN2[i])
    
    for (j in seq(1,dim_input_function[1])) {
      if(((input_function$month[j])==1)|((input_function$month[j])==2)|((input_function$month[j])==3)|
         ((input_function$month[j])==4)|((input_function$month[j])==11)|((input_function$month[j])==12)){
        if(AM5[j]<1.3){
          CN[j,i]<-CN1[i]+((CN2[i]-CN1[i])/(1.3-0))*AM5[j]
        }
        if((1.3<=AM5[j]) & (AM5[j])<=2.8){
          CN[j,i]<-CN2[i]+((CN3[i]-CN2[i])/(2.8-1.3))*(AM5[j]-1.3)
        }
        if(AM5[j]>2.8){
          CN[j,i]=CN3[i]
        }
      } 
      else 
        if(((input_function$month[j])==5)|((input_function$month[j])==6)|((input_function$month[j])==7)|
           ((input_function$month[j])==8)|((input_function$month[j])==9)|((input_function$month[j])==10)){
          if(AM5[j]<3.6){
            CN[j,i]<-CN1[i]+((CN2[i]-CN1[i])/(3.6-0))*AM5[j]
          }
          if((3.6<=AM5[j])&(AM5[j])<=5.3){
            CN[j,i]<-CN2[i]+((CN3[i]-CN2[i])/(5.3-3.6))*(AM5[j]-3.6)
          }
          if(AM5[j]>5.3){
            CN[j,i]=CN3[i]
          }
        }
      CN<-round(CN,digits=1)
    }
  }
  # After calculating CN for each CN2, now S should calculate for each CN.
  
  S <- (2540/CN)-25.4
  # Next stem would be calculating the surface runoff and after that subsurface storage.  
  surface_runoff <- matrix(0,dim_input_function[1],10)
  for (j in seq(1:10)){
     for (i in seq(1,dim_input_function[1])){
      if (input_function$precp[i] > (0.2*S[i,j])){
        surface_runoff[i,j] <- ((input_function$precp[i]-(0.2*S[i,j]))^2)/(input_function$precp[i]+(0.8*S[i,j]))
        surface_runoff <- round(surface_runoff,digits=1)
      } 
    }
  }
  
  Subsurface_storage <- matrix(0,dim_input_function[1],10)
  for (j in seq(1:10)){
   for (i in seq(1,(dim_input[1]-1))){
    if(Subsurface_storage[i,j] < input_function$ET[i]){
      input_function$ET[i] <- Subsurface_storage[i,j]
    } 
    Subsurface_storage[(i+1),j]<-(Subsurface_storage[i,j]+input_function$precp[i]-
                                surface_runoff[i,j]-input_function$ET[i]-((1-0.9)*(Subsurface_storage[i,j]-input_function$ET[i])))
   } 
  }
  
  # Subsurface storage is calculated and it has controlled if the evapotranspiration
  # is more than storage water.
  # According to the assignment, the subsurface discharge should be calculated:
  
  Subsurface_discharge <- matrix(0,dim_input_function[1],10)
  for (j in seq(1:10)){
   for (i in seq(1,dim_input[1])){
    Subsurface_discharge[i,j] <- ((1-.9)*(Subsurface_storage[i,j]-input_function$ET[i]))
   }
  }
  
  # After calculating the surface runoff and subsurface discharge, the final step is 
  # calculating the streamflow which is combination 
  
  discharge_predicted <- matrix(0,dim_input_function[1],10)
  discharge_predicted <- surface_runoff + Subsurface_discharge 
  
  # The units must be changed to cfs:
  
  discharge_predicted <- discharge_predicted * 
                         ((1541*100000*100000/((2.54*12)^3))/(24*3600))
  
  # #################################################################################
  # After calculating the predicted discharges, the next step is defining monthly NSE
  
  input_function2 <- read.csv(file="daily_Data_West_Branch.csv") 
  d1=dim(input_function2)
  Average_daily_flow_total_yrs <- (sum(input_function2[,2]))/d1[1]
  
  # We shoud have monthly average based on 
  # (1) observed data:
  
  date <- as.Date(input_function2[,1], format="%m/%d/%Y")
  D <- data.frame(year=as.numeric(format(date,format="%Y")),
                  month=as.numeric(format(date,format="%m")),
                  day=as.numeric(format(date,format="%d")))
  D[,4] <- data.frame(discharge=as.numeric(format(input_function2[,2])))
  d2 <- dim(D)
  
  date_start <- input_function2[1,1]
  date_start_R <- as.Date(date_start,format="%m/%d/%Y")
  date_end <- input_function2[(d2[1]),1]
  date_end_R <- as.Date(date_end,format="%m/%d/%Y")
  number_of_days <- diff(seq(as.Date(date_start_R), as.Date(date_end_R+1), by = "month"))
  number_of_days_table <- ts(number_of_days, start = c(D[1,1], D[1,2]), freq = 12)
  
  monthly_averages <- array(0,length(number_of_days_table))
  j <- 0
  for(i in seq(1,length(monthly_averages))){
    monthly_averages[i] <- mean(input_function2[(j+1):(j+number_of_days_table[i]),2])
    j <- j + number_of_days_table[i]
  }
  
  # (2) based on predicted or modeled data:
  
  predicted_flows_for_every_month <- matrix(0,length(number_of_days_table),10)
  j <- 0
  for (z in seq(1:10)){
   for(i in seq(1,length(monthly_averages))){
    predicted_flows_for_every_month[i,z] <- mean(discharge_predicted[((j+1):(j+number_of_days_table[i])),z])
    j <- j + number_of_days_table[i]
   }
  }

  # Now, I am going to calculate monthly NSE:  
  sum_monthly_NSE_1 <- array(0,10)
  sum_monthly_NSE_2 <- array(0,10)
  NSE_monthly_values <- array(0,10)
  for (j in seq(1:10)){
    for (i in seq(1, length(monthly_averages))){
      sum_monthly_NSE_1[j] <- sum_monthly_NSE_1[j] + (predicted_flows_for_every_month[i,j] 
                                                - monthly_averages[i])^2 
      sum_monthly_NSE_2[j] <- sum_monthly_NSE_2[j] + (monthly_averages[i] - Average_daily_flow_total_yrs)^2                                        
    }
    NSE_monthly_values[j] <- 1 - (sum_monthly_NSE_1[j] / sum_monthly_NSE_2[j])
    NSE_monthly_MAX <- max(NSE_monthly_values)
    NSE_values <- list(NSE_monthly_values, NSE_monthly_MAX)
  
  }
  return(NSE_values)
}