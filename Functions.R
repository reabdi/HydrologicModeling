# ###############################################
# Correcting the temperature, changing the units and calculating the average temperature:

Correcting_temperatures <- function(temp_Ave){
  # This function is for correcting the temerature data which have -9999!
  # Moreover, if there is a missed day (which already is), I will find the dey!
  
  input_3 <- read.csv(file="rawFiles.csv")
  
  
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
  
  temp_MAX_HW4_missed <- (temp_MAX_HW4[missed_day_before]+temp_MAX_HW4[missed_day_after])/2
  temp_MIN_HW4_missed <- (temp_MIN_HW4[missed_day_before]+temp_MIN_HW4[missed_day_after])/2
  
  temp_MAX_HW4 <- append(temp_MAX_HW4,as.character(temp_MAX_HW4_missed),missed_day_before)
  temp_MIN_HW4 <- append(temp_MIN_HW4,as.character(temp_MIN_HW4_missed),missed_day_before)
  
  dates_HW4 <- append(dates_HW4,as.character(missed_day),missed_day_before)
  
  # In this section, I want to change the units of temperatures and precipitations:
  
  temp_MAX_HW4 <- as.numeric(temp_MAX_HW4)
  temp_MAX_HW4 <- temp_MAX_HW4 / 10
  temp_MIN_HW4 <- as.numeric(temp_MIN_HW4)
  temp_MIN_HW4 <- temp_MIN_HW4 / 10
  
  temp_Ave <- (temp_MIN_HW4 + temp_MAX_HW4)/2
  return(temp_Ave)
}


# ###############################################
# calculating the AM5s using the average temperature:

Five_day_antecedent_precipitation <- function(precipitation_AM5){
  AM5 <- array(0,dim_input[1])
  
  AM5[1] <- 0
  AM5[2] <- precipitation_AM5[1]
  AM5[3] <- precipitation_AM5[1]+precipitation_AM5[2]
  AM5[4] <- precipitation_AM5[1]+precipitation_AM5[2]+precipitation_AM5[3]
  AM5[5] <- precipitation_AM5[1]+precipitation_AM5[2]+precipitation_AM5[3]+precipitation_AM5[4]
  
  
  for (i in seq(1,(dim_input[1]-5))){
    i1<-i+5
    i2<-i1-1
    i3<-i1-2
    i4<-i1-3
    i5<-i1-4
    i6<-i1-5
    AM5[i1]<-precipitation_AM5[i2]+precipitation_AM5[i3]+precipitation_AM5[i4]+precipitation_AM5[i5]+precipitation_AM5[i6]
  }
  return(AM5)
}


# ###############################################
# considering CN2, I would like to calculate "surface runoff"

surface_runoff <- function(CN2){
  # Now, the next step would be calculating CNs using initial amount of CN which has
  # calculated using the information of assignment and CN1 and CN3.
  # According to the assignment, dormant seasons are: 1, 2, 3, 4, 11 & 12. Therefore,
  # other months considered as growing months.
  
  CN2 <- 65
  CN<-array(0,dim_input[1])
  CN1<-CN2/(2.334-0.01334*CN2)
  CN3<-CN2/(0.4036+0.0059*CN2)
  
  for (i in seq(1,dim_input[1])) {
    if (snow_melt[i] > 0){
      CN[i] <- CN3
    }
    if (snow_melt[i] <= 0){
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
    if (precipitation_AM5[i] > (0.2*S[i])){
      surface_runoff[i] <- ((precipitation_AM5[i]-(0.2*S[i]))^2)/(precipitation_AM5[i]+(0.8*S[i]))
    } 
  }
  return(surface_runoff)
}

# ###############################################
# This function is for calculating actual evapotranspiration:

Actual_evapotranspiration <- function(potential_evapotranspiration){
  months_for_daylight <- input$month
  est_factor <- 6.108 * exp((17.27 * temp_average)/(237.3 + temp_average))
  
  potential_evapotranspiration <- array(0,length(temp_average))
  for (i in seq(1,length(temp_average))){
    if (temp_average[i] > 0){
      if (months_for_daylight[i] == 10){
        potential_evapotranspiration[i] <- (0.021 * (10.9^2) * est_factor[i])/(temp_average[i] + 273)
      }
      if (months_for_daylight[i] == 11){
        potential_evapotranspiration[i] <- (0.021 * (9.7^2) * est_factor[i])/(temp_average[i] + 273)
      }
      if (months_for_daylight[i] == 12){
        potential_evapotranspiration[i] <- (0.021 * (9.0^2) * est_factor[i])/(temp_average[i] + 273)
      }
      if (months_for_daylight[i] == 01){
        potential_evapotranspiration[i] <- (0.021 * (9.3^2) * est_factor[i])/(temp_average[i] + 273)
      }
      if (months_for_daylight[i] == 02){
        potential_evapotranspiration[i] <- (0.021 * (10.4^2) * est_factor[i])/(temp_average[i] + 273)
      }
      if (months_for_daylight[i] == 03){
        potential_evapotranspiration[i] <- (0.021 * (11.7^2) * est_factor[i])/(temp_average[i] + 273)
      }
      if (months_for_daylight[i] == 04){
        potential_evapotranspiration[i] <- (0.021 * (13.1^2) * est_factor[i])/(temp_average[i] + 273)
      }
      if (months_for_daylight[i] == 05){
        potential_evapotranspiration[i] <- (0.021 * (14.3^2) * est_factor[i])/(temp_average[i] + 273) 
      }
      if (months_for_daylight[i] == 06){
        potential_evapotranspiration[i] <- (0.021 * (15^2) * est_factor[i])/(temp_average[i] + 273) 
      }
      if (months_for_daylight[i] == 07){
        potential_evapotranspiration[i] <- (0.021 * (14.6^2) * est_factor[i])/(temp_average[i] + 273) 
      }
      if (months_for_daylight[i] == 08){
        potential_evapotranspiration[i] <- (0.021 * (13.6^2) * est_factor[i])/(temp_average[i] + 273) 
      }
      if (months_for_daylight[i] == 09){
        potential_evapotranspiration[i] <- (0.021 * (12.3^2) * est_factor[i])/(temp_average[i] + 273) 
      }
    }
  }
  
  Actual_evapotranspiration <- array(0,length(temp_average))
  Unsaturated_storage <- array(0,length(temp_average))
  Unsaturated_storage[1] <- 10
  Percolation_PERC <- array(0,length(temp_average))
  for (i in seq(1,length(temp_average))){
    if(months_for_daylight[i] == 10) {Actual_evapotranspiration[i] <- min((0.97*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 11){Actual_evapotranspiration[i] <- min((0.72*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 12){Actual_evapotranspiration[i] <- min((0.61*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 1){Actual_evapotranspiration[i] <- min((0.78*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 2){Actual_evapotranspiration[i] <- min((0.82*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 3){Actual_evapotranspiration[i] <- min((0.82*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 4){Actual_evapotranspiration[i] <- min((0.79*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 5){Actual_evapotranspiration[i] <- min((0.89*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 6){Actual_evapotranspiration[i] <- min((0.91*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 7){Actual_evapotranspiration[i] <- min((0.93*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 8){Actual_evapotranspiration[i] <- min((0.98*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    if(months_for_daylight[i] == 9){Actual_evapotranspiration[i] <- min((1.03*potential_evapotranspiration[i]),Unsaturated_storage[i])}
    Unsaturated_storage[i+1] <- Unsaturated_storage[i] + Infiltration[i] - Actual_evapotranspiration[i]
    if (Unsaturated_storage[i+1] > 10){
      Percolation_PERC[i] <- Unsaturated_storage[i+1] - 10
      Unsaturated_storage[i+1] <-10
      
    }
  }
  results <- list(potential_evapotranspiration,Actual_evapotranspiration,Percolation_PERC,Unsaturated_storage)
  return(results)
}

# ############################################### 
# Next function would be a function to calculate subsurface discharge
# Groundwater Discharge

subsurface_discharge <- function(saturated_zone){
  saturated_zone <- array(0,length(temp_average))
  saturated_zone[1] <- 0.265
  Kb <- 0.924
  for (i in seq(1,length(temp_average))){
    saturated_zone[i+1] <- saturated_zone[i] + PERC[i] - ((1-Kb)*saturated_zone[i])
  }
  subsurface_discharge <- array(0,length(temp_average))
  for (i in seq(1,length(temp_average))){
    subsurface_discharge[i] <- (1-Kb)*saturated_zone[i]
  }
  results <- list(saturated_zone,subsurface_discharge)
  return(results)
}