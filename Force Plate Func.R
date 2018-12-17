
###Pasco File Read
Pasco_read <- function(flnm) {
  read_delim(flnm, Encoding("windows-1250"), delim = "\t", skip = 1, col_names = c(
    "Time", "Force Beam 1A", "Force Beam 2A", "Force Beam 3A", "Force Beam 4A", "VerticalForceA", 
    "Force Beam 1C", "Force Beam 2C", "Force Beam 3C", "Force Beam 4C", "VerticalForceC")) %>%
    mutate_all(as.numeric) %>%
    select(c(1, 6, 11)) %>%
    mutate(MassA = VerticalForceA / 9.80665,
           MassC = VerticalForceC / 9.80665,
           Combined = VerticalForceA + VerticalForceC,
           TotalMass = MassA+MassC)
}

####Produce weight/ass vairables
weight_mass <- function(df) {
  df %>%
    summarise_at(vars(4,5,7,2,3,6), funs(mean))
}

###Accel to Velocity
Velocity_func <- function(time, accel) {
  cumsum(c(0,time[-1] - time[-length(time)]) * accel)  
}

###Produce force, acceleration and velocity metrics
Initial_metric <- function(x) {

  x %>%
    mutate(NetForceA = VerticalForceA - weight_mass$VerticalForceA,
           NetForceC = VerticalForceC - weight_mass[[5]],
           NetForceTotal = Combined - weight_mass[[6]],
           ForceA = VerticalForceA/weight_mass[[1]],
           ForceC = VerticalForceC/weight_mass[[2]],
           ForceTotal = Combined/weight_mass[[3]],
           AccelA = NetForceA/weight_mass[[1]],
           AccelC = NetForceC/weight_mass[[2]],
           AccelTotal = NetForceTotal/weight_mass[[3]],
           Velocity_A =  Velocity_func(Time, AccelA),
           Velocity_C =  Velocity_func(Time, AccelC),
           Velocity_total =  Velocity_func(Time, AccelTotal),
           DisplacementA = Velocity_A*(1/sample_rate),
           DisplacementC = Velocity_C*(1/sample_rate),
           DisplacementTotal = Velocity_total*(1/sample_rate),
           DisplacementA = DisplacementA+lag(DisplacementA),
           DisplacementC = DisplacementC+lag(DisplacementC),
           DisplacementTotal = DisplacementTotal+lag(DisplacementTotal),
           ImpulseA = ((NetForceA+lag(NetForceA))/2)*(1/sample_rate),
           ImpulseC = ((NetForceC+lag(NetForceC))/2)*(1/sample_rate),
           ImpulseTotal = ((NetForceTotal+lag(NetForceTotal))/2)*(1/sample_rate),
           PowerA = Velocity_A*ForceA,
           PowerC = Velocity_C*ForceC,
           PowerTotal = Velocity_total*ForceTotal,
           RelativePowerA = PowerA/weight_mass[[1]],
           RelativePowerC = PowerC/weight_mass[[2]],
           RelativePowerTotal = PowerTotal/weight_mass[[3]],
           Symetry = VerticalForceA/VerticalForceC
    )
}  


takeoff_land_times <- function(x) {
  
  A <- x %>%
    filter(VerticalForceA < 40)
  a <- min(A$Time)
  C <- x %>%
    filter(VerticalForceC < 40)
  b <- min(C$Time)
  Total <- x %>%
    filter(Combined < 40)
  c <- min(Total$Time)
  A2 <- x %>%
    filter(Time > a & VerticalForceA > 40)
  d <- min(A2$Time)
  C2 <- x %>%
    filter(Time > b & VerticalForceC>40)
  e <- min(C2$Time)
  Total2 <- x %>%
    filter(Time > c & Combined >40)
  f <- min(Total2$Time)
  
  return(list(TakeOffTimeA=a,TakeOffTimeC=b, TakeOffTimeTotal=c, LandingTimeA=d, LandingTimeC=e, LandingTimeTotal=f,
              FlightTimeA=(d-a), FlightTimeC=(e-b), FlightTimetotal=(f-c),
              JumpHeightA = (((d-a)/2)^2)*9.80665*0.5, JumpHeightC = (((e-b)/2)^2)*9.80665*0.5,
              JumpHeightTotal = (((f-c)/2)^2)*9.80665*0.5
  ))
}

row_starts <- function(df) {
  
  Test1 <- df %>%
    filter(between(row_number(), 1, 300))
  a <- as.integer(weight_mass[[4]] - (5*(sd(Test1$VerticalForceA))))
  a <- min(which(data2$VerticalForceA <=a))
  
  b <- as.integer(weight_mass[[5]] - (5*(sd(Test1$VerticalForceC))))
  b <- min(which(data2$VerticalForceC <=b))
  
  c <- weight_mass[[6]] - (5*(sd(Test1$Combined)))
  c <- min(which(data2$Combined <=c))
  
  d <- min(which(grepl(JumpLandFlight_list[[1]], data2$Time)))
  e <- min(which(grepl(JumpLandFlight_list[[2]], data2$Time)))
  f <- min(which(grepl(JumpLandFlight_list[[3]], data2$Time)))
  
  return(list(Start_A=a, Start_C=b, Start_Total=c, rowTakeOffA=d, rowTakeOffC=e, rowTakeOffTotal=f,
              RSI_A = (d - a)*(1/sample_rate),
              RSI_C = (e - b)*(1/sample_rate),
              RSI_Total = (f - c)*(1/sample_rate)
  ))
  
}

grf_data <- function(df) {
  
  GRFData <- df %>%
    select(c(1:3, 6, 11:13)) %>%
    filter(Time < JumpLandFlight_list[[3]])
  
  a <- max(GRFData$VerticalForceA)
  b <- max(GRFData$VerticalForceC)
  c <- max(GRFData$Combined)
  
  d <- max(GRFData$ForceA)
  e <- max(GRFData$ForceC)
  f <- max(GRFData$ForceTotal)
  
  g <- a/weight_massLIST[[4]]
  h <- b/weight_massLIST[[5]]
  i <- c/weight_massLIST[[6]]
  
  return(list(PeakGRF_A=a,PeakGRF_C=b, PeakGRF_Total=c, PeakGRF_A_Nkg=d, PeakGRF_C_Nkg=e, PeakGRF_Total_Nkg=f,
              Relative_PeakF_a=g, Relative_PeakF_c=h, Relative_PeakF_total=i))
}

Peak_landasym <- function(df){
  
  a <- df$Time[df$VerticalForceA == list_grf[[1]]]
  b <- df$Time[df$VerticalForceC == list_grf[[2]]]
  c <- df$Time[df$Combined == list_grf[[3]]]
  d <- (max(df$VerticalForceA)/max(df$VerticalForceC))*100
  e <- JumpLandFlight_list[[1]] - a
  f <- JumpLandFlight_list[[2]] - b
  g <- JumpLandFlight_list[[3]] - c
  
  return(list(Peaktime_a=a,Peaktime_C=b, Peaktime_total=c, Landing_Assym=d, TimetoPeak_a=e, TimetoPeak_c=f,
              TimetoPeak_total=g))
  
}





