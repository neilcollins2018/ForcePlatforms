
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
  
  g <- a/weight_mass[[4]]
  h <- b/weight_mass[[5]]
  i <- c/weight_mass[[6]]
  
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

###Separate Unweighting phase
unweighting_func <- function(df1, df2, df3) {
  
  a1 <- min(df1$Time[df1[[2]] < list_weights_n[[c(1, 2)]]])
  a <- df1 %>% filter(Time >= a1)
  a2 <- min(a$Time[a[[2]] > list_weights_n[[ c(1, 1) ]]])
  a %<>% filter(Time < a2)
  
  b1 <- min(df2$Time[df2[[2]] < list_weights_n[[c(1, 2)]]])
  b <- df2 %>% filter(Time >= b1)
  b2 <- min(b$Time[b[[2]] > list_weights_n[[ c(1, 1) ]]])
  b %<>% filter(Time < b2)
  
  c1 <- min(df3$Time[df3[[2]] < list_weights_n[[c(1, 2)]]])
  c <- df3 %>% filter(Time >= c1)
  c2 <- min(c$Time[c[[2]] > list_weights_n[[ c(1, 1) ]]])
  c %<>% filter(Time < c2)
  
  return(list(unweighting_A=a, unweighting_C=b, unweighting_total=c))
}  
  

###Separate Braking phase
braking_func <- function(df1, df2, df3) {
  
  brake_start <- max(list_unweighting[[c(1,1)]])
  braking_a <-df1 %>%
    filter(Time >= brake_start)
  braking_a$force_diff <- c(0, diff(braking_a[[2]]))
  brake_end <- min(braking_a[[1]][braking_a$force_diff < 0])
  braking_a %<>%
    filter(Time <= brake_end)
  
  brake_start <- max(list_unweighting[[c(2,1)]])
  braking_c <-df2 %>%
    filter(Time >= brake_start)
  braking_c$force_diff <- c(0, diff(braking_c[[2]]))
  brake_end <- min(braking_c[[1]][braking_c$force_diff < 0])
  braking_c %<>%
    filter(Time <= brake_end)
  
  brake_start <- max(list_unweighting[[c(3,1)]])
  braking_tot <-df3 %>%
    filter(Time >= brake_start)
  braking_tot$force_diff <- c(0, diff(braking_tot[[2]]))
  brake_end <- min(braking_tot[[1]][braking_tot$force_diff < 0])
  braking_tot %<>%
    filter(Time <= brake_end)
  
  return(list(braking_A=braking_a, braking_C=braking_c, braking_Com=braking_tot))
  
}

###Separate Propulsion phase
prop_func <- function(df1, df2, df3) {
  
  prop_a <-df1 %>%
    filter(Time >= max(list_brake[[c(1,1)]]))
  prop_a %<>%
    filter(Time <= min(prop_a$Time[prop_a$VerticalForceA <= list_weights_n[[c(1,1)]]]))
  
  prop_c <-df2 %>%
    filter(Time >= max(list_brake[[c(2,1)]]))
  prop_c %<>%
    filter(Time <= min(prop_c$Time[prop_c$VerticalForceC <= list_weights_n[[c(2,1)]]]))
  
  prop_total <-df3 %>%
    filter(Time >= max(list_brake[[c(3,1)]]))
  prop_total %<>%
    filter(Time <= min(prop_total$Time[prop_total$Combined <= list_weights_n[[c(3,1)]]]))
  
  return(list(Prop_A=prop_a, Prop_C=prop_c, Prop_Total=prop_total))
}


flight_func <- function(df1, df2, df3) {
  
  flight_a <- df1 %>%
    filter(Time >= max(list_prop[[c(1,1)]]))
   flight_a %<>%
    filter(Time >= min(flight_a$Time[flight_a[[2]] <= 40]))
   flight_a %<>%
     filter(Time <= min(flight_a$Time[flight_a[[2]] >= 40]))
 
   flight_c <- df2 %>%
    filter(Time >= max(list_prop[[c(2,1)]]))
   flight_c %<>%
     filter(Time >= min(flight_c$Time[flight_c[[2]] <= 40]))
   flight_c %<>%
     filter(Time <= min(flight_c$Time[flight_c[[2]] >= 40]))
  
   flight_tot <- df3 %>%
    filter(Time >= max(list_prop[[c(3,1)]]))
   flight_tot %<>%
     filter(Time >= min(flight_tot$Time[flight_tot[[2]] <= 40]))
   flight_tot %<>%
     filter(Time <= min(flight_tot$Time[flight_tot[[2]] >= 40]))
   
   return(list(Flight_A=flight_a, Flight_C=flight_c, Flight_Total=flight_tot))
}

landing_func <- function(df1, df2, df3) {
  
  Landing_start <- max(list_flight[[c(1,1)]])
  landing_a <-df1 %>%
    filter(Time >= Landing_start)
  
  Landing_start <- max(list_flight[[c(1,1)]])
  landing_b <-df2 %>%
    filter(Time >= Landing_start)
  
  Landing_start <- max(list_flight[[c(1,1)]])
  landing_c <-df3 %>%
    filter(Time >= Landing_start)
  
  return(list(Landing_A=landing_a, Landing_C=landing_b, Landing_Total=landing_c))
  
}

finaldf_func <- function(list_df) {
  
  bind_rows(list_df, .id = "Jump_Phase") %>%
    mutate(Jump_Phase = case_when(Jump_Phase == '1' ~ "weighing",
                                  Jump_Phase == '2' ~ "unweighting",
                                  Jump_Phase == '3' ~ "braking",
                                  Jump_Phase == '4' ~ "propulsion",
                                  Jump_Phase == '5' ~ 'flight',
                                  Jump_Phase == '6' ~ "landing"),
           Jump_Phase = factor(Jump_Phase, levels = c('weighing', 'unweighting',
                                                      'braking', 'propulsion', 'flight', 'landing'))) %>%
    group_by(Jump_Phase) %>%
    mutate(
      Time_Period = cumsum(c(0, diff(Time)))
    ) %>% ungroup()
  

}







