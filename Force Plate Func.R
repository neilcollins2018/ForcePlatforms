library(magrittr)
library(tidyverse)

###Pasco File Read
PascoDual_read <- function(flnm) {
  read_delim(flnm, Encoding("windows-1250"), delim = "\t", skip = 1, col_names = c(
    "Time", "Force Beam 1A", "Force Beam 2A", "Force Beam 3A", "Force Beam 4A", "VerticalForce", 
    "Force Beam 1C", "Force Beam 2C", "Force Beam 3C", "Force Beam 4C", "VerticalForce")) %>%
    mutate_all(as.numeric) %>%
    select(c(1, 6, 11))
}

PascoSingle_read <- function(flnm) {
  read_delim(flnm, Encoding("windows-1250"), delim = "\t", skip = 1, col_names = c(
    "Time", "Force Beam 1A", "Force Beam 2A", "Force Beam 3A", "Force Beam 4A", "VerticalForce")) %>%
    mutate_all(as.numeric) %>%
    select(c(1, 6))
}

###Sample Rate
sample_rate_func <- function(df) {
   df2 <- df %>%
    filter(Time < 1)
  sample_rate <- nrow(df2)
  rm(df2)
  
  return(sample_rate)
}

###Separate into different force traces
IND_trace_func <- function(df) {
  
  df %<>%
    mutate(Combined = (VerticalForce + VerticalForce_1)/2)
  data_a <- df[, c(1,2)]
  data_c <- df[, c(1,3)]
  data_combined <- df[,c(1,4)]
  colnames(data_a) <- c("Time", 'VerticalForce')
  colnames(data_c) <- c("Time", 'VerticalForce')
  colnames(data_combined) <- c("Time", 'VerticalForce')
  
  return(list(data_A=data_a, data_C=data_c, data_Combined=data_combined))
  
}

###Produce Mass_kg
mass_func <- function(df){
  df %>%
    mutate(Mass = df[[2]] / 9.80665)
}

####Produce weight/mass vairables
weight_mass <- function(df) {
  df %>%
    summarise_at(vars(2,3), funs(mean))
}

###Accel to Velocity
Velocity_func <- function(time, accel) {
  cumsum(c(0,time[-1] - time[-length(time)]) * accel)  
}

###Produce force, acceleration and velocity metrics
Initial_metric <- function(df1, weight_list) {
  
  df1 %>%
    mutate(NetForce = df1[[2]] - weight_list[[1]],
           Force = df1[[2]]/weight_list[[1]],
           Accel = NetForce/weight_list[[1]],
           Velocity =  Velocity_func(Time, Accel),
           Displacement = Velocity*(1/sample_rate),
           Displacement = Displacement+lag(Displacement),
           Impulse = ((NetForce+lag(NetForce))/2)*(1/sample_rate),
           Power = Velocity*Force,
           RelativePower = Power/weight_list[[1]],
    )
}

#Produce intial metric listt
Initial_metric_list <- function(df1, df2, df3){
  
  data_a <- Initial_metric(df1, weight_massA)
  data_c <- Initial_metric(df2, weight_massC)
  data_combined <- Initial_metric(df3, weight_massCombined)
  
  return(list(data_A=data_a, data_C=data_c, data_Combined=data_combined))
  
}

###Separate Unweighting phase
unweighting_func <- function(df1, df2, df3) {
  
  a1 <- min(df1$Time[df1[[2]] < list_weights_n[[c(1, 2)]]])
  a <- df1 %>% filter(Time >= a1)
  a2 <- min(a$Time[a[[2]] > list_weights_n[[ c(1, 1) ]]])
  a %<>% filter(Time < a2)
  
  b1 <- min(df2$Time[df2[[2]] < list_weights_n[[c(2, 2)]]])
  b <- df2 %>% filter(Time >= b1)
  b2 <- min(b$Time[b[[2]] > list_weights_n[[ c(2, 1) ]]])
  b %<>% filter(Time < b2)
  
  c1 <- min(df3$Time[df3[[2]] < list_weights_n[[c(3, 2)]]])
  c <- df3 %>% filter(Time >= c1)
  c2 <- min(c$Time[c[[2]] > list_weights_n[[ c(3, 1) ]]])
  c %<>% filter(Time < c2)
  
  
  return(list(unweighting_A=a, unweighting_C=b, unweighting_combined=c))
}  
  

###Separate Braking phase
braking_func <- function(df1, df2, df3) {
  
  brake_start <- max(list_unweighting[[c(1,1)]])
  braking_a <-df1 %>%
    filter(Time >= brake_start)
  brake_end <- min(braking_a[[1]][braking_a$Velocity > 0.01])
  braking_a %<>%
    filter(Time <= brake_end)
  
  brake_start <- max(list_unweighting[[c(2,1)]])
  braking_b <-df2 %>%
    filter(Time >= brake_start)
  brake_end <- min(braking_b[[1]][braking_b$Velocity > 0.01 ])
  braking_b %<>%
    filter(Time <= brake_end)
  
  brake_start <- max(list_unweighting[[c(3,1)]])
  braking_c <-df3 %>%
    filter(Time >= brake_start)
  brake_end <- min(braking_c[[1]][braking_c$Velocity > 0.01 ])
  braking_c %<>%
    filter(Time <= brake_end)
  
  
  return(list(braking_A=braking_a, braking_A=braking_b, braking_C=braking_c))
  
}

###Separate Propulsion phase
prop_func <- function(df1, df2, df3) {
  
  prop_a <-df1 %>%
    filter(Time >= max(list_brake[[c(1,1)]]))
  prop_a %<>%
    filter(Time <= min(prop_a$Time[prop_a[[2]] <= list_weights_n[[c(1,1)]]]))
  
  prop_b <-df2 %>%
    filter(Time >= max(list_brake[[c(2,1)]]))
  prop_b %<>%
    filter(Time <= min(prop_b$Time[prop_b[[2]] <= list_weights_n[[c(2,1)]]]))

  prop_c <-df3 %>%
    filter(Time >= max(list_brake[[c(3,1)]]))
  prop_c %<>%
    filter(Time <= min(prop_c$Time[prop_c[[2]] <= list_weights_n[[c(3,1)]]]))
  
    return(list(Prop_A=prop_a, Prop_B=prop_b, Prop_C=prop_c))
}

###Define flight phase
flight_func <- function(df1, df2, df3) {
  
  flight_a <- df1 %>%
    filter(Time >= max(list_prop[[c(1,1)]]))
   flight_a %<>%
    filter(Time >= min(flight_a$Time[flight_a[[2]] <= 40]))
   flight_a %<>%
     filter(Time <= min(flight_a$Time[flight_a[[2]] >= 40]))
 
   flight_b <- df2 %>%
    filter(Time >= max(list_prop[[c(2,1)]]))
   flight_b %<>%
     filter(Time >= min(flight_b$Time[flight_b[[2]] <= 40]))
   flight_b %<>%
     filter(Time <= min(flight_b$Time[flight_b[[2]] >= 40]))

   flight_c <- df3 %>%
     filter(Time >= max(list_prop[[c(3,1)]]))
   flight_c %<>%
     filter(Time >= min(flight_c$Time[flight_c[[3]] <= 40]))
   flight_c %<>%
     filter(Time <= min(flight_c$Time[flight_c[[3]] >= 40]))
   
      return(list(Flight_A=flight_a, Flight_B=flight_b, Flight_C=flight_c))
}

##Define landing phase
landing_func <- function(df1, df2, df3) {
  
  Landing_start <- max(list_flight[[c(1,1)]])
  landing_a <-df1 %>%
    filter(Time >= Landing_start)
  
  Landing_start <- max(list_flight[[c(2,1)]])
  landing_b <-df2 %>%
    filter(Time >= Landing_start)
  
  Landing_start <- max(list_flight[[c(3,1)]])
  landing_c <-df3 %>%
    filter(Time >= Landing_start)
  

  return(list(Landing_A=landing_a, Landing_B=landing_b, Landing_C=landing_c))
  
}

###Combine related forces data
listforces_func <- function(x) {
  list(list_weighing[[x]], list_unweighting[[x]], list_brake[[x]], list_prop[[x]], list_flight[[x]],
       list_landing[[x]])
}

###Join two traces together
finaldf_func <- function(list_df) {
  
  bind_rows(list_df, .id = "Jump_Phase") %>%
    mutate(Jump_Phase = case_when(Jump_Phase == '1' ~ "Weighing",
                                  Jump_Phase == '2' ~ "Unweighting",
                                  Jump_Phase == '3' ~ "Braking",
                                  Jump_Phase == '4' ~ "Propulsion",
                                  Jump_Phase == '5' ~ 'Flight',
                                  Jump_Phase == '6' ~ "Landing"),
           Jump_Phase = factor(Jump_Phase, levels = c('Weighing', 'Unweighting',
                                                      'Braking', 'Propulsion', 
                                                      'Flight', 'Landing'))) %>%
    group_by(Jump_Phase) %>%
    mutate(
      Time_Period = cumsum(c(0, diff(Time)))
    ) %>% ungroup()
}

###Produe relevant final metrics
finalmetrics_func <- function(df) {
 df %>%
   group_by(Jump_Phase) %>%
    summarise(Phase_Time = max(Time_Period),
              Peak_grf = max(VerticalForce),
              Peak_vel = max(Velocity),
              Min_vel = min(Velocity),
              Max_vel = max(Velocity),
              Peak_power = max(Power),
              Min_power = min(Power),
              Avg_power = mean(Power),
              Impulse = sum(Impulse),
              AUC = Peak_grf*Min_vel,
              AUC2 = Peak_grf*Max_vel)
}

###Combine final dataframes
combinefinal_func <- function(df1, df2, df3) {
  df <-
    bind_rows(df1, df2, df3, .id="Sensor") %>%
    mutate(Sensor = case_when(Sensor == '1' ~ "A",
                              Sensor == '2' ~ 'C',
                              Sensor == "3" ~ "Combined"))
}

###Final Metrics
finalmetricsdf_func <- function() {
  combinefinal_func(finalmetrics_func(finaldf_func(list(list_weighing[[1]],list_unweighting[[1]], list_brake[[1]], list_prop[[1]], list_flight[[1]],list_landing[[1]]))), 
                    finalmetrics_func(finaldf_func(list(list_weighing[[2]], list_unweighting[[2]], list_brake[[2]], list_prop[[2]], list_flight[[2]],list_landing[[2]]))), 
                    finalmetrics_func(finaldf_func(list(list_weighing[[3]], list_unweighting[[3]], list_brake[[3]], list_prop[[3]], list_flight[[3]],list_landing[[3]]))))

  }

###Final Raw Metrics
finaldatadf_func <- function() {
  combinefinal_func(finaldf_func(list(list_weighing[[1]], list_unweighting[[1]], list_brake[[1]], list_prop[[1]], list_flight[[1]],list_landing[[1]])), 
                    finaldf_func(list(list_weighing[[2]], list_unweighting[[2]], list_brake[[2]], list_prop[[2]], list_flight[[2]],list_landing[[2]])), 
                    finaldf_func(list(list_weighing[[3]], list_unweighting[[3]], list_brake[[3]], list_prop[[3]], list_flight[[3]], list_landing[[3]])))
  
}
