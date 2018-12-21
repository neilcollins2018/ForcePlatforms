###Pasco File Read
Pasco_read <- function(flnm) {
  read_delim(flnm, Encoding("windows-1250"), delim = "\t", skip = 1, col_names = c(
    "Time", "Force Beam 1A", "Force Beam 2A", "Force Beam 3A", "Force Beam 4A", "VerticalForce", 
    "Force Beam 1C", "Force Beam 2C", "Force Beam 3C", "Force Beam 4C", "VerticalForce")) %>%
    mutate_all(as.numeric) %>%
    select(c(1, 6, 11))
}

###Sample Rate
sample_rate_func <- function(df) {
   df2 <- df %>%
    filter(Time < 1)
  sample_rate <- nrow(df2)
  rm(df2)
  
  return(sample_rate)
}

###Produce Mass_kg
mass_func <- function(df){
  df %>%
    mutate(Mass = df[[2]] / 9.80665)
}

####Produce weight/ass vairables
weight_mass <- function(df) {
  df %>%
    summarise_at(vars(2,3), funs(mean))
}

###Accel to Velocity
Velocity_func <- function(time, accel) {
  cumsum(c(0,time[-1] - time[-length(time)]) * accel)  
}

###Produce force, acceleration and velocity metrics
Initial_metric <- function(df1, df2, weight_list) {
  
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
           Symetry = df1[[2]]/df2[[2]]
    )
}  

###Separate Unweighting phase
unweighting_func <- function(df1, df2) {
  
  a1 <- min(df1$Time[df1[[2]] < list_weights_n[[c(1, 2)]]])
  a <- df1 %>% filter(Time >= a1)
  a2 <- min(a$Time[a[[2]] > list_weights_n[[ c(1, 1) ]]])
  a %<>% filter(Time < a2)
  
  b1 <- min(df2$Time[df2[[2]] < list_weights_n[[c(1, 2)]]])
  b <- df2 %>% filter(Time >= b1)
  b2 <- min(b$Time[b[[2]] > list_weights_n[[ c(1, 1) ]]])
  b %<>% filter(Time < b2)
  
  return(list(unweighting_A=a, unweighting_C=b))
}  
  

###Separate Braking phase
braking_func <- function(df1, df2) {
  
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
  
  return(list(braking_A=braking_a, braking_C=braking_c))
  
}

###Separate Propulsion phase
prop_func <- function(df1, df2) {
  
  prop_a <-df1 %>%
    filter(Time >= max(list_brake[[c(1,1)]]))
  prop_a %<>%
    filter(Time <= min(prop_a$Time[prop_a$VerticalForce <= list_weights_n[[c(1,1)]]]))
  
  prop_c <-df2 %>%
    filter(Time >= max(list_brake[[c(2,1)]]))
  prop_c %<>%
    filter(Time <= min(prop_c$Time[prop_c$VerticalForce <= list_weights_n[[c(2,1)]]]))

  return(list(Prop_A=prop_a, Prop_C=prop_c))
}


flight_func <- function(df1, df2) {
  
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

   return(list(Flight_A=flight_a, Flight_C=flight_c))
}

landing_func <- function(df1, df2) {
  
  Landing_start <- max(list_flight[[c(1,1)]])
  landing_a <-df1 %>%
    filter(Time >= Landing_start)
  
  Landing_start <- max(list_flight[[c(1,1)]])
  landing_b <-df2 %>%
    filter(Time >= Landing_start)

  return(list(Landing_A=landing_a, Landing_C=landing_b))
  
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
              AUC2 = Peak_grf*Max_vel,
              Max_symetry = max(Symetry),
              Min_symetry = min(Symetry))
              
  
}
