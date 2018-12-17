source('Force Plate Func.R')

library(data.table)
library(tidyverse)
library(magrittr)
options(scipen = 999, digits=5)

###File Read
data <- Pasco_read("CMJ_Run02.txt")

##Calculate sample rate
data_sample <- data %>%
  filter(Time < 1)
sample_rate <- nrow(data_sample)

##Produce weights(N), massess (Kg)
weight_massLIST <- weight_mass(data)

###Produce force, impulse, power, relative values, 
data2 <- Initial_metric(data)

###Takeoff, landing, jump times & jump heights
JumpLandFlight_list <- takeoff_land_times(data2)

###Remove data 1.5sec after landing
data2 %<>%
  filter(Time < JumpLandFlight_list[[6]]+1.5)

###RSI Data & row numbers for start of initial descent to takeoff
list_rsi <- row_starts(data2)

###GRF  & relative GRF
list_grf <- grf_data(data2)

###Time to peak force, landing assym
landing_list <- Peak_landasym(data2)

Ecc_End_peak <- data$Time[Data$]

start_peak <- data2%>%
  filter(row_number() > list_rsi[[1]] & Time < landing_list[[1]])

start_peak$VerticalForceA_diff <- c(0, diff(start_peak$VerticalForceA)) 
start_peak$VerticalForceC_diff <- c(0, diff(start_peak$VerticalForceC)) 
start_peak$Combined_diff <- c(0, diff(start_peak$Combined)) 

###Ecc A
ecc_start_a <- min(which(start_peak$VerticalForceA_diff >0))
Min_disA <- min(start_peak$DisplacementA)
MinDissA_row <- min(which(start_peak$DisplacementA == Min_disA))
A_ecc_only <- start_peak %>%
  filter(between(dplyr::row_number(), ecc_start_a, MinDissA_row))

ecc_time <- min(A_ecc_only$Time)
ecc_end <- max(A_ecc_only$Time)

PeakForce_ecc_A <- max(A_ecc_only$VerticalForceA)
MeanForce_ecc_A <- mean(A_ecc_only$VerticalForceA)
PeakVel_ecc_A <- max(A_ecc_only$Velocity_A)
MeanVel_ecc_A <- mean(A_ecc_only$Velocity_A)
PeakPower_ecc_A <- max(A_ecc_only$PowerA)
MeanPower_ecc_A <- mean(A_ecc_only$PowerA)
PeakRelPower_ecc_A <- max(A_ecc_only$RelativePowerA)
MeanRelPower_ecc_A <- mean(A_ecc_only$RelativePowerA)

###Ecc C
ecc_start_c <- min(which(start_peak$VerticalForceC_diff >0))
Min_disC <- min(start_peak$DisplacementC)
MinDissC_row <- min(which(start_peak$DisplacementC == Min_disC))
C_ecc_only <- start_peak %>%
  filter(between(dplyr::row_number(), ecc_start_c, MinDissC_row))

PeakForce_ecc_C <- max(C_ecc_only$VerticalForceC)
MeanForce_ecc_C <- mean(C_ecc_only$VerticalForceC)
PeakVel_ecc_C <- min(C_ecc_only$Velocity_C)
MeanVel_ecc_C <- mean(C_ecc_only$Velocity_C)
PeakPower_ecc_C <- min(C_ecc_only$PowerC)
MeanPower_ecc_C <- mean(C_ecc_only$PowerC)
PeakRelPower_ecc_C <- min(C_ecc_only$RelativePowerC)
MeanRelPower_ecc_C <- mean(C_ecc_only$RelativePowerC)


###Ecc Combined
ecc_start_total <- min(which(start_peak$VerticalForceA_diff >0))
Min_disTotal <- min(start_peak$DisplacementTotal)
MinDissTotal_row <- min(which(start_peak$DisplacementTotal == Min_disTotal))
Total_ecc_only <- start_peak %>%
  filter(between(dplyr::row_number(), ecc_start_total, MinDissTotal_row))

PeakForce_ecc_total <- max(Total_ecc_only$Combined)
MeanForce_ecc_total <- mean(Total_ecc_only$Combined)
PeakVel_ecc_total <- min(Total_ecc_only$Velocity_total)
MeanVel_ecc_total <- mean(Total_ecc_only$Velocity_total)
PeakPower_ecc_total <- min(Total_ecc_only$Combined)
MeanPower_ecc_total <- mean(Total_ecc_only$Combined)
RelPower_ecc_total <- min(Total_ecc_only$RelativePowerTotal)
MeanRelPower_ecc_total <- mean(Total_ecc_only$RelativePowerTotal)

data$VerticalForceA

  ggplot(data, aes(x=Time, y=VerticalForceA))+
  geom_line()+
  theme_minimal()+
  geom_vline(aes(xintercept = JumpLandFlight_list[[1]],colour='TakeOff')) +
  geom_vline(aes(xintercept = JumpLandFlight_list[[4]], colour='Land')) +
  geom_vline(aes(xintercept = starttime_a, colour='JumpStart/EccStart'))+
  geom_vline(aes(xintercept = ecc_time, colour='EccDecelerationStart')) +
  geom_vline(aes(xintercept = ecc_end, colour='EccDecelerationEnd')) +
  geom_vline(aes(xintercept = landing_list[[1]], colour='EccEnd')) +
  geom_segment(aes(y=4000, x=starttime_a, xend=landing_list[[1]], yend=4000))+
  annotate('text', x=3.2, label='Eccentric', y=4100, hjust=1, colour = "black", alpha=0.8, size=3, fontface="bold") +
  geom_segment(aes(y=3500, x=JumpLandFlight_list[[4]], xend=JumpLandFlight_list[[1]], yend=3500))+
  annotate('text', x=3.8, label='Flight', y=3600, hjust=1, colour = "black", alpha=0.8, size=3, fontface="bold")+
  geom_segment(aes(y=3000, x=ecc_time, xend=ecc_end, yend=3000))+
  annotate('text', x=3, label='EccDecel', y=3100, hjust=1, colour = "black", alpha=0.8, size=3, fontface="bold")


  



?geom_vline
  
ecc_time

