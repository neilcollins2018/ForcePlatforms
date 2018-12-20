library(tidyverse)
library(magrittr)
library(zoo)
library(data.table)
library(tidyr)
library(tictoc)

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
weight_mass <- weight_mass(data)

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

###Weighting phase
weighing <- data%>%
  filter(row_number() < list_rsi[[1]])

###Unweighting phase
unweighting <- start_peak%>%
  filter(VerticalForceA < weight_mass[[4]])
  
###Braking Phase
braking <- start_peak%>%
  filter(VerticalForceA > weight_mass[[4]])

###Propulsive Phase
PeakGRFtime <-data$Time[data$VerticalForceA == list_grf[[1]]]
propulsive <- data %>%
  filter(Time < JumpLandFlight_list[[1]] & Time > PeakGRFtime)

###Flight Phase
flight <- data%>%
  filter(Time > JumpLandFlight_list[[1]] & Time < JumpLandFlight_list[[4]])

timetest <- min(start_peak$Time[start_peak$VerticalForceA > weight_mass[[4]]])
Test1 <- data %>%
  filter(between(row_number(), 1, 300))
a <- as.integer(weight_mass[[4]] - (5*(sd(Test1$VerticalForceA))))
a <- min(which(data2$VerticalForceA <=a))

timetest2 <- min(start_peak$Time[start_peak$VerticalForceA < a])


data_phase <- data %>%
  mutate(phase = case_when(Time <= timetest2-0.3 ~ "Weighing",
                           Time > timetest2-0.3 & Time <= timetest ~ "Unweighting",
                           Time > timetest & Time <= PeakGRFtime ~ 'Braking',
                           Time > PeakGRFtime & Time <= JumpLandFlight_list[[1]] ~ 'Propulsive',
                           Time > JumpLandFlight_list[[1]] & Time <= JumpLandFlight_list[[4]] ~ "Flight",
                           Time > JumpLandFlight_list[[4]] ~ "Landing"),
         phase = factor(phase, levels = c('Weighing', 'Unweighting', 'Braking', 'Propulsive', 'Flight', 'Landing'))) 

ggplot(data_phase, aes(Time, VerticalForceC))+
  geom_line() +
  facet_wrap(.~phase, scales = 'free_x') +
  theme_bw()

ggplot(data, aes(Time, VerticalForceC))+
  geom_line()+
  theme_bw()

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

ggplot(data2, aes(x=Time, y=VerticalForceA))+
  geom_line()+
  geom_line(aes(x=Time, y=Velocity_A*100))+
  theme_minimal()+
  geom_vline(aes(xintercept = JumpLandFlight_list[[1]],colour='TakeOff'), colour='green') +
  geom_vline(aes(xintercept = JumpLandFlight_list[[4]], colour='Land'), colour='green') +
  geom_vline(aes(xintercept = ecc_time, colour='EccDecelerationStart'), colour='blue') +
  geom_vline(aes(xintercept = ecc_end, colour='EccDecelerationEnd'), colour='blue') +
  geom_vline(aes(xintercept = landing_list[[1]], colour='EccEnd'), colour='red') +
  geom_segment(aes(y=3500, x=JumpLandFlight_list[[4]], xend=JumpLandFlight_list[[1]], yend=3500))+
  annotate('text', x=3.8, label='Flight', y=3600, hjust=1, colour = "black", alpha=0.8, size=3, fontface="bold")+
  geom_segment(aes(y=3000, x=ecc_time, xend=landing_list[[1]], yend=3000))+
  annotate('text', x=3, label='EccDecel', y=3100, hjust=1, colour = "black", alpha=0.8, size=3, fontface="bold")+
  geom_segment(aes(y=3250, x=ecc_end, xend=landing_list[[1]], yend=3250))+
  annotate('text', x=3.18, label='EccAccel', y=3350, hjust=1, colour = "black", alpha=0.8, size=3, fontface="bold")
  

