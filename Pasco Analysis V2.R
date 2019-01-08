library(tidyverse)
library(magrittr)
library(zoo)
library(data.table)
library(tidyr)

devtools::source_gist('b966daa2539e10c53a3fc8688d66b819')

###File Read
data <- PascoDual_read('./1khz/ExportData_3_Run02_CMJ.TXT')

##Calculate sample rate
sample_rate <- sample_rate_func(data)

###Separate into different force traces
list_dataDF <- IND_trace_func(data)

###Calculate Mass
data_A <- mass_func(list_dataDF[[1]])
data_C <- mass_func(list_dataDF[[2]])
data_combined <- mass_func(list_dataDF[[3]])

##Produce weights(N), massess (Kg)
weight_massA <- weight_mass(data_A)
weight_massC <- weight_mass(data_C)
weight_massCombined <- weight_mass(data_combined)

###Produce force, impulse, power, relative values & list of df 
data_list <- Initial_metric_list(data_A, data_C, data_combined)

###
###Weighing Phase
###
list_weighing <- lapply(data_list, function(x){x %<>% filter(Time < 1.5 & Time > 0.5);x})

####Weights(N)/Masses (Kg)
list_weights_n <- lapply(list_weighing, function(x){x %<>% summarise(weight_n = mean(x[[2]]),
                                                                     unweight_onset_N = (weight_n -5*sd(x[[2]])));x})
list_mass_kg <- lapply(list_weighing, function(x){x %<>% summarise(mass_kg = mean(x[[3]]),
                                                                   unweight_onset_KG = (mass_kg - 5*sd(x[[3]])));x})

###
###Unweighting Phase
###
list_unweighting <- unweighting_func(data_list[[1]], data_list[[2]], data_list[[3]])

###
###Braking Phase
###
list_brake <- braking_func(data_list[[1]], data_list[[2]], data_list[[3]])

###
###Propulsion Phase
###
list_prop <- prop_func(data_list[[1]], data_list[[2]], data_list[[3]])

###
###Flight Phase
###
list_flight <- flight_func(data_list[[1]], data_list[[2]], data_list[[3]])

###
###Landing Phase
###
list_landing <- landing_func(data_list[[1]], data_list[[2]], data_list[[3]])

###
###Final Wrangling
#######Seperate into lists of forces/Not phases
list_A <- listforces_func(1)
list_C <- listforces_func(2)
list_combined <- listforces_func(3)

###Combine Summary Metrics & raw data
finalmetric_df <- finalmetricsdf_func()
full_data <- finaldatadf_func()

####Plots
#All traces
ggplot(full_data, aes(x=Time, y=VerticalForce, colour=Sensor))+
  geom_line()+
  theme_bw()+
  facet_wrap(.~Jump_Phase, scales = "free_x")


MassKG_A <- weight_massA[[2]]
MassKG_C <- weight_massC[[2]]
WeightN_A <- weight_massA[[1]]
WeightN_C <- weight_massC[[1]]
FlightTime_A <- max(list_flight[[1]][1]) - min(list_flight[[1]][1])
FlightTime_C <- max(list_flight[[2]][1]) - min(list_flight[[2]][1])
TimeAtTakeOff_A <- min(list_flight[[1]][1])
TimeAtTakeOff_C <- min(list_flight[[2]][1])
TimeAtLanding_A <- min(list_landing[[1]][1])
TimeAtLanding_C <- min(list_landing[[2]][1])
JumpHeightA <- ((FlightTime_A/2)^2)*9.80665*.5
JumpHeightC <- ((FlightTime_C/2)^2)*9.80665*.5
PeakGRF_A <- max(list_brake[[1]][2])
PeakGRF_C <- max(list_brake[[2]][2])
TimeOfPeakGRF_A <- list_brake[[1]] %>%
  filter(list_brake[[1]][2] == PeakGRF_A) %>%
  select(1)
TimeOfPeakGRF_C <- list_brake[[2]] %>%
  filter(list_brake[[2]][2] == PeakGRF_C) %>%
  select(1)
TimeToPeak_A <- TimeOfPeakGRF_A - min(list_unweighting[[1]][1])
TimeToPeak_A <- TimeOfPeakGRF_C - min(list_unweighting[[2]][1])

rm(list=ls(pattern="list"))
