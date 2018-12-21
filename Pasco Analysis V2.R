library(tidyverse)
library(magrittr)
library(zoo)
library(data.table)
library(tidyr)
library(devtools)
library(cowplot)
source_gist("524eade46135f6348140")

source('Force Plate Func.R')
options(scipen = 999, digits=5)

###File Read
data <- Pasco_read("CMJ_Run02.txt")

##Calculate sample rate
sample_rate <- sample_rate_func(data)

###Separate into different force traces
data_A <- data[, c(1,2)]
data_C <- data[, c(1,3)]
colnames(data_C) <- c("Time", 'VerticalForce')

###Calculate Mass
data_A <- mass_func(data_A)
data_C <- mass_func(data_C)

##Produce weights(N), massess (Kg)
weight_massA <- weight_mass(data_A)
weight_massC <- weight_mass(data_C)

###Produce force, impulse, power, relative values & list of df 
data_A <- Initial_metric(data_A, data_C, weight_massA)
data_C <- Initial_metric(data_C, data_A, weight_massC)
data_list <- list(data_A, data_C)

###
###Weighing Phase
###
list_weighing <- lapply(data_list, function(x){x %<>% filter(Time < 1);x})

####Weights(N)/Masses (Kg)
list_weights_n <- lapply(list_weighing, function(x){x %<>% summarise(weight_n = mean(x[[2]]),
                                                                     unweight_onset_N = (weight_n -5*sd(x[[2]])));x})
list_mass_kg <- lapply(list_weighing, function(x){x %<>% summarise(mass_kg = mean(x[[3]]),
                                                                   unweight_onset_KG = (mass_kg - 5*sd(x[[3]])));x})

###
###Unweighting Phase
###
list_unweighting <- unweighting_func(data_A, data_C)

###
###Braking Phase
###
list_brake <- braking_func(data_A, data_C)

###
###Propulsion Phase
###
list_prop <- prop_func(data_A, data_C)
nrow_a <- nrow(list_prop[[1]])
nrow_c <- nrow(list_prop[[2]])

###
###Flight Phase
###
list_flight <- flight_func(data_A, data_C)

###
###Landing Phase
###
list_landing <- landing_func(data_A, data_C)

###Final Wrangling
#######Seperate into lists of forces/Not phases
list_A <- list(list_weighing[[1]], list_unweighting[[1]], list_brake[[1]], list_prop[[1]], list_flight[[1]],
               list_landing[[1]])
list_C <- list(list_weighing[[2]], list_unweighting[[2]], list_brake[[2]], list_prop[[2]], list_flight[[2]],
               list_landing[[2]])

###Separate df per force trace
df_forceA <- finaldf_func(list_A)
df_forceC <- finaldf_func(list_C)

###Summary Metrics
df_forceAfinal <- finalmetrics_func(df_forceA)
df_forceCfinal <- finalmetrics_func(df_forceC)


###Combine Summary Metrics
finalmetric_df <- bind_rows(df_forceAfinal, df_forceCfinal, .id="Sensor") %>%
  mutate(Sensor = case_when(Sensor == '1' ~ "A",
                            Sensor == '2' ~ 'C'))



MassKG_A <- weight_massA[[2]]
MassKG_C <- weight_massC[[2]]
WeightN_A <- weight_massA[[1]]
WeightN_C <- weight_massC[[1]]
FlightTime_A <- max(list_flight[[1]][1]) - min(list_flight[[1]][1])
FlightTime_C <- max(list_flight[[2]][1]) - min(list_flight[[2]][1])
TimeAtTO_A <- min(list_flight[[1]][1])
TimeAtTO_C <- min(list_flight[[2]][1])
TimeAtLanding_A <- min(list_landing[[1]][1])
TimeAtLanding_C <- min(list_landing[[2]][1])
JumpHeightA <- ((FlightTime_A/2)^2)*9.80665*.5
JumpHeightC <- ((FlightTime_C/2)^2)*9.80665*.5
PeakGRF_A <- max(list_brake[[1]][2])
PeakGRF_C <- max(list_brake[[2]][2])
TimeOfPeak_A <- list_brake[[1]] %>%
  filter(list_brake[[1]][2] == PeakGRF_A) %>%
  select(1)
TimeOfPeak_C <- list_brake[[2]] %>%
  filter(list_brake[[2]][2] == PeakGRF_C) %>%
  select(1)
TimeToPeak_A <- TimeOfPeak_A - min(list_unweighting[[1]][1])
TimeToPeak_A <- TimeOfPeak_C - min(list_unweighting[[2]][1])


full_data <- bind_rows(df_forceA, df_forceC, .id='sensor')

ggplot(full_data, aes(x=Time, y=VerticalForce, colour=sensor))+
  geom_line()+
  theme_bw()+
  facet_wrap(.~Jump_Phase, scales = "free_x")



TimeOfPeak <- list_brake[[1]] %>%
  filter(list_brake[[1]][2] == PeakGRF_A) %>%
  select(1)
list_brake[[1]][1][list_brake[[1]][2] == PeakGRF_A]
  
###Plots
ggplot(df_forceA, aes(x=Time, y=VerticalForce))+
  geom_line()+
  theme_bw()+
  facet_wrap(.~Jump_Phase, scales = "free_x")

ggplot(df_forceC, aes(x=Time, y=VerticalForce))+
  geom_line()+
  theme_bw()+
  facet_wrap(.~Jump_Phase, scales = "free_x")


A <-df_forceA %>%
  filter(Jump_Phase == "braking") %>%
  select(2,3) %>%
  ggplot(aes(Time, VerticalForceA))+
  geom_line() +
  ylim(250,2500)+
  theme_bw()+
  stat_smooth_func(geom="text",method="lm",hjust=0, vjust=4,parse=TRUE) +
  stat_smooth(method = "lm", alpha=.2)

B <- df_forceC %>%
  filter(Jump_Phase == "braking") %>%
  select(2,3) %>%
  ggplot(aes(Time, VerticalForceC))+
  geom_line() +
  theme_bw()+
  ylim(250,2500)+
  stat_smooth_func(geom="text",method="lm",hjust=0, vjust=4,parse=TRUE) +
  stat_smooth(method = "lm", alpha=.2)

plot_grid(A, B, ncol = 2, labels = c("Left", "Right"))

session_info()
