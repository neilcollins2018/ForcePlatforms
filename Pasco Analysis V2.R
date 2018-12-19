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
rm(data_sample)

##Produce weights(N), massess (Kg)
weight_mass <- weight_mass(data)

###Produce force, impulse, power, relative values, 
data2 <- Initial_metric(data)


###Seperate into different force sources
data_A <- data2[, c(1,2,4, 8, 11, 14, 17, 20, 23, 26, 29)]
data_C <- data2[, c(1,3,5, 9, 12, 15, 18, 21, 24, 27, 30)]
data_Combined <- data2[, c(1,6,7, 10, 13, 16, 19, 22, 25, 28, 31)]

data_list <- list(data_A, data_C, data_Combined)


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
list_unweighting <- unweighting(data_A, data_C, data_Combined)

###
###Braking Phase
###
list_brake <- braking_func(data_A, data_C, data_Combined)

list_brake[[1]] %>%
  filter(Time < 3.8) %>%
  ggplot(aes(x=Time, y=VerticalForceA))+
  geom_line() +
  theme_bw()

###
###Propulsion Phase
###

list_prop <- prop_func(data_A, data_C, data_Combined)


###
###Flight Phase
###
flight_start <- max(list_prop[[c(1,1)]])
braking_a <-data_A %>%
  filter(Time >= brake_start)

###
###Landing Phase
###
brake_start <- max(list_unweighting[[c(3,1)]])
braking_a <-data_Combined %>%
  filter(Time >= brake_start)
braking_a$force_diff <- c(0, diff(braking_a$Combined))
brake_end <- min(braking_a$Time[braking_a$force_diff < 0])
braking_a2 <-braking_a %>%
  filter(Time <= brake_end)


braking_a %>%
  filter(Time < 3.8) %>%
  ggplot(aes(x=Time, y=vel_diff*10, colour="force_diff"))+
  geom_line() +
  geom_line(aes(Time, Combined, colour="VertForce")) +
  theme_bw()
  
ggplot(braking_a2, aes(x=Time, y=Combined))+
  geom_line()
  

