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
list_unweighting <- unweighting_func(data_A, data_C, data_Combined)

###
###Braking Phase
###
list_brake <- braking_func(data_A, data_C, data_Combined)

###
###Propulsion Phase
###
list_prop <- prop_func(data_A, data_C, data_Combined)
nrow_a <- nrow(list_prop[[1]])
nrow_c <- nrow(list_prop[[2]])
nrow_total <- nrow(list_prop[[3]])

###
###Flight Phase
###
list_flight <- flight_func(data_A, data_C, data_Combined)

##Jump Height from flight time ((ft/2)^2)*9.80665*0.5)
jumpheight_df <-data_frame(jumpheight_meters=c(
jumpheightFL_a <- (((max(list_flight[[1]][1])-min(list_flight[[1]][1]))/2)^2)*9.80665*0.5,
jumpheightFL_c <- (((max(list_flight[[2]][1])-min(list_flight[[2]][1]))/2)^2)*9.80665*0.5,
jumpheightFL_total <- (((max(list_flight[[3]][1])-min(list_flight[[3]][1]))/2)^2)*9.80665*0.5))

##Jump height from takeoff vel TOVel^2/2*9.8066,
jumpheightTOV_a <- ((list_prop[[1]][[c(7,nrow_a)]])^2)/(9.80665*2)

###
###Landing Phase
###
list_landing <- landing_func(data_A, data_C, data_Combined)

#######Seperate into lists of forces/Not phases
list_A <- list(list_weighing[[1]], list_unweighting[[1]], list_brake[[1]], list_prop[[1]], list_flight[[1]],
               list_landing[[1]])
list_C <- list(list_weighing[[2]], list_unweighting[[2]], list_brake[[2]], list_prop[[2]], list_flight[[2]],
               list_landing[[2]])
list_total <- list(list_weighing[[3]], list_unweighting[[3]], list_brake[[3]], list_prop[[3]], list_flight[[3]],
               list_landing[[3]])

###Separate df per force trace
df_forceA <- finaldf_func(list_A)
df_forceC <- finaldf_func(list_C)
df_forceTotal <- finaldf_func(list_total)


###Plots
ggplot(df_forceA, aes(x=Time, y=VerticalForceA))+
  geom_line()+
  theme_bw()+
  facet_wrap(.~Jump_Phase, scales = "free_x")

ggplot(df_forceC, aes(x=Time, y=VerticalForceC))+
  geom_line()+
  theme_bw()+
  facet_wrap(.~Jump_Phase, scales = "free_x")

ggplot(df_forceTotal, aes(x=Time, y=Combined))+
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

C <- df_forceTotal %>%
  filter(Jump_Phase == "braking") %>%
  select(2,3) %>%
  ggplot(aes(Time, Combined))+
  geom_line() +
  ylim(250,2500)+
  theme_bw()+
  stat_smooth_func(geom="text",method="lm",hjust=0, vjust=4,parse=TRUE) +
  stat_smooth(method = "lm", alpha=.2)


plot_grid(A, B, C, ncol = 3, labels = c("Left", "Right", "Combined"))

data_A$force_diff <- c(0, diff(data_A$VerticalForceA))

timefil <- data_A$Time[data_A$force_diff == max]

max <- max(data_A$force_diff)


f <- data_A %>%
  filter(Time < timefil-.02) %>%
  ggplot(aes(Time, VerticalForceA, colour="Force"))+
  geom_line()+
  # geom_vline(xintercept = 3.28)+
  # geom_vline(xintercept = 3.394)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 2.66)+
  geom_vline(xintercept = 3.05)+
  geom_vline(xintercept = 3.28)+
  geom_line(aes(x=Time, y=force_diff*50, colour='Force_diff'))+
  geom_line(aes(x=Time, y=Velocity_A*100, colour='Vel'))


session_info()
