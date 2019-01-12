###################################################################################
RequiredPackages <- c("pacman")                                                   #
for (i in RequiredPackages) { #Installs packages if not yet installed             #
  if (!require(i, character.only = TRUE)) install.packages(i)                     #
}                                                                                 #
pacman::p_load("tidyverse","magrittr", 'zoo', 'data.table', 'tidyr', 'devtools',  # 
               "openxlsx" )                                                       #
###########Above can be deleted following first error free analysis performed######

##Load functions 
devtools::source_gist('b966daa2539e10c53a3fc8688d66b819', quiet = T)

###File Read
data <- Pasco_read(file=file.choose())

##Calculate sample rate
sample_rate <- sample_rate(data)

###Separate into different force traces
list_dataDF <- IND_force_df(data)

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
list_weighing <- lapply(data_list, function(x){x %<>% filter(Time < 1.5 & Time > 0.5);x})

####Weights(N)/Masses (Kg)
list_weights_masses <- weightmass_list(list_weighing)

###
###Unweighting/Braking/Propulsion/Flight/Landing Phase
list_A <- phase_split(data_list[[1]])
list_C <- phase_split(data_list[[2]])
list_combined <- phase_split(data_list[[3]])


###Combine Summary Metrics & raw data divided into phases
finalmetric_df <- finalmetricsdf()
full_data <- finaldata_raw()

###Overall Summary metrics
result <- summary_metrics()

#Single Trace Plot
full_data %>% filter(Sensor == "A") %>% plot_phasetrace(.)
full_data %>% filter(Sensor == "C") %>% plot_phasetrace(.)
full_data %>% filter(Sensor == "Combined") %>% plot_phasetrace(.)

#All traces Plot
plot_phasetrace(full_data)

###View Files
View(result)
View(finalmetric_df)
View(full_data)

###Create and open excel
excel_create()

##Tidy Up 
rm(list=ls(pattern = "list|weight|i"))
