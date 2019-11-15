

#  This code produces time series graphs of cumulative growing
#  degree days for meteorological stations in the University of
#  Arizona - Arizona Meteorological (AZMET) network.

#  A variable that helps us understand and predict the timing of 
#  plant and insect growth stages is the accumulation of heat 
#  during the year, often quantified by the sum of growing degree
#  days, that is, cumulative growing degree days (CGDDs). CGDDs are
#  calculated following the 'Temperature Averaging' method 
#  described at:
#  https://www.canr.msu.edu/news/understanding_growing_degree_days

#  Author:
#  Jeremy Weiss, Climate and Geospatial Extension Scientist
#  School of Natural Resources and the Environment
#  University of Arizona
#  520-626-8063, jlweiss@email.arizona.edu


#####  SETUP


#  Load required libraries.
library( "dplyr" )

#  Load required functions.
source( "azmet.calculate.cgdd.R" )

#  Load information about currently active AZMET stations.
stn_list <- read.csv( "azmet_station_list_active.csv",sep="," )

#  Set the AZMET station name based on currently active stations
#  in "azmet_station_list_active.csv".
stn_name <- "Bonita"

#  Set the base temperature with which to calculate GDDs. Units 
#  for base temperatures need to be in degrees C.
t_base <- 10

#  Set the day-of-year from which to start calculating CGDDs and
#  from within the same calendar year.
doy_start <- 1


#####  ACQUIRE DATA AND CALCULATE CGDDS


#  This function uses daily average temperature data from an 
#  individual AZMET station and calculates growing degree-days 
#  (GDD) and cumulative GDDs for each year.

#  Input arguments include 'stn_name', selected from 'stn_list'
#  dataframe, 't_base', the base temperature with which to 
#  calculate GDDs, and 'doy_start', the start day-of-year within 
#  the same calendar year. Units for base temperatures need to be 
#  in degrees C.

#  Download data for the specified station.
stn_data <- azmet.calculate.cgdd( stn_name,t_base,doy_start )


#####  GRAPH CGDDS


#  


#####

