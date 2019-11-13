

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
source( "azmet.data.download.R" )

#  Load information about currently active AZMET stations.
stn_list <- read.csv( "azmet_station_list_active.csv",sep="," )

#  Set the AZMET station name based on currently active stations
#  in "azmet_station_list_active.csv".
stn_name <- "Bonita"


#####  ACQUIRE DATA


#  Download data for the specified station.
stn_data <- azmet.data.download( stn_name )



#####

