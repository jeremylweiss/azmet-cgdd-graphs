##################################################################
##  EXPORT CGDD VIZ BASED ON AZMET STATION DATA TO WEB
##################################################################


#  Authors: 
#  Jeremy Weiss, Climate and Geospatial Extension Scientist
#  School of Natural Resources and the Environment
#  University of Arizona
#  520-626-8063, jlweiss@email.arizona.edu

#  This code generates and exports visualizations of growing degree-day
#  (GDD) data from individual active AZMET stations. Visualizations
#  are exported to the climateGEM plotly account.

#  Input arguments include variables 'Tbase' and 'doy_start'. These
#  are initially set to '10' and '1', respectively. Change as
#  need be.

#  In the code, the 'AZMET_station_list_active.csv' file is called.
#  Ensure that this file is present and that this code is looking
#  for it in the right location. See Section A.

#  In the code, the 'azmet.download.data.R' function is called.
#  Ensure that this file is present and that this code is looking
#  for it in the right location. See Section B1.

#  In the code, the 'azmet.calcluate.gdd.R' function is called.
#  Ensure that this file is present and that this code is looking
#  for it in the right location. See Section B2.

#  In the code, the 'azmet.viz.gdd.range.R' function is called.
#  Ensure that this file is present and that this code is looking
#  for it in the right location. See Section B3.

#  In the code, the 'azmet.viz.gdd.trace.R' function is called.
#  Ensure that this file is present and that this code is looking
#  for it in the right location. See Section B3.


##################################################################
##  A. SET THE WORKING ENVIRONMENT AND GLOBAL VARIABLES
##################################################################


#  Make the contents of needed packages available for use in the
#  current R session. This code assumes that these needed 
#  packages are already installed. Note that the 'dplyr' package
#  is called in the 'azmet.download.data.R' function. That code
#  also assumes that this package is already installed. Note that
#  loading of the 'plotly' package also loads the 'ggplot2' package.
#  We are loading these packages now in order to not duplicate the
#  loading within the 'azmet.viz.gdd.range.R' and 'azmet.viz.gdd.trace.R'
#  functions.
require( dplyr )
require( reshape2 )
require( plotly )

#  Set the base temperature (in degrees C) that will be used in
#  the GDD calculations.
Tbase <- 10

#  Set the day-of-year start that will be used in the GDD 
#  calculations.
doy_start <- 1

#  Load the 'AZMET_station_list_active.csv' file. This file 
#  contains a list of station names, station numbers, start years,
#  and end years. We will use this for cycling through individual
#  stations for the GDD calculations, and for determining the years
#  for which data exist for each station, which we will use in the.
AZMET_stn_list <- read.csv( "~/AZMET_station_list_active.csv",sep="," )

#  We will export GDD graphics to the climateGEM plot.ly account,
#  the location where the climatGEM website looks for these graphics.
#  In order to export as such, open the plotly account connection.
Sys.setenv( "plotly_username"="" )
Sys.setenv( "plotly_api_key"="" )
#py <- plot_ly( username="",key="" )


##################################################################
##  B. START CYCLING THROUGH THE INDIVIDUAL AZMET STATIONS
##################################################################


for( i in 1:nrow( AZMET_stn_list ) ) {
  stn_name <- AZMET_stn_list$stn[ i ]
  
  
  ################################################################
  ##  B1. DOWNLOAD AND FORMAT DAILY AZMET DATA
  ################################################################

  
  #  Download the most recent AZMET data for the chosen station
  #  and load it as a dataframe into the current R environment. The
  #  'azmet.download.data.R' function takes the variable 'stn_name',
  #  and returns the variable 'stn_data'.
  source( '~/azmet.download.data.R' )
  stn_data <- azmet.download.data( stn_name )
  
  
  ################################################################
  ##  B2. CALCULATE GDD FROM DAILY AZMET DATA
  ################################################################
  
  
  #  Dataframe columns of interest in the variable 'stn_data' passed
  #  from the function 'azmet.download.data.R' include year (col 1),
  #  month(col 2), day (col 3), doy (col 4), stn_no (col 5), and 
  #  Tmean (col 8). Select these columns from 'stn_data'.
  stn_data <- select( stn_data,year,month,day,doy,stn_no,Tmean )
  
  #  Extract the row of information (station name, station number,
  #  start year, and end year) tied to the selected AZMET station.
  stn_info <- subset( AZMET_stn_list,stn==stn_name )
  
  #  The start doy of the start year for AZMET stations varies.
  #  Thus, there may be instances in which GDD calculations for an
  #  individual year are called to start before station data exist.
  #  This can result in cummulative GDD values for an individual
  #  year that are nonsense. Using the 'doy_start' value, check the
  #  the first year of 'stn_data' to see if its 'doy' is before or
  #  after the designated 'doy_start'. If 'doy' is earlier than 
  #  'doy_start', the current 'stn_data' is fine. If 'doy' is later
  #  than 'doy_start', remove the first year of data from 'stn_data'.
  if ( stn_data$doy[ 1 ] > doy_start) {
    stn_data <- filter( stn_data,stn_data$year > stn_info$start_yr )
    stn_info$start_yr <- stn_info$start_yr + 1
  }
  
  #  Set the range of years for which data are available for the
  #  selected station.
  stn_yrs <- as.integer( select( stn_info,start_yr ) ):as.integer( select( stn_info,end_yr ) )
  
  #  Set the current year, based on the 'stn_yrs' values.
  curyr <- max( stn_yrs )
  
  #  Generate the GDD time series for the chosen AZMET station.
  #  This function takes the variables 'stn_name', 'Tbase', and 
  #  'doy_start', and returns the variable 'stn_data'.
  source( '~/azmet.calculate.gdd.R' )
  stn_data <- azmet.calculate.gdd( stn_name,Tbase,doy_start )
  
  
  ################################################################
  ##  B3. GENERATE AND EXPORT cGDD TRACE GRAPHIC
  ################################################################
  
  
  #  Generate the cGDD trace graphic using the 'azmet.viz.gdd.trace.R'
  #  function. This function takes the variable 'stn_data', and 
  #  returns the variable 'p', which serves as input to the 
  #  'ggplotly' function.
  source( '~/azmet.viz.gdd.trace.R' )
  p <- azmet.viz.gdd.trace( stn_data )
  ggplotly( p )
  
  #  Export the generated graphic to the climateGEM plotly account.
  prefix <- "AZMET-GDD/trace"
  fname <-  paste( prefix,stn_name,sep="" )
  #plotly_POST( p,filename = fname ) ### 'plotly_POST is deprecated'
  
  gg <- ggplotly(p)
  
  api_create( gg,filename = fname,fileopt="overwrite",sharing="public" )
  rm( prefix,fname,p )
  

##################################################################
##  C. END CYCLING THROUGH THE INDIVIDUAL AZMET STATIONS
##################################################################


}


##################################################################
##  D. TERMINATE THE CURRENT RSTUDIO SESSION
##################################################################


#  Terminate the current RStudio session without saving the
#  current environment.
#quit( save="no" )
