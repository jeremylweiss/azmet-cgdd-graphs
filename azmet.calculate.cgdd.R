

##################################################################
##  FUNCTION FOR CALCULATING CGDDS FROM DAILY AZMET DATA 
##################################################################


#  Authors:
#  Jeremy Weiss, Climate and Geospatial Extension Scientist
#  School of Natural Resources and the Environment
#  University of Arizona
#  520-626-8063, jlweiss@email.arizona.edu
#
#  Michael Crimmins, Climate Science Extension Specialist
#  Department of Soil, Water, and Environmental Science
#  University of Arizona
#  520-626-4244, crimmins@email.arizona.edu

#  This code uses daily average temperature data from an individual 
#  AZMET station and calculates growing degree-days (GDD) and 
#  cumulative GDDs for each year.

#  Input arguments include 'stn_name', selected from 'stn_list'
#  dataframe, 't_base', the base temperature from which to 
#  calculate GDD, and 'doy_start', the start day-of-year within the
#  same calendar year. Units for base temperatures need to be in 
#  degrees C.


#####  START THE FUNCTION


azmet.calculate.cgdd <- function( stn_name,t_base,doy_start ) {
  
  
  #####  DOWNLOAD AZMET DATA
  
  
  #  Download the most recent AZMET data for the chosen station
  #  and load it as a dataframe into the current R environment. The
  #  'azmet.data.download.R' function returns the variable 'stn_data'.
  source( 'azmet.data.download.R' )
  stn_data <- azmet.data.download( stn_name )
  
  
  #####  FORMAT DATA
  
  
  #  Dataframe columns of interest include year, month, day, doy, 
  #  stn_no, and Tmean. Select these columns from the AZMET station 
  #  data.
  stn_data <- select( stn_data,
                      date,
                      year,
                      month,
                      day,
                      doy,
                      stn_no,
                      Tmean )
  
  #  Extract the row of information (station name, station number,
  #  start year, and end year) tied to the selected AZMET station.
  stn_info <- subset( stn_list,
                      stn == stn_name )
  
  #  The start doy of the start year for AZMET stations varies.
  #  Thus, there may be instances in which GDD calculations for an
  #  individual year are called to start before station data exist.
  #  This can result in cummulative GDD values for an individual
  #  year that are nonsense. Using the 'doy_start' value, check the
  #  the first year of 'stn_data' to see if its 'doy' is before or
  #  after the designated 'doy_start'. If 'doy' is earlier than 
  #  'doy_start', the current 'stn_data' is fine. If 'doy' is later
  #  than 'doy_start', remove the first year of data from 'stn_data'.
  if ( first( stn_data$doy ) > doy_start ) {
    stn_data <- filter( stn_data,
                        stn_data$year > stn_info$start_yr )
    stn_info$start_yr <- stn_info$start_yr + 1
  }
  
  #  Set the range of years for which data are available for the
  #  selected station.
  stn_yrs <- as.integer( select( stn_info,start_yr ) ):as.integer( select( stn_info,end_yr ) )
  
  #  This script will use doy Tmean climatology in place of daily
  #  Tmean values of NA when calculating daily GDD values.
  #  Make a doy-Tmean climatology vector and calculate the doy
  #  Tmean climatology.
  #Tmean_clim <- matrix( data = NA,
  #                      nrow = 366,
  #                      ncol = 1 )
  #for( i in 1:nrow( Tmean_clim ) ) {
  #  a <- filter( stn_data,doy==i )
  #  Tmean_clim[ i,1 ] <- mean( a$Tmean,na.rm=TRUE )
  #  rm( a )
  #}
  #rm( i )
  Tmean_clim <- group_by( stn_data,doy ) %>%
    summarize( Tmean_clim = mean( Tmean,na.rm = TRUE ) )
  
  
  #####  CALCULATE DAILY GDD VALUES
  
  
  #  Add an extra column to the 'stn_data' dataframe for the daily
  #  GDD values that will be  calculated. The initial value for 
  #  all rows in this column will be NA. Also, rename the new 
  #  column to "GDD".
  stn_data[ ,ncol( stn_data )+1 ] <- NA
  colnames( stn_data )[ ncol( stn_data ) ] <- "GDD"
  
  #  Iteratively go through the daily Tmean values and assign or
  #  calculate a corresponding GDD value for each doy and year.
  for ( i in 1:nrow( stn_data ) ) {
    
    #  We first address the condition of a doy being before or after
    #  the specified 'doy_start' variable. If a doy is before
    #  'doy_start', assign a GDD value of 0.
    if ( stn_data$doy[ i ] < doy_start ) {
      stn_data$GDD[ i ] <- 0
    }
    
    #  Or, if a doy is equal to or after 'doy_start', go through the 
    #  following GDD value assignment or calculation.
    else {
      
      #  If the Tmean value for a doy and year is missing, substitue
      #  the climatological Tmean value for the missing Tmean value
      #  for that doy and year, and assign or calculate the daily 
      #  GDD.
      if ( is.na( stn_data$Tmean[ i ]==TRUE ) ) {
        
        #  If the climatological Tmean value for a doy is less than
        #  'Tbase', assign a GDD value of 0.
        if ( Tmean_clim$Tmean_clim[ stn_data$doy[ i ] ] < t_base ) {
          stn_data$GDD[ i ] <- 0
        }
        #  Or, if the climatological Tmean value for a doy is equal
        #  to or greater than 'Tbase', calculate the daily GDD.
        else {
          stn_data$GDD[ i ] <- Tmean_clim$Tmean_clim[ stn_data$doy[ i ] ] - t_base
        }
      }
      
      #  Or, if the Tmean value for a doy and year is present and 
      #  less than 't_base', assign a GDD value of 0.
      else if ( stn_data$Tmean[ i ] < t_base ) {
        stn_data$GDD[ i ] <- 0
      }
      
      #  Or, if the daily Tmean value for a doy and year is present 
      #  and equal to or greater than 't_base', calculate the daily
      #  GDD.
      else {
        stn_data$GDD[ i ] <- stn_data$Tmean[ i ] - t_base
      }
      
    }
    
  }
  rm( i )
  
  
  #####  CALCULATE CGDD VALUES
  
  
  #  Add a  column to the 'stn_data' dataframe for the daily
  #  cummulative GDD values that will be calculated. The initial 
  #  value for all rows in this column will be NA.
  stn_data[ "CGDD" ] <- NA
  
  #  Calculate daily CGDD values for each year in station record.
  for ( yr in 1:length( stn_yrs ) ) {
    
    #  Subset individual years.
    x <- filter( stn_data,
                 year == stn_yrs[ yr ] )
    
    #  Calculate cumulative sum of GDD values.
    stn_data$CGDD[ which( stn_data$year == stn_yrs[ yr ] ) ] <- 
      cumsum( x$GDD )
    
    rm( x )
  }
  rm( yr )
  
  
  #####  PATCH
  
  
  #  The Yuma South AZMET station has no entries from June 8, 2013
  #  through September 10, 2013. This renders any cGDD values from
  #  September 10, 2013 onwards as useless. Omit these autumn and
  #  early winter values from the station data.
  if ( stn_name=="Yuma South" ) {
    stn_data <- rbind( stn_data[ 1:which( stn_data$year==2013 & stn_data$doy==158 ),1:ncol( stn_data ) ],
                       stn_data[ which(stn_data$year==2014 & stn_data$doy==1):nrow( stn_data ),1:ncol( stn_data ) ] )
  }
  
  
  #####  RETURN DATA AND CLOSE THE FUNCTION
  
  
  return( stn_data )
}





