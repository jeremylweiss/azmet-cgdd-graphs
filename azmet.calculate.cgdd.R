

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


#####  A. START THE FUNCTION


azmet.calculate.cgdd <- function( stn_name,t_base,doy_start ) {
  
  
  #####  DOWNLOAD AZMET DATA
  
  
  #  Download the most recent AZMET data for the chosen station
  #  and load it as a dataframe into the current R environment. The
  #  'azmet.data.download.R' function returns the variable 'stn_data'.
  source( 'azmet.data.download.R' )
  stn_data <- azmet.data.download( stn_name )
  
  
  #####  FORMAT DATA AND ADDRESS MISSING VALUES
  
  
  #  Dataframe columns of interest include year, month, day, doy, 
  #  stn_no, and Tmean. Select these columns from the AZMET station 
  #  data.
  stn_data <- select( stn_data,
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
  
  #  This function will use doy Tmean climatology to fill in daily
  #  Tmean values of NA when calculating daily GDD values.
  #  Preallocate a Tmean climatology vector and calculate the doy
  #  Tmean climatology. Initial values of NA in the preallocated
  #  matrix will be overwritten by Tmean climatology values.
  Tmean_clim <- matrix( data = NA,
                        nrow = 366,
                        ncol = 1 )
  for( i in 1:nrow( Tmean_clim ) ) {
    a <- filter( stn_data,doy==i )
    Tmean_clim[ i,1 ] <- mean( a$Tmean,na.rm=TRUE )
    rm( a )
  }
  rm( i )
  
  
  Tmean_clim <- group_by( stn_data,doy ) %>%
    summarize( mean( Tmean,na.rm = TRUE ) )
  
  
  group
  
  
  
  
  
  ##################################################################
  ##  C. CALCULATE DAILY GDD VALUES
  ##################################################################
  
  
  #  Add an extra column to the 'stn_data' dataframe to mark daily
  #  data that are missing. We will use this information for data
  #  visualization purposes later on in the overall project. The
  #  initial value for all rows in this column will be NA. Overwrite
  #  these initial values with logical output from the 'is.na' 
  #  function. Also, rename the new column to "is.na" and convert 
  #  the logical output to integer.
  stn_data[ ,ncol( stn_data )+1 ] <- NA
  names( stn_data )[ ncol( stn_data ) ] <- "is.na"
  stn_data$is.na <- is.na( stn_data$Tmean )
  stn_data$is.na <- as.integer( stn_data$is.na )
  
  #  Add an extra column to the 'stn_data' dataframe for the daily
  #  GDD values that will be assigned or calculated. The initial 
  #  value for all rows in this column will be NA. Also, rename the 
  #  new column to "GDD".
  stn_data[ ,ncol( stn_data )+1 ] <- NA
  names( stn_data )[ ncol( stn_data ) ] <- "GDD"
  
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
    else{
      
      #  If the Tmean value for a doy and year is missing, substitue
      #  the climatological Tmean value for the missing Tmean value
      #  for that doy and year, and assign or calculate the daily 
      #  GDD. Occurrence of this substitution is marked by 'TRUE' 
      #  values in the stn_data$is.na column.
      if( stn_data$is.na[ i ]==TRUE ) {
        
        #  If the climatological Tmean value for a doy is less than
        #  'Tbase', assign a GDD value of 0.
        if ( Tmean_clim[ stn_data$doy[ i ] ] < Tbase ) {
          stn_data$GDD[ i ] <- 0
        }
        #  Or, if the climatological Tmean value for a doy is equal
        #  to or greater than 'Tbase', calculate the daily GDD.
        else{
          stn_data$GDD[ i ] <- Tmean_clim[ stn_data$doy[ i ] ] - Tbase
        }
      }
      
      #  Or, if the Tmean value for a doy and year is less than
      #  'Tbase', assign a GDD value of 0.
      else if( stn_data$Tmean[ i ] < Tbase ) {
        stn_data$GDD[ i ] <- 0
      }
      #  Or, if the daily Tmean value is equal to or greater than
      #  'Tbase', calculate the daily GDD.
      else{
        stn_data$GDD[ i ] <- stn_data$Tmean[ i ] - Tbase
      }
      
    }
    
  }
  rm( i )
  
  
  ##################################################################
  ##  D. CALCULATE DAILY CUMMULATIVE GDD VALUES BY CALENDAR YEAR
  ##################################################################
  
  
  #  Add an extra column to the 'stn_data' dataframe for the daily
  #  cummulative GDD values that will be calculated. The initial 
  #  value for all rows in this column will be NA. Also, rename the 
  #  new column to "cGDD".
  stn_data[ ,ncol( stn_data )+1 ] <- NA
  names( stn_data )[ ncol( stn_data ) ] <- "cGDD"
  
  #  Iteratively go through the daily GDD values and calculate a 
  #  corresponding cummulative GDD value for each doy by calendar
  #  year.
  for ( i in 1:nrow( stn_data ) ) {
    
    #  We first address the condition of a GDD value being the first
    #  in the station data record. If it is, the 'cGDD' value is
    #  simply the same value as the 'GDD' value.
    if ( i==1 ) {
      stn_data$cGDD[ i ] <- stn_data$GDD[ i ]
    }
    #  Or, if the GDD value is not the first in the station data
    #  record, go through the following 'cGDD' value calculations.
    else {
      
      #  If the GDD value is from the same calendar year as the one
      #  in the previous row, add the GDD value to this previous
      #  value.
      if ( stn_data$year[ i ]==stn_data$year[ i-1 ] ) {
        stn_data$cGDD[ i ] <- stn_data$cGDD[ i-1 ] + stn_data$GDD[ i ]
      }
      else {
        stn_data$cGDD[ i ] <- stn_data$GDD[ i ]
      }
      
    }
    
  }
  rm( i )
  
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





