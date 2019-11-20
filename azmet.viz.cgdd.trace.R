

##################################################################
##  FUNCTION FOR VISUALIZING GDD DATA TRACES FROM AZMET STATIONS 
##################################################################


#  Authors:
#  Jeremy Weiss, Climate and Geospatial Extension Scientist
#  School of Natural Resources and the Environment
#  University of Arizona
#  520-626-8063, jlweiss@email.arizona.edu

#  This code generates plots of cummulative growing degree-day 
#  (CGDD) data from an individual AZMET station. Input argument is 
#  'stn_data', which is generated from the R functions 
#  'azmet.download.data.R' and 'azmet.calculate.cgdd.R'. The plot
#  style includes past individual years, climatology, and current
#  year time series. The variable 'p', which contains plotting 
#  information for the 'ggplot' function, is returned.


##################################################################
##  START THE FUNCTION
##################################################################


azmet.viz.cgdd.trace <- function( stn_data ) {
  
  
  #  Select from 'stn_data' the x variable ('doy') and y variable 
  #  ('CGDD') that we will use for this plot. Also select the 'year'
  #  column as we will need this for pulling out the CGDD data for 
  #  the current year as well as for making the data traces for 
  #  past individual years. The reason for doing this step is that
  #  we will be melting this dataframe with that of CGDD 
  #  climatology later in the script.
  df <- select( .data = stn_data,
                year,doy,CGDD )
  
  #  Preallocate a CGDD climatology matrix and calculate the day-
  #  of-year CGDD climatology. Initial values of NA in the 
  #  preallocated matrix will be overwritten by CGDD climatology 
  #  values. Columns will match that of the 'df' dataframe in 
  #  order to melt later in the script.
  cgdd_clim <- as.data.frame( matrix( data = NA,
                                      nrow = 366,
                                      ncol = ncol( df ) ) )
  colnames( cgdd_clim ) <- c( colnames( df ) )
  
  #  Assign year values to CGDD climatology dataframe. Use '9999'
  #  as we will need an integer to smooth the binding of this 
  #  dataframe with that of the individual year dataframe later 
  #  on.
  cgdd_clim$year <- 9999
  
  #  Assign day-of-year values to CGDD climatology dataframe.
  cgdd_clim$doy <- seq( 1:366 )
  
  #  Assign CGDD climatology values to CGDD climatology dataframe.
  for ( i in 1:nrow( cgdd_clim ) ) {
    a <- filter( stn_data, doy == i )
    cgdd_clim$CGDD[ i ] <- mean( a$CGDD, na.rm = TRUE )
    rm( a )
  }
  rm( i )
  
  #  Combine data from the individual year and station climatology
  #  dataframes.
  stn_data_bind <- rbind.data.frame( df,cgdd_clim )
  
  #  Create a new column in the bound dataframe that stores values
  #  from the 'year' column in character format. This will help with
  #  the plotting of individual year data traces. For the climatology
  #  values, place the value "climatology". Otherwise, convert the 
  #  year in integer form to character form.
  stn_data_bind[ "label" ] <- NA
  for ( i in 1:nrow( stn_data_bind ) ) {
    if ( stn_data_bind$year[ i ] == 9999 ) {
      stn_data_bind$label[ i ] <- "climatology"
    }
    else {
      stn_data_bind$label[ i ] <- toString( stn_data_bind$year[ i ] )
    }
  }
  rm( i )
  
  #  Melt the bound dataframe into long format in order to work
  #  better with the plotting commands in ggplot2. Since we want to
  #  know the CGDD values for each 'year' (numeric and character form)
  #  and 'doy', set these as the 'id.vars'.
  stn_data_melt <- melt( data = stn_data_bind,
                         id.vars = c( "year","label","doy" ) )
  
  #  Remove the first column of the bound and melted dataframe, which
  #  is the 'year' vector in numeric form. We will work with the 
  #  character form version of this vector that we recently created, 
  #  in order to make for easier and better plots.
  stn_data_plot <- select( .data = stn_data_melt,
                           label,doy,variable,value )
  
  #  Rename the columns of the bound, melted, and trimmed dataframe.
  colnames( stn_data_plot ) <- c( "year","doy","variable","CGDD" )
  
  #  Round the CGDD values to the nearest whole number before
  #  plotting.
  stn_data_plot$CGDD <- round( x = stn_data_plot$CGDD,
                               digits = 0 )
  
  #  Set up a dataframe with numeric values for each individual year
  #  in the station record. This will help with the symbology of the 
  #  data traces. Add to this dataframe a row for a climatology value. 
  key <- as.data.frame( unique( df$year ) )
  key <- rbind( key, 9999 )
  colnames( key ) <- "stn_yrs"
  
  #  Assign colors to individual years before the current year, to the
  #  current year, and to the climatology as a new vector to the 'key'
  #  dataframe.
  for ( i in 1:nrow( key ) ) {
    #  Assign a color to individual years before the current year.
    if ( key$stn_yrs[ i ] < max( unique( df$year ) ) ) {
      key[ i,2 ] <- "gray80"
    }
    #  Assign a color to the current year.
    else if ( key$stn_yrs[ i ] == max( unique( df$year ) ) ) {
      key[ i,2 ] <- "dark green"
    }
    #  Assign a color to the climatology.
    else if ( key$stn_yrs[ i ] == 9999 ) {
      key[ i,2 ] <- "gray40"
    }
  }
  rm( i )
  colnames( key ) <- c( "stn_yrs","color" )
  
  #  Convert the color assignment vector in the 'key' dataframe to
  #  a form that ggplot can use.
  key <- as.matrix( key$color )
  key <- apply( key,1,paste,collapse="," )
  
  #  Plot the most recent year, average, and min/max range
  #  of cGDD by doy.
  ptitle <- paste( "cummulative growing degree-days at the AZMET",stn_name,"station",sep=" " )
  p <- ggplot( data=stn_data_plot,aes( x=doy,y=CGDD,color=year ) ) +
    geom_line( data=filter( stn_data_plot,year<max( unique( df$year ) ) ) ) + 
    geom_line( data=filter( stn_data_plot,year=="climatology" ) ) + 
    geom_line( data=filter( stn_data_plot,year==max( unique( df$year ) ) ) ) +
    ggtitle( ptitle ) +
    labs( x="day of year",y="cummulative growing degree-days (cGDD)" ) +
    theme_minimal() +
    theme( plot.title=element_text( family="Arvo",color="#404040",face="bold",size=10 ) ) +
    theme( axis.title=element_text( family="Arvo",color="#404040",face="plain",size=10 ) ) +
    theme( plot.margin=unit( c( 0,0,1,1 ),"cm" ) ) +
    theme( axis.text=element_text( family="Arvo",color="#404040",face="plain",size=10 ) ) + 
    theme( legend.position="none") +
    scale_x_continuous( breaks=c( 1,32,60,91,121,152,182,213,244,274,305,335 ),
                        limits=c( min( stn_data_plot$doy ),365 ) ) +
    scale_y_continuous( breaks=seq( min( stn_data_plot$CGDD,na.rm=TRUE ),
                                    max( stn_data_plot$CGDD,na.rm=TRUE ),by=round( ( max( df$CGDD,na.rm=TRUE )/10 ),-2 ) ),
                        limits=c( min( stn_data_plot$CGDD,na.rm=TRUE ),
                                  max( stn_data_plot$CGDD,na.rm=TRUE )+1 ) ) +
    scale_colour_manual( values=key )
  
  #  Transform 'ggplot()' graph into a 'ggplotly()' graph.
  #ggplotly( p )
  #
  #  Note that instead of the above, one must have the function
  #  return the value/variable assigned to the 'ggplot' function
  #  above, and then call the 'ggplotly' function outside of this
  #  current function. See the following example code.
  #source( 'azmet.viz.gdd.trace.R' )
  #p <- azmet.viz.gdd.trace( "Bonita",10,1 )
  #ggplotly( p )
  
  
##################################################################
##  CLOSE THE FUNCTION
##################################################################


return( p )
}
