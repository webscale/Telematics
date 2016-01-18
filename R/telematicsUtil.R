# library(plyr)
# library(ggplot2)
# library(gridExtra)
# library(grid)


##
## Begin niraj9 code
##

#-----------------------------------------------------------------------------------
# A utility for Telematics
# 
# Contains the various functions to create the various features such as velocity,
# acceleration, breaks and also plot driver graphs
#
#-----------------------------------------------------------------------------------


#' A function to plot the X-Y coordinates taken by the driver of the trip.
#' 
#' Based on a certain trip specified, this function will be able to plot
#' the XY Coordinates of a trip using ggplot scatter plot
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#' @return It will return a qplot of the X-Y coordinates taken by the driver of the trip.
#' @import ggplot2
#' @export
#' @examples # X-Y coordinates plot of a trip taken by a driver
#'  PlotggCoordinates(trip=sampleTrip)

PlotggCoordinates<- function(trip){
  xyPlot = qplot(trip$x, trip$y, data=trip)
  xyPlot = xyPlot + labs(list(title = "X-Y Coordinates for Trip", 
                              x = "X (mtrs)",
                              y = "Y (mtrs)"))
  return(xyPlot)
}

#' A function to plot the Velocity (Moving average over 10 second interval) for the trip 
#' 
#' Based on a certain trip specified, this function will be able to plot
#' the velocity of the trip. The different colors show the status of the car,
#' to show whether the car is driving on the freeway or whether it is moving very slowly.
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#' @return It will return a qplot of the velocity (Moving average over 10 second interval) for the trip.
#' @import ggplot2
#' @export
#' @examples # Velocity plot of a trip taken by a driver
#'  PlotggVelocity(trip=sampleTrip)

PlotggVelocity <- function(trip){
  v10 = velocity10(trip)
  x = seq(1:length(v10))
  vel = data.frame(x, v10)
  
  vel$zone = "street"
  #land on the right cut off's here
  sRowCount = nrow(vel[vel$v10 < 15, ])
  fRowCount = nrow(vel[vel$v10 > 75, ])
  
  if(sRowCount > 0) vel[vel$v10 < 15, ]$zone = "slow-area"
  if(fRowCount > 0) vel[vel$v10 > 75, ]$zone = "freeway"
  
  pp2 = qplot(x, v10,data = vel, colour = vel$zone) + 
              scale_colour_discrete(name = "Velocity Zone")
  pp2 = pp2 + labs(list(title = "Velocity Moving Average(10 sec)",
                        x = "Time(seconds)", 
                        y = "Velocity(kmps)"))
  return(pp2)
  
}

#' A function to plot the Quantiles for Velocity, Acceleration and Breaks
#' 
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#' @return The function will return TRUE if everything was OK
#' @export
#' @examples # Velocity plot of a trip taken by a driver
#'  PlotQuantiles(trip=sampleTrip)
PlotQuantiles <- function(trip){
  par(mfrow=c(1, 3))
  v = velocity10(trip)
  cdf.v = ecdf(v)
  plot(cdf.v, xlab = 'Speed m/s', ylab = 'Percent(%)', main = 'Speed Quantiles')
  
  a = acceleration(trip)
  cdf.a = ecdf(a)
  plot(cdf.a, xlab = 'Acceleration kmph/s', ylab = '%', main = 'Acceleration Quantiles')
  
  b4 = breaks(trip)
  cdf.b4 = ecdf(b4)
  plot(cdf.a, xlab = 'Breaks kmph/s', ylab = '%', main = 'Break Quantiles')
  return(TRUE)
}


#' A function to plot the acceleration (Moving average over 10 second interval) for the trip 
#' 
#' Based on a certain trip specified, this function will be able to plot
#' the acceleration acceleration (Moving average over 10 second interval) of the trip. The different colors show the different levels of acceleration,
#' to show whether the car is accelerating at a high level.
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#' @return It will return a qplot of the acceleration over 10 seconds of the driver for that trip.
#' @import ggplot2
#' @export
#' @examples # Acceleration plot of a trip taken by a driver
#'  PlotggAcceleration(trip=sampleTrip)

PlotggAcceleration <- function(trip){
  a10 = acceleration(trip = trip)
  x = seq(1:length(a10))
  acc = data.frame(x, a10)
  
  acc$levels = "low"
  hCount = nrow(acc[acc$a10 > 15,]) 
  
  if(hCount > 0){
    acc[acc$a10 > 15,]$levels = "high"
    acc[acc$a10 < 0,]$a10 = 0
  }
  
  pp3 = qplot(x, a10, data = acc, colour = acc$levels) +
        scale_colour_discrete(name = "Acceleration Levels")
  pp3 = pp3 + labs(list(title = "Acceleration Moving Average(10 sec)", 
                        x = "Time(seconds)", 
                        y = "acceleration(kmph/sec)"))
 return(pp3) 
}




#' A function to plots the breaks (Moving average over 4 second interval) for the trip. 
#' 
#' Based on a certain trip specified, this function will be able to plot
#' the breaks  (Moving average over 4 second interval) occurred during the trip. The different colors show the different levels of breaks.
#' Note that breaks is the same as acceleration
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#' @return It will return a qplot of the brakes over 4 seconds of the driver for that trip.
#' @import ggplot2
#' @export
#' @examples # Breaks plot of a trip taken by a driver
#'  PlotggBreaks(trip=sampleTrip)

PlotggBreaks <- function(trip){
  b4 = breaks(trip)
  x = seq(1:length(b4))
  brks = data.frame(x, b4)
  
  brks$levels = "normal-breaking"
  hCount = nrow(brks[brks$b4 < -8,]) 
  if(hCount > 0){
    brks[brks$b4 < -8, ]$levels = "hard-breaking"
    brks[brks$b4 > 0, ]$b4 = 0
  }
  pp3 = qplot(x, b4, data = brks, colour = brks$levels) +
        scale_colour_discrete(name = "Breaking Levels")
  pp3 = pp3 + labs(list(title = "Breaking behaviour(4 sec moving avg)", 
                        x = "Time(seconds)", 
                        y = "break (kmph per sec"))
  return(pp3)
}

#' A function that shows the points of time the speed limit of 75KMPH was broken by the driver
#' 
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of
#' the trip
#' @return It will return a qplot .
#' @import ggplot2
#' @export
#' @examples # Speed Limit Breaks plot of a trip taken by a driver
#'  PlotggSpeedLimitBreakage(trip=sampleTrip)

PlotggSpeedLimitBreakage <- function(trip){
  v10 = velocity10(trip)
  x = seq(1:length(v10))
  vel = data.frame(x, v10)
  category = sapply(vel$v10, FUN = function(speed){ifelse(speed > 75, "break","normal")} )
  vel$zone = category
  vel[vel$zone == "normal",]$v10 = 0
  pp2 = qplot(x, v10,data = vel,colour = vel$zone ) + 
            scale_colour_discrete(name = "Speed Limit Cross ( > 75KMPH)")
  pp2 = pp2 + labs(list(title = "Speed Limit Breakages on Freeway",
                        x = "Time(seconds)",
                        y = "Velocity(kmps)"))
  return(pp2)
}

##
## End niraj9 code
##

##
## Begin smiyao code
##

#' A function to plot a summary trip graph in a grid 
#' 
#' Based on a certain trip specified, this function will be able to plot
#' the 4 different features.
#' a) X-Y Coordinates
#' b) Velocity of the trip
#' c) Acceleration of the trip
#' d) Breaks of the trip
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#' @return It will return a multi plot of the 4 features (X-Y coordinates, velocity, acceleration, breaks)
#' @import ggplot2
#' @import gridExtra
#' @export
#' @examples # Plot of the 4 features (X-Y coordinates, velocity, acceleration, breaks)
#'  PlotggTripNow(trip=sampleTrip)

PlotggTripNow <- function(trip){
  p1 = PlotggCoordinates(trip)
  p2 = PlotggVelocity(trip)
  p3 = PlotggAcceleration(trip)
  p4 = PlotggBreaks(trip)
  GridPlot(p1,p2,p3,p4, cols=2)
}



#' Main function to summarize a trip to a set of features represented by a row 
#' 
#' A Trip can be of varying timeframes. i.e a trip of 200 seconds will have 200 rows and trip of 1000 seconds #' will have 1000 rows.
#' This function normalize's all trips to 1 row of 63 features.
#' The features include
#' triptime, tripdistance(travel), tripStarttoend distance, 20 features for velocity quantiles,
#' 20 features for acceleration quantiles, 20 features for breaks quantiles
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#'@return a vector of 63 columns representing a trip
#' @import ggplot2
#' @export
#' @examples # Trip details of a certain trip
#'  TripDetails(trip=sampleTrip)
TripDetails <- function(trip){
  v10 =  quantile(velocity10(trip), seq(0.05, 1, by = 0.05))
  a =  quantile(acceleration(trip), seq(0.05, 1, by = 0.05))  
  b4 =  quantile(breaks(trip), seq(0.05, 1, by = 0.05))  
    
  tripTime = length(trip$x)
  tripDistance= sum(TravelDisList(trip))
  tripStartToEnd = StartToEnd(trip)
  
  TripDetails = c(tripTime, tripDistance, tripStartToEnd, v10, a, b4)
  TripDetails = round(TripDetails, digits = 3)
}

#' A function to calculate the Vector Distance for the trip from start to end
#' 
#' Based on a certain trip specified, this function will be able to calculate
#' the Vector Distance for the trip from start to end.
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#' @return It will return the travel distance in meters.
#' @export
#' @examples # Vector Distance of a certain trip
#'  StartToEnd(trip=sampleTrip)

StartToEnd <-function(trip){
  xi  = tail(trip$x, n=1)
  yi = tail(trip$y, n=1)
  tdis = sqrt(yi^2 + xi^2)
  return(tdis)
}

##
## End smiyao code
##

##
## Begin niraj9 code
##


#' A function to create a list of the travel Distance for the trip from start to end
#' 
#' Based on a certain trip specified, this function will be able to create
#' a list of the travel distance for the trip from start to end
#' 
#' @param trip a dataframe with x and y columns representing the locations of the driver at each second of the trip
#' @return It will return a list of the travel distance in meters.
#' @export
#' @examples # List of distance of a certain trip.
#'  TravelDisList(trip=sampleTrip)

TravelDisList <- function(trip){
  z =  sqrt(diff(trip$x,1,1)^2 + diff(trip$y,1,1)^2)
  return(z)
}



#' A function to calculate the velocities of the trip based on interval of 10 seconds.
#' 
#' Based on a certain trip specified, this function will be able to calculate
#' the velocities of the trip based on interval of 10 seconds.
#' 
#' @param trip The trip of the driver
#' @return It will return the velocities of the trip in km/h based on 10 second intervals.
#' @export
#' @examples # Velocities of the trip.
#'  velocity10(trip=sampleTrip)

velocity10 <- function(trip){
  x = diff(trip$x,8,1)
  y = diff(trip$y,8,1)
  x = MovingAverage(xv = x, n = 8, centered = TRUE)
  y = MovingAverage(xv = y, n = 8, centered = TRUE)
  vel = 3.6 * sqrt(x^2 + y^2) / 8
  return(vel)
}

##
## End niraj9 code
##

##
## Begin smiyao code
##

#' A function to calculate the velocities of the trip based on interval of 4 seconds.
#' 
#' Based on a certain trip specified, this function will be able to calculate
#' the velocities of the trip based on interval of 10 seconds.
#' 
#' @param trip The trip of the driver
#' @return It will return the velocities of the trip in km/h based on 10 second intervals.
#' @export
#' @examples # Velocities of the trip.
#'  velocity4(trip=sampleTrip)

velocity4 <- function(trip){
  x = diff(trip$x,4,1)
  y = diff(trip$y,4,1)
  x = MovingAverage(xv = x, n = 4, centered = TRUE)
  y = MovingAverage(xv = y, n = 4, centered = TRUE)
  vel = 3.6* sqrt(x^2 + y^2)/4
  return(vel)
}

#' A function to calculate the acceleration using velocities obtained based on interval of 10 seconds.
#' 
#' Based on a certain trip specified, this function will be able to calculate the acceleration using
#' the velocities of the trip that is based on interval of 10 seconds.
#' 
#' @param trip The trip of the driver
#' @return It will return the acceleration of the trip in km/h based on 10 second intervals.
#' @export
#' @examples # Acceleration of the trip (based on velocity10)
#'  acceleration(trip=sampleTrip)

acceleration <- function(trip){
  velocities = velocity10(trip)
  acc = diff(velocities, 15)  
  return(acc)
}

#' A function to calculate the breaks using velocities obtained based on interval of 4 seconds.
#' 
#' Based on a certain trip specified, this function will be able to calculate the breaks using
#' the velocities of the trip that is based on interval of 4 seconds.
#' 
#' @param trip The trip of the driver
#' @return It will return the breaks of the trip based on 4 second intervals.
#' @export
#' @examples # Breaks of the trip (based on velocity4)
#'  breaks(trip=sampleTrip)

breaks <- function(trip){
  velocities = velocity4(trip)
  br =  diff(velocities, 4)  
  return(br)
}

##
## End smiyao code
##

##
## Begin niraj9 code
##


#' A function to calculate the breaks using velocities obtained based on interval of 4 seconds.
#' 
#' Based on a certain trip specified, this function will be able to calculate the breaks using
#' the velocities of the trip that is based on interval of 4 seconds.
#' 
#' @param xv xv
#' @param n value=1
#' @param centered True or False
#' @return It will return the breaks of the trip based on 4 second intervals.
#' @export
#' @examples # Breaks of the trip (based on velocity4)
#'  breaks(trip=sampleTrip)

MovingAverage <- function(xv, n = 1, centered = FALSE) {
  
  if (centered) {
    before <- floor  ((n-1) / 2)
    after  <- ceiling((n-1) / 2)
  } else {
    before <- n-1
    after  <- 0
  }
  
  # Track the sum and count of number of non-NA items
  s     <- rep(0, length(xv))
  count <- rep(0, length(xv))
  
  # Add the centered data 
  new <- xv
  # Add to count list wherever there isn't a 
  count <- count + !is.na(new)
  # Now replace NA_s with 0_s and add to total
  new[is.na(new)] <- 0
  s <- s + new
  
  # Add the data from before
  i <- 1
  while (i <= before) {
    # This is the vector with offset values to add
    new   <- c(rep(NA, i), xv[1:(length(xv)-i)])
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i + 1
  }
  
  # Add the data from after
  i <- 1
  while (i <= after) {
    # This is the vector with offset values to add
    new   <- c(xv[(i+1):length(xv)], rep(NA, i))
    
    count <- count + !is.na(new)
    new[is.na(new)] <- 0
    s <- s + new
    
    i <- i + 1
  }
  
  # return sum divided by count
  zz =s / count
  return(zz)
}

##
## End niraj9 code
##

##
## Begin smiyao code
##


#' A function to create grid plots
#' 
#' @param ... Any arguments
#' @param plotlist List of plots
#' @param cols columns
#' @return It will return a function of grid plot
#' @import ggplot2
#' @import gridExtra
#' @import grid
#' @export

GridPlot <- function(..., plotlist=NULL, cols) {
  
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  plotCols = cols                         
  plotRows = ceiling(numPlots / plotCols) 
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i / plotCols)
    curCol = (i - 1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol))
  }
  
}

#' A function to print a message and stop execution
#' 
#' @param msg a string message
#' @return It will stop execution
#' @export

PrintAndStop <- function(msg){
  print(msg)
  stop()
}

##
## End smiyao code
##


