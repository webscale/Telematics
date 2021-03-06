% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/telematicsUtil.R
\name{PlotggAcceleration}
\alias{PlotggAcceleration}
\title{A function to plot the acceleration (Moving average over 10 second interval) for the trip}
\usage{
PlotggAcceleration(trip)
}
\arguments{
\item{trip}{a dataframe with x and y columns representing the locations of the driver at each second of the trip}
}
\value{
It will return a qplot of the acceleration over 10 seconds of the driver for that trip.
}
\description{
Based on a certain trip specified, this function will be able to plot
the acceleration acceleration (Moving average over 10 second interval) of the trip. The different colors show the different levels of acceleration,
to show whether the car is accelerating at a high level.
}
\examples{
# Acceleration plot of a trip taken by a driver
 PlotggAcceleration(trip=sampleTrip)
}

