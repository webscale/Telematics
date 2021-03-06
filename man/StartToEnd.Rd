% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/telematicsUtil.R
\name{StartToEnd}
\alias{StartToEnd}
\title{A function to calculate the Vector Distance for the trip from start to end}
\usage{
StartToEnd(trip)
}
\arguments{
\item{trip}{a dataframe with x and y columns representing the locations of the driver at each second of the trip}
}
\value{
It will return the travel distance in meters.
}
\description{
Based on a certain trip specified, this function will be able to calculate
the Vector Distance for the trip from start to end.
}
\examples{
# Vector Distance of a certain trip
 StartToEnd(trip=sampleTrip)
}

