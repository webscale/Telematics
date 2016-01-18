#' R6 class for Driver
#' 
#' This is a R6 class for Driver which contains methods to obtain various plots and information
#' about the driver.
#' 
#' 
#' @docType class
#' @import R6
#' @import gbm
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export Driver
#' @section Methods:
#' \describe{
#'  \item{\code{ShowTrip(trip)}}{This method uses \code{trip} as an argument to return a plot of 4 features (X-Y coordinates, velocity, acceleration, breaks).}
#'  \item{\code{ShowCoordinates(trip)}}{This method uses \code{trip} as an argument to return a qplot of the X-Y coordinates taken by the driver of the trip.}
#'  \item{\code{ShowVelocity(trip)}}{This method uses \code{trip} as an argument to return a qplot of the velocity (Moving average over 10 second interval) for the trip.}
#'  \item{\code{ShowAcceleration(trip)}}{This method uses \code{trip} as an argument to return a qplot of the acceleration over 10 seconds of the driver for that trip.}
#'  \item{\code{ShowBreaks(trip)}}{This method uses \code{trip} as an argument to return a qplot of the breaks over 4 seconds of the driver for that trip.}
#'  \item{\code{ShowSpeedBreaks(trip)}}{This method uses \code{trip} as an argument to return a qplot of the Speed Limit Breakages on Freeway for Driver}
#'  \item{\code{ShowQuantiles(trip)}}{This method uses \code{trip} as an argument to return TRUE if everything was OK.}
#'  \item{\code{CreateDriverSignature(trainClassMultipler = 5, trees = 100, shrinkage = .01)}}{This method returns a model for creating a driver unique signature.}
#' }

# library(R6)
##
## Begin smiyao code
##
Driver <- R6Class("Driver",
                  public = list(
                    name = NA,
                    driver = NA,
                    tripCount = NA,
                    teleDB = NA,
                    initialize = function(db = NA, driver.name=NA){
                      self$name = driver.name
                      self$teleDB <<- db
                      if(is.na(driver.name)) printAndStop(" The TeleDB or driver name(num) are absent . Stopping")
                      self$driver = db$master[db$master$driver == driver.name,]
                      self$tripCount = nrow(self$driver)
                    },
                    ShowTrip = function(trip.num = sample(1:self$tripCount, size =1)){
                      print(trip.num)
                      drow = self$driver[(self$driver$trip == trip.num),]
                      fname = as.character(drow$file)
                      trip =  read.csv(fname)
                      PlotggTripNow(trip)
                    },
                    ShowCoordinates = function(trip.num = sample(1:self$tripCount, size =1)){
                      print(trip.num)
                      drow = self$driver[(self$driver$trip == trip.num), ]
                      fname = as.character(drow$file)
                      trip =  read.csv(fname)
                      qp = PlotggCoordinates(trip)
                      print(qp)
                      return(qp)
                    },
                    ShowVelocity = function(trip.num = sample(1:self$tripCount, size =1)){
                      print(trip.num)
                      drow = self$driver[(self$driver$trip == trip.num), ]
                      fname = as.character(drow$file)
                      trip =  read.csv(fname)
                      vtitle = paste("Velocity Moving Avg(10 sec) for Driver =", self$name, "and Trip =", trip.num)
                      qp = PlotggVelocity(trip)
                      qp = qp + labs(list(title = vtitle, x = "Time(seconds)", y = "Velocity(kmps)"))
                      print(qp)
                      return(qp)
                      
                    },
                    ShowAcceleration = function(trip.num = sample(1:self$tripCount, size = 1)){
                      print(trip.num)
                      drow = self$driver[(self$driver$trip == trip.num), ]
                      fname = as.character(drow$file)
                      trip =  read.csv(fname)
                      qp = PlotggAcceleration(trip)
                      vtitle = paste("Acceleration Moving Avg(10 sec) for Driver =", self$name,"and Trip =", trip.num)
                      qp = qp + labs(list(title = vtitle, x = "Time(seconds)", y = "Acceleration(kmps/sec)"))
                      print(qp)
                      return(qp)
                      
                    },
                    ShowBreaks = function(trip.num = sample(1:self$tripCount, size = 1)){
                      print(trip.num)
                      drow = self$driver[(self$driver$trip == trip.num),]
                      fname = as.character(drow$file)
                      trip =  read.csv(fname)
                      qp = PlotggBreaks(trip)
                      vtitle = paste("Breaking behaviour(4 sec moving avg) for Driver =", self$name, "and Trip =", trip.num)
                      qp = qp + labs(list(title = vtitle, x = "Time(seconds)", y = "Breaks(kmps/sec)"))
                      print(qp)
                      return(qp)
                    },
                    ShowSpeedBreaks = function(trip.num = sample(1:self$tripCount, size = 1)){
                      print(trip.num)
                      drow = self$driver[(self$driver$trip == trip.num), ]
                      fname = as.character(drow$file)
                      trip =  read.csv(fname)
                      qp = PlotggSpeedLimitBreakage(trip)
                      vtitle = paste("Speed Limit Breakages on Freeway for Driver =", self$name, "and Trip =", trip.num)
                      qp = qp + labs(list(title = vtitle, x = "Time(seconds)", y = "Velocity(kmph)"))
                      print(qp)
                      return(qp)
                    },
                    ShowQuantiles = function(trip.num = sample(1:self$tripCount, size = 1)){
                      print(trip.num)
                      drow = self$driver[(self$driver$trip == trip.num), ]
                      fname = as.character(drow$file)
                      trip =  read.csv(fname)
                      PlotQuantiles(trip)
                      return(TRUE)
                    },
                    summary = function(){
                      print(paste(" trip count =", nrow(self$driver)))
                      print(paste(" Driver Name/Num = ", self$name))
                    },
                    CreateDriverSignature = function(trainClassMultipler = 5, trees = 100, shrinkage = .01){
                      db = self$teleDB$master
                      targetA = db[db$driver == self$name,]
                      targetA$target = 1
                      targetB = db[db$driver != self$name,]
                      targetB = db[sample(nrow(targetB), trainClassMultipler * nrow(targetA)), ]
                      targetB$target = 0
                      train =  rbind(targetA,targetB)
                      train$file = NULL
                      train$driver = NULL
                      train$trip = NULL
                      
                      # running the GBM Model
                      trip.model = gbm(target ~ ., data = train, distribution = "gaussian", 
                                       n.trees = trees, shrinkage = shrinkage, interaction.depth = 5 )
                      
                      return(trip.model)
                      
                    }
                  ),
                  private = list(
                    teledb = NA
                  )
)

##
## End smiyao code
##
