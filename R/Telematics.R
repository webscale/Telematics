#' R6 class for Telematics
#' 
#' This is an R6 class for Telematics which contains methods to create a master summary file.
#' The mster summary file is a summarization of all trips by all drivers. 
#' Each trip has about 66 features that are created by this class
#' 
#' This class needs a location for masterSummary.csv as part of constructors. 
#' You can call load method on the class to load the summary file OR
#' You can call createMasterSummary to create a new one. 
#' 
#' @docType class
#' @import R6
#' @import parallel
#' @import cluster
#' @source telematicsUtil.R
#' @format An \code{\link{R6Class}} generator object
#' @keywords data
#' @export Telematics
#' @section Methods:
#' \describe{
#'  \item{\code{load(trip)}}{This method uses \code{verbose = TRUE} as an argument to load the masterfile into memory.}
#'  \item{\code{summary()}}{This method prints a summary of the master file. }
#'  \item{\code{drivers()}}{This method returns the unique drivers loaded into memory.}
#'  \item{\code{CreateMasterSummary()}}{Creates the master summary csv file that has a summary of the various features of all the trips. \cr
#'              Method options and Defaults : \cr
#'               file.list ="fileList.csv" : The CSV file that has listing of all the trips\cr 
#'               root.folder = ". : The root.folder where the directory structure is placed\cr 
#'               subset.driver.count = 0 : the number of drivers you want in your master. IF 0 then all drivers considered. Note this can take a few hours as we have about half million trip. \cr 
#'               cores = detectCores() : number of cores to run on\cr 
#'              }
#'  \item{\code{SegmentDrivers()}}{This method runs a K-means clustering algorith to segment drivers in the clusters. 
#'                                The user has to supply the numebr of segments. The return type if a k-means model}
#'  \item{\code{VisualizeSegments()}}{This method shows a clusterPlot of a previously create k-means model}
#'              
#' }

# library(R6)
# library(parallel)
# source("telematicsUtil.R")
# library(cluster) 
 

##
## Begin niraj9 code
##
Telematics <- R6Class("Telematics",
                      public = list(
                        master = data.frame(),
                        initialize = function(master.file = NA){
                          private$master.file = master.file 
                          ifelse(is.na(private$master.file),
                                 PrintAndStop("MasterSummary file is NA. Stopping execution"),
                                 print(paste(" Master Summary = ",  private$master.file)))
                        },
                        load = function(verbose = TRUE ){
                          
                          # Load the Masterfile into the memory
                          private$loadMasterFile(private$master.file)
                          if(verbose) self$summary()
                          print("Master File is loaded")
                        },
                        summary = function(){
                          print(summary(self$master))
                        },
                        drivers = function(count = NA){
                          ifelse(is.na(count), 
                                 return(unique(self$master$driver)),
                                 return(unique(self$master$driver)[1:count]))
                        },
                        CreateMasterSummary = function(file.list ="fileList.csv",
                                                       root.folder = ".",
                                                       subset.driver.count = 0, 
                                                       cores = detectCores()){
                          # Create the master summary file and save it.Overwrite if TRUE
                          fileList = read.csv(file = file.list)
                          fileList <- fileList[order(fileList$driver), ] 
                          private$root.folder = root.folder
                          
                          if(subset.driver.count != 0){
                            sub = sample(fileList$driver, subset.driver.count)
                            fileList = fileList[fileList$driver %in% sub, ]
                          }
                          
                          fileList$dpath = as.character(fileList$fileName)
                          tripfiles = fileList$dpath
                          print(paste("Processing... with cores=", cores))
                          
                          cl <- makeCluster(cores, outfile = "msg.txt")
                          a = parSapply(cl,tripfiles, private$CreateTrip)
                          stopCluster(cl) # Don't forget 
                          
                          trips = t(a)
                          rownames(trips) = trips[, 1]
                          
                          trips = as.data.frame(trips)
                          
                          fileList$driver = as.character(fileList$driver)
                          fileList$trip = as.character(fileList$trip)
                          
                          trips$driver = fileList$driver
                          trips$trip = fileList$trip
                          vn = paste("v", seq(1:20), sep = "")
                          an = paste("a", seq(1:20), sep = "")
                          bn = paste("b", seq(1:20), sep = "")
                          
                          
                          cn = c("file", "time", "dis", "vecDis",  vn, an, bn, "driver", "trip" )
                          colnames(trips) = cn
                          colnames(trips)
                          write.csv(trips, file = private$master.file, row.names=F, quote=F)
                          self$master = trips
                          trips = NULL
                          print(paste("Summary file created = ", private$master.file))
                          print(paste("row count=", nrow(self$master)))
                          print(paste("col count=", ncol(self$master)))
                          
                        },
                        SegmentDrivers = function(segments = 7, verbose = TRUE){
                          kmeansDataFrame <- self$master
                          kmeansDataFrame$file <-  NULL
                          kmeansDataFrame$driver <- NULL
                          kmeansDataFrame$trip <- NULL
                          fit <- kmeans(kmeansDataFrame, segments)
                          private$kmeans.fit <- fit
                          if(verbose) print(summary(fit))
                          return(fit) 
                        },
                        VisualizeSegments = function(){
                          fit<- private$kmeans.fit 
                          kmeansDataFrame <- self$master
                          clusplot(kmeansDataFrame, fit$cluster, color = TRUE, shade = TRUE, 
                                   labels = 2, lines = 0, main = "Cluster Plot against 1&2 principal components")
                          # plotcluster(kmeansDataFrame, fit$cluster, main = "Centroid Plot- 1st& 2 discriminant functions")
                          return(TRUE)
                        }
                      ),
                      private = list(
                        test.mode = TRUE,
                        master.file = NA,
                        multi.core = TRUE,
                        kmeans.fit = NA,
                        root.folder = NA,
                        n.cores = detectCores(),
                        loadMasterFile = function(mfile){
                          self$master = read.csv(file = mfile)
                        },
                        CreateTrip = function(x){
                          # source("telematicsUtil.R")
                          print(x)
                          tf = read.csv(x)
                          r = c(x, niraj9.telematics::TripDetails(trip = tf))
                          return(r)
                        }
                      )
)

##
## End niraj9 code
##