#library(testthat)
# some of the tests are commented out as the test was done locally on our machines
# but will cause errors if use R CMD check.


test_that("Telematics and Driver features", {
  
  ##
  ## Begin niraj9 code
  ##
  
  # Test 1
  masterPath1 <- system.file("extdata", "master.csv", package="niraj9.telematics")
  telematics <- Telematics$new(masterPath1)
  arun = telematics$load(verbose = FALSE)
  expect_that(arun, prints_text("Master File is loaded"))

  # Test 2
  telematics <- Telematics$new(masterPath1)
  arun = telematics$load()
  expect_that(arun, prints_text("Master File is loaded")) # Visually inspect the verbose output
  
  # Test 3
  arun = telematics$drivers()
  expect_that(arun, prints_text("14")) 
  
  # Test 4
  d14 = Driver$new(db = telematics , driver.name = "14")
  arun = d14$summary()
  expect_that(arun, prints_text(" Driver Name/Num =  14"))
  
  # Test 5
  arun =  d14$ShowCoordinates()
  expect_that(class(arun), prints_text("ggplot"))
  
  # Test 6
  arun =  d14$ShowVelocity()
  expect_that(class(arun), prints_text("ggplot"))
  
  # Test 7
  arun =  d14$ShowVelocity(trip= 104)
  expect_that(class(arun), prints_text("ggplot"))
  
  # Test 8
  arun =  d14$ShowAcceleration(trip=104)
  expect_that(class(arun), prints_text("ggplot"))
  
  # Test 9
  arun =  d14$ShowBreaks()
  expect_that(class(arun), prints_text("ggplot"))
  
  # Test 10
  arun =  d14$ShowBreaks(trip.num = 90)
  expect_that(class(arun), prints_text("ggplot"))
  
  # Test 11
  arun =  d14$ShowQuantiles()
  expect_that(arun, prints_text("TRUE"))
  
  # Test 12
  arun =  d14$ShowQuantiles(trip.num = 23)
  expect_that(arun, prints_text("TRUE"))
  
  ##
  ## End niraj9 code
  ##
  
  ##
  ## Begin smiyao code
  ##
  
  # Test 13
  arun =  d14$ShowSpeedBreaks(trip.num = 112)
  expect_that(class(arun), prints_text("ggplot"))
  
  
  # Test 14
  model = d14$CreateDriverSignature()
  summary(model)
  class(model)
  expect_that(class(model) ,prints_text("gbm"))
  
  # Test 15
  telematics <- Telematics$new(masterPath1)
  telematics$load()
  fit <- telematics$SegmentDrivers(segments = 3)
  expect_that(fit, prints_text("totss")) 
  
  # Test 16
  arun = telematics$VisualizeSegments()
  expect_that(arun, prints_text("TRUE")) 
  
  ##
  ## End smiyao code
  ##
})


##
## Begin niraj9 code
##
## the Multi-cores scenario takes a lot of time and hence commenting this out

#test_that("Telematics file Creation", {
#  telematics <- Telematics$new("test-master.csv")
#  arun = telematics$CreateMasterSummary(file.list = "/home/niraj9/public/fileList.csv" ,
#                                         root.folder = "/home/niraj9/public/drivers/",
#                                         subset.driver.count = 5 ,
#                                         cores =2)
#   expect_that(arun, prints_text("col count= 66"))
# })

##
## End niraj9 code
##