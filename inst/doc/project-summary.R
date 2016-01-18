### R code from vignette source 'project-summary.Rnw'

###################################################
### code chunk number 1: project-summary.Rnw:20-21
###################################################
library(niraj9.telematics)


###################################################
### code chunk number 2: project-summary.Rnw:24-28
###################################################
# a sample trip that contains the x-y coordinates of a certain
# trip from a driver. Each row is interval of 1 second.

str(sampleTrip)


###################################################
### code chunk number 3: project-summary.Rnw:31-32
###################################################
head(sampleTrip) # example of the dataset of a trip


###################################################
### code chunk number 4: project-summary.Rnw:44-45
###################################################
velocity10  # Function of velocity10


###################################################
### code chunk number 5: project-summary.Rnw:48-50
###################################################
sampleVelocity10 <- velocity10(sampleTrip) #sampleTrip dataset
head(sampleVelocity10)


###################################################
### code chunk number 6: project-summary.Rnw:59-60
###################################################
velocity4 # Function of velocity4


###################################################
### code chunk number 7: project-summary.Rnw:63-65
###################################################
sampleVelocity4 <- velocity4(sampleTrip) #sampleTrip dataset
head(sampleVelocity4)


###################################################
### code chunk number 8: project-summary.Rnw:75-76
###################################################
acceleration # Function of acceleration


###################################################
### code chunk number 9: project-summary.Rnw:79-81
###################################################
sampleAcceleration <- acceleration(sampleTrip) #sampleTrip dataset
head(sampleAcceleration)


###################################################
### code chunk number 10: project-summary.Rnw:90-91
###################################################
breaks # Function of breaks


###################################################
### code chunk number 11: project-summary.Rnw:94-96
###################################################
sampleBreaks <- breaks(sampleTrip) #sampleTrip dataset
head(sampleBreaks)


###################################################
### code chunk number 12: project-summary.Rnw:105-106
###################################################
MovingAverage # Function of MovingAverage


###################################################
### code chunk number 13: project-summary.Rnw:109-111
###################################################
sampleMvAvg <- MovingAverage(sampleTrip) #sampleTrip dataset
head(sampleMvAvg)


###################################################
### code chunk number 14: project-summary.Rnw:123-124
###################################################
PlotggCoordinates(trip=sampleTrip)


###################################################
### code chunk number 15: project-summary.Rnw:132-133
###################################################
PlotggVelocity(trip=sampleTrip)


###################################################
### code chunk number 16: project-summary.Rnw:144-145
###################################################
PlotggAcceleration(trip=sampleTrip)


###################################################
### code chunk number 17: project-summary.Rnw:155-156
###################################################
PlotggBreaks(trip=sampleTrip)


###################################################
### code chunk number 18: project-summary.Rnw:166-167
###################################################
PlotggTripNow(trip=sampleTrip)


###################################################
### code chunk number 19: project-summary.Rnw:177-186
###################################################
# Example,
#
# telematics <- Telematics$new()
# telematics$createMasterSummary(
#  master.file = "../data/master.csv",  
#  file.list = "../data/fileList.csv" ,      
#  root.folder = "../data/drivers",
#  subset.driver.count = 10 ,
#  cores =2)


###################################################
### code chunk number 20: project-summary.Rnw:189-192
###################################################
masterPath1 <- system.file("extdata", "master.csv", package="niraj9.telematics")
master <- read.csv(masterPath1)
str(master)


###################################################
### code chunk number 21: project-summary.Rnw:207-216
###################################################
# Example,
#
# d1793$createDriverSignature()
#
# gbm(formula = target ~ ., distribution = "gaussian", data = train, 
#    n.trees = trees, interaction.depth = 5, shrinkage = shrinkage)
# A gradient boosted model with gaussian loss function.
# 100 iterations were performed.
# There were 63 predictors of which 57 had non-zero influence.


###################################################
### code chunk number 22: project-summary.Rnw:236-246
###################################################
# Example,
#
# telematics <- Telematics$new()
# telematics$createMasterSummary()
#
#  master.file = "../data/master.csv",  
#  file.list = "../data/fileList.csv" ,      
#  root.folder = "../data/drivers",
#  subset.driver.count = 10 ,
#  cores =2)


###################################################
### code chunk number 23: project-summary.Rnw:273-278
###################################################
# Example,
# 
# telematics$load(verbose = FALSE)
# d1793 = Driver$new(db = telematics, driver.name = "1793")
# d1793$ShowTrip("55")


