##
## Begin smiyao code
##

library(package = "niraj9.telematics")
public.folder = "/home/niraj9/public/"
master.file.3 = paste(public.folder, "master-3.csv", sep = "")
master.file.4 = paste(public.folder, "master-4.csv", sep = "")

#Loading the telematics object
telematics <- Telematics$new(master.file.3)
telematics$load(verbose = FALSE)

#Creating the driver object
driver = Driver$new(db = telematics , driver.name = "2751")
driver$summary()

#Plotting methods
q =driver$ShowCoordinates(trip = 165)
q =driver$ShowVelocity(trip = 165)
q =driver$ShowAcceleration(trip = 165)
q =driver$ShowBreaks(trip = 165)
q=driver$ShowQuantiles(trip = 111)
q=driver$ShowSpeedBreaks(trip = 120)

##
## End smiyao code
##
