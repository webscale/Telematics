##
## Begin niraj9 code
##

library(package = "niraj9.telematics")
public.folder = "/home/niraj9/public/"
master.file.3 = paste(public.folder, "master-3.csv", sep = "")
master.file.4 = paste(public.folder, "master-4.csv", sep = "")

#Loading the telematics object
telematics <- Telematics$new(master.file.3)
telematics$load(verbose = FALSE)

#Creating the driver object
driver = Driver$new(db = telematics, driver.name = "2751")

#Creating the model
model = driver$CreateDriverSignature()
summary(model)


##
## End niraj9 code
##
