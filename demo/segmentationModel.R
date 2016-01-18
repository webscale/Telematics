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

#Driver Class Predictive Model
fit <- telematics$SegmentDrivers(segments = 5)
summary(fit)

#Visualize the cluster
telematics$VisualizeSegments()

##
## End niraj9 code
##
