##
## Begin niraj9 code
##


library(package = "niraj9.telematics")
public.folder = "/home/niraj9/public/"
master.file.3 = paste(public.folder, "master-3.csv", sep = "")
master.file.4 = paste(public.folder, "master-4.csv", sep = "")

telematics <- Telematics$new(master.file.4)
telematics$CreateMasterSummary(file.list = "/home/niraj9/public/fileList.csv" ,
                                      root.folder = "/home/niraj9/public/drivers/",
                                      subset.driver.count = 5 ,
                                      cores =2)

##
## End niraj9 code
##
