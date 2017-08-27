#Setup 
#Libraries needed to performe the operation 

#This is an elegant way of calling and automatically install all necessary packages

#list.of.packages <- c("janitor", "lubridate", hms)
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#Setup variables 
#VORTX API Adresses

#Setting up working evnrionment
setwd("~/r2vortx/")
#Standard links
urlCreateJob<-"https://api.vortx.io/jobs/create"
urlJobStart<-"https://api.vortx.io/discoverer/start"

#API-Definition
myAPI<-"O8mXNYEUVtsrixVsH8fiTcHhHkEb9gMRo6pmbbeozS"

#Job Basic Information
#Definition of the dataset that will be add to VORTX
fileToVORTX<-paste(getwd(),"/Data/01-wine.csv", sep = "")

#It is recommended that the analysis have at least 3 parts [Type of the analytics, Dataset, Sample Size]
jobName<-paste("Evaluation", "Wine-bottles", "178 lines", sep = " ")

#Simple Description
jobDescription<-paste("How the Wine can be organized?", jobName," Saved at ", fileToVORTX, " Created at: ", Sys.time(), " using R script", sep = "")

#Preparing the bundle to send
bodyCreateJob<-list(myAPI,jobName,jobDescription,upload_file(fileToVORTX, "text/csv"))

reqCreateJob <- POST(url = urlCreateJob, body = bodyCreateJob,  encode = "multipart", handle = verbose())


library(httr)
bodyCreateJob
content(reqCreateJob)
return(reqCreateJob)



print(CreateJob)
print(myAPI)
print(jobName)
print(jobDescription)
print(upload_file(fileToVORTX, "text/csv"))
print(fileToVORTX)
