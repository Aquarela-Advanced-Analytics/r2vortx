#Setup 
#Libraries needed to performe the operation 

#This is an elegant way of calling and automatically install all necessary packages

#list.of.packages <- c("janitor", "lubridate", hms)
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#Setup variables 
#VORTX API Adresses

setwd("~/r2tvortx/")
getwd()

getwd()
urlCreateJob<-"https://api.vortx.io/jobs/create"
urlJobStart<-"https://api.vortx.io/discoverer/start"
 



#Job Casic Information
sampleSize<-1000
jobName<-paste("Comparativo2-", sampleSize, sep = "")
fileToVORTX<-paste("~/Downloads/",jobName, ".csv", sep = "")
myAPI<-"O8mXNYEUVtsrixVsH8fiTcHhHkEb9gMRo6pmbbeozS87"
jobDescription<-paste(
  "Find out what are the key influence factors of no-show in medical",
  jobName," Saved at ", fileToVORTX, " Created at: ", Sys.time(), " using R script", sep = "")



base_de_dados_2010_2014 <- rio::import(file = "/home/j/Downloads/IBGE/base_de_dados_2010_2014.xls", which = 1L)

