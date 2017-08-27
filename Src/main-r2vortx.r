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
urlCreateJob<-"https://api.vortx.io/jobs/create"
urlJobStart<-"https://api.vortx.io/discoverer/start"

#It is recommended that the analysis have at least 3 parts [Type of the analytics, Dataset, Sample Size]
jobName<-paste("Evaluation", "Wine-bottles", "178 lines", sep = " ")

#Definition of the dataset that will be add to VORTX
fileToVORTX<-paste(getwd(),"/Data/01-wine.csv", sep = "")

#API-Definition
myAPI<-"O8mXNYEUVtsrixVsH8fiTcHhHkEb9gMRo6pmbbeozS87"


#Job Basic Information
jobDescription<-paste(
  "What VORTX says about the Wine characterists?", jobName," Saved at ", fileToVORTX, " Created at: ", Sys.time(), " using R script", sep = "")



base_de_dados_2010_2014 <- rio::import(file = "/home/j/Downloads/IBGE/base_de_dados_2010_2014.xls", which = 1L)

