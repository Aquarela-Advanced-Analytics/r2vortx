#Setup 
#Libraries needed to performe the operation 

#This is an elegant way of calling and automatically install all necessary packages

#list.of.packages <- c("janitor", "lubridate", hms)
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#Setup variables 
#VORTX API Adresses
urlCreateJob<-"https://api.vortx.io/jobs/create"
urlJobStart<-"https://api.vortx.io/discoverer/start"


