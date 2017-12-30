################################################################################
# Author: Ozan Aygun
# Date: 12/27/2017 
#
# Purpose: this is the global code for the Shiny app "companion Ver 0.1"
# The global functions will be defined here.
# Libraries to run the app are also launched here.
################################################################################


#################################################################
## global parameters
#################################################################

## app name
APPNAME <<- sub('.*/','',getwd())

library(RColorBrewer)
library(shiny)
library(plotly)
library(colorspace)
library(dplyr)
library(ape)
library(googleVis)
library(lubridate)
library(shinydashboard)
library(stringr)
require(jsonlite)


# Drug list to be used in the selectInput
drug.list <- unique(as.character((readRDS("drug_list.rds"))))


# Initial event.list for Herceptin to be used for selectInput
event.list <- readRDS("Herceptin.events.list.rds")

initial.drug <- "Herceptin"


###################
# Global Functions
###################

yearlyPRR <- function(event,drug, start.date = "1998-01-01", end.date = as.character(Sys.Date())){
        
        # Returns yearly PRR scores for a given drug for a given event

        end.date <- gsub("-","", end.date)
        start.date <- gsub("-","", start.date)
        
        drug <- toupper(drug)
        event <- gsub(" ","+",toupper(event))
        
        #######################################################
        #This requires internet connection!
        #Perform openFDA API inquiries to obtain relevant data
        ########################################################
        reports_with_drug_and_event <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.drug.openfda.brand_name:(%22',drug,'%22)+AND+patient.reaction.reactionmeddrapt.exact=',event,'&count=receivedate'))$results
        reports_with_drug_and_event$year <- substr(as.character(reports_with_drug_and_event$time), 1,4)
        reports_with_drug_and_event <- reports_with_drug_and_event %>% group_by(year) %>% summarise(reports_with_drug_and_event = sum(count))
        
        reports_with_drug <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.drug.openfda.brand_name:(%22',drug,'%22)&count=receivedate'))$results
        reports_with_drug$year <- substr(as.character(reports_with_drug$time), 1,4)
        reports_with_drug <- reports_with_drug %>% group_by(year) %>% summarise(reports_with_drug = sum(count)) 
        
        reports_with_event_in_database <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.reaction.reactionmeddrapt.exact=',event,'&count=receivedate'))$results
        reports_with_event_in_database$year <- substr(as.character(reports_with_event_in_database$time), 1,4)
        reports_with_event_in_database <- reports_with_event_in_database %>% group_by(year) %>% summarise(reports_with_event_in_database = sum(count))
        
        reports_in_database <- fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']&count=receivedate'))$results
        reports_in_database$year <- substr(as.character(reports_in_database$time), 1,4)
        reports_in_database <- reports_in_database %>% group_by(year) %>% summarise(reports_in_database = sum(count))
        
        # Merge the above reports based on years 
        temp <- merge(reports_with_drug_and_event,reports_with_drug,by = "year", all.x = T, all.y = F, sort = F)
        temp <- merge(temp,reports_with_event_in_database,by = "year", all.x = T, all.y = F, sort = F)
        temp <- merge(temp,reports_in_database,by = "year", all.x = T, all.y = F, sort = F)
        
        # Calculate PRR for each year:
        # PRR = (m/n)/( (M-m)/(N-n) )
        # Where; 
        m = temp$reports_with_drug_and_event #reports with drug and event
        n = temp$reports_with_drug #reports with drug
        M = temp$reports_with_event_in_database #reports with event in database
        N = temp$reports_in_database #reports in database
        
        temp$PRR <- (m/n)/( (M-m)/(N-n) )
        
        temp$drug <- drug
        temp$event <- event
        
        # Set the yearly points as the end of the year
        temp$end.year <- date(format(paste(temp$year,"12","30",sep = "-")))
        
        # Return a nice dataframe with 
        return(temp)         
}

####################################################
# A function to update events list for a given drug
####################################################

queryEvents <- function(drug,start.date,end.date){

        end.date <- gsub("-","", end.date)
        start.date <- gsub("-","", start.date)
        drug <- toupper(drug)
        
        # General query to bring events list for a given drug
        events.list <- unique(fromJSON(paste0('https://api.fda.gov/drug/event.json?search=receivedate:[',start.date,'+TO+',end.date,']+AND+patient.drug.openfda.brand_name:(%22',drug,'%22)&count=patient.reaction.reactionmeddrapt.exact'))$results$term)
        
        return(events.list)
}

##########################################################################
# Write a systematic function that compiles relevant device information:
##########################################################################
        #                       a. it checks the current drug selected
        #                       b. finds the right companion diagnostic(s) associated with the drug
        #                       c. searches data associated with the device(s) from openFDA
        #                       d. returns a data.frame with relevant data
        #                       e. this function should be able to let json errors go and also return a FLAG if no information is found in API query

queryDeviceData <- function(drug,
                            start.date = "1998-01-01", 
                            end.date = as.character(Sys.Date()),
                            drug.device.table = readRDS("drug.device.table.rds")){
        
        companions <- unique(drug.device.table$Device.Trade.Name[which(drug.device.table$Drug.Trade.Name == tolower(drug))])
        
        device.event.data <- data.frame(device = "",
                                        date_of_event = "",
                                        event_type = "",
                                        event_location ="",
                                        event_description = "")
        
        for(i in seq_along(companions)){
                
                is.this.error <- try(device.event.query <- fromJSON(paste0('https://api.fda.gov/device/event.json?search=device.brand_name:',companions[i],'&limit=100'))$results)
                
                
                if(is.this.error != 'Error in open.connection(con, \"rb\") : HTTP error 400.\n'){
                        event.text.vector <- sapply((sapply(device.event.query$mdr_text,function(x) return(x$text))),function(x) return(x[1]))
                        temp <- data.frame(device = companions[i],
                                           date_of_event = device.event.query$date_of_event,
                                           event_type = device.event.query$event_type,
                                           event_location = device.event.query$event_location,
                                           event_description = event.text.vector)
                        
                        device.event.data <- rbind(device.event.data,temp) 
                        
                }       
                
        }
        
        if((nrow(device.event.data) == 1)){
                
                return(FALSE) # This means error! :this function should be able to let json errors go and also return a FLAG if no information is found in API query
        }else{
                device.event.data <- device.event.data[-1,] # remove the first row
                
                # Convert all features to chr:
                for(j in 1:ncol(device.event.data)){
                        device.event.data[,j] <- as.character(device.event.data[,j])
                }
        
                 
                return(device.event.data)
        }
        
} # End of queryDeviceData