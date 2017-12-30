################################################################################
# Author: Ozan Aygun
# Date: 12/27/2017 
#
# Purpose: this is the server logic code for the Shiny app "companion Ver 0.1"
# 
################################################################################

library(shiny)
library(shinydashboard)
library(googleVis)



shinyServer(function(input, output,session) {
        
        options(shiny.maxRequestSize=30*1024^2) #to the top of server.R 
        # this would increase the upload limit to 30MB    

        
        #######################################################################
        # Reactive values to store and access across a session
        #######################################################################
        
        global.values <- reactiveValues()
        global.values$start.date = "1998-01-01" 
        global.values$end.date = as.character(Sys.Date())
        global.values$drug.data = NULL
        global.values$event.list = NULL
        global.values$drug.FLAG = FALSE
        global.values$drug.device.data = NULL
                                        
        
        
              
        
withProgress({
        isolate({
                
                #############################################################################################
                # Reads the available PPR data for Herceptin and FATIGUE just the initialize the application  
                #############################################################################################
                
                #If archive exists, read from the local folder (when running the app locally for development and troubleshooting)
                
                if(file.exists("herceptin_data.rds")) {
                        global.values$drug.data <- readRDS("herceptin_data.rds")
                }
                
                # If archive is not found get it from openFDA API (normally intended)
                if(!file.exists("herceptin_data.rds")) {
                       # Parse the data from openFDA API
                       global.values$drug.data <- yearlyPRR("FATIGUE",
                                                            "Herceptin",
                                                            global.values$start.date,
                                                            global.values$end.date)
                }
       
        }) },message = "Fetching data from openFDA API")
        
        
                
        
###############################################################################        
# Actual computation for the welcome tab       
###############################################################################        
        

        
        
###############################################################################        
# Actual computation for the second tab (time-series plots)       
###############################################################################

# 1. Drug selection observes User and just updates global.values$event.list 
observeEvent(input$drugs,{
        drug <- as.character(input$drugs)
withProgress(expr = {        
        
        
        start.date <- as.character(global.values$start.date)
        end.date <- as.character(global.values$end.date)
        
        try(global.values$event.list <- queryEvents(drug = drug, 
                                                start.date,
                                                end.date)) 
        
        },message = paste0("Parsing information about ",drug," from openFDA API"))
        
}, ignoreInit = TRUE) #End of Drug Selection observer


# 2. Fetch and Update events observes outcome of #1, and updates event selection input 
observeEvent(global.values$event.list,{
       updateSelectInput(session = session,
                         inputId = "events",
                         label = "Select a drug adverse effect",
                         choices = event.list,
                         selected = event.list[3])
}, ignoreInit = TRUE)


# 3. Update the drug.data (observes any change in event selection input as a result of #2)
observeEvent(c(input$events,input$drugs),{
        
        drug <- as.character(input$drugs)
        event <- as.character(input$events)
 
withProgress(expr = {          
            
        
        cat("drug is: ",drug,"\n")
        cat("global.values$drug is: ",global.values$drug,"\n")
        
        
        start.date <- as.character(global.values$start.date)
        end.date <- as.character(global.values$end.date)
        
        
        
        is.error <<- as.character(unlist(try(global.values$drug.data <- yearlyPRR(event,
                                             drug,
                                             start.date,
                                             end.date)))) 
        
        if(is.error == 'Error in open.connection(con, \"rb\") : HTTP error 404.\n'){
                global.values$drug.FLAG <- TRUE #FLAG when a drug does not bring a productive query from the database
        }else{
                global.values$drug.FLAG <- FALSE
        }
        
        cat("For number 3: \n",
            "global.values$drug.FLAG : ", global.values$drug.FLAG, "\n")
        
        
        cat("For number 3 \n",
            "3 is error? : ", is.error, "\n")
        
        cat("For number 3 \n",
            event,"\n",
            drug, "\n",
            start.date,"\n",
            end.date)
        
        
        },message = paste0("Calculating yearly PRR of ",event," for ",drug))
        
}, ignoreInit = TRUE)

#############################################################
# Device data observer: observes drug selection and checkbox:
#############################################################

# This part is problematic!!! Can not get drug.device.data out!

observeEvent(input$device.checkbox,{

        drug <- as.character(input$drugs)
        start.date <- as.character(global.values$start.date)
        end.date <- as.character(global.values$end.date)

        if(input$device.checkbox){
                # Can not get this object properly!!!
                global.values$drug.device.data <- queryDeviceData(drug = drug,
                                                    start.date = start.date,
                                                    end.date = end.date)
                

        }



}, ignoreInit = TRUE)

# 4. Update the plot (observe 3 and date)


observeEvent(c(input$date.range,global.values$drug.data,input$drugs, global.values$drug.FLAG,input$device.checkbox),{ 
                
                        
                        
         output$PPRplot<- renderPlotly({  
                               
                        withProgress(expr = {
                                
                                cat("For number 4 \n",
                                    "global.values$drug.FLAG : ", global.values$drug.FLAG, "\n")
                                
                                
                                
                                ######################################################################
                                # drug.data <- Herceptin.Data[Herceptin.Data$event == "FATIGUE",] # Use for the example, generalize later
                                ######################################################################
                                # Phase I
                                # Next, we need to accomplish 3 goals:
                                # 1. Make the plots reactive to time choice (include a global date input selector into sidebar) (ACHIEVED!)
                                # 2. Make the plots reactive to drug and event choices:
                                #                               - this will require these aspects:
                                #                               a. Get the limited list of drugs from companion diagnostics table to be searched in the openFDA API
                                #                               b. Make 2 reactive selectInput panels:  (ACHIEVED!)
                                #                                       - Drug
                                #                                       - Event (dependent on the Drug)
                                #                               c. Whenever a drug is choosen, update the adverse event list for that drug (ACHIEVED!)
                                #                               d. Make the yearlyPRR and this plot reactive to BOTH time, drug and event (ACHIEVED!)
                                
                                # # Phase II
                                # try to add device data reactively. 
                                # Next, we need to accomplish 3 goals
                                # 1. Write a systematic function that compiles relevant device information:
                                #                       a. it checks the current drug selected
                                #                       b. finds the right companion diagnostic(s) associated with the drug
                                #                       c. searches data associated with the device(s) from openFDA
                                #                       d. returns a data.frame with relevant data
                                #                       e. this function should be able to let json errors go and also return a FLAG if no information is found in API query
                                # 2. Find a neat way to add this time-series data into the drug PRR plot
                                # 3. Make this reactive to device.checkbox
                                
                                
                                
                                ##################################################################################
                                # Input: data frame generated by yearlyPRR
                                drug.data <- global.values$drug.data
                                
                                
                                # Set the date range from the date input:
                                year.range <- input$date.range
                                global.values$start.date <- date(format(year.range[1]))
                                global.values$end.date <- date(format(year.range[2]))
                                
                                start.date <<- global.values$start.date
                                end.date <<- global.values$end.date
                                
                                cat("We make it until here? 1 \n")
                                
                                # Filter the data set based on the selected date range
                                drug.data.temp <- dplyr::filter(drug.data,end.year >= start.date, end.date >= end.year)
                                
                                cat("We make it until here? 2 \n")
                                
                                if(global.values$drug.FLAG){
                                        error.text <- "Currently no adverse event data is available for this drug through openFDA API\n
                                                                Please check again at a later time!"
                                        df <- data.frame(x = 5, y = 50)
                                        p <- ggplot(df, aes(x,y)) + geom_blank() + xlim(0, 10) + ylim(0, 100)+geom_text(label = error.text,size = 4.5, color ="red")+
                                                labs(x="Date", y = "PRR")+
                                                theme(panel.background=element_rect(fill = "white",colour = "black"),
                                                      panel.border=element_rect(colour = "black", fill = NA),
                                                      plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
                                                      axis.title = element_text(size = 13, face = "bold"),
                                                      axis.text = element_text(size = 13, face = "bold"))
                                
                                        }else if(input$device.checkbox){

                                                        drug.device.data <<- global.values$drug.device.data
                                                        
                                                        # Convert the date feature to proper date
                                                        for(k in 1:nrow(drug.device.data)){
                                                                year <- substr(drug.device.data$date_of_event[k],1,4)
                                                                month <- substr(drug.device.data$date_of_event[k],5,6)
                                                                day <- substr(drug.device.data$date_of_event[k],7,8)
                                                                ymd.date <- paste(year,month,day,sep = "-")
                                                                drug.device.data$date_of_event[k] <- ymd.date
                                                        }
                
                                                        drug.device.data$date_of_event <- date(format(drug.device.data$date_of_event))
                
                                                        jitter <- rnorm(nrow(drug.device.data), mean = mean(drug.data.temp$PRR), sd = sd(drug.data.temp$PRR) )
                
                                                        a <- drug.device.data$device
                                                        b <- drug.device.data$date_of_event
                                                        c <- drug.device.data$event_type
                                                        d <- drug.device.data$event_location
                                                        e <- drug.device.data$event_description
                
                
                                                        p <- ggplot(data = drug.data.temp , aes(x = end.year , y = PRR))+
                                                                geom_line(colour = "purple", size = 2)+
                                                                geom_point(colour = "navy", size = 4)+
                                                                labs(x="Date", y = "PRR", title = paste0("Yearly PRR of adverse event ",gsub("\\+"," ",unique(drug.data$event))," for drug ",unique(drug.data$drug)))+
                                                                theme(panel.background=element_rect(fill = "white",colour = "black"),
                                                                      panel.border=element_rect(colour = "black", fill = NA),
                                                                      plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
                                                                      axis.title = element_text(size = 13, face = "bold"),
                                                                      axis.text = element_text(size = 13, face = "bold"))+
                                                                      geom_point(data = drug.device.data,shape= 17,size = 8,color ="red",
                                                                                 aes(x = date_of_event, y= jitter))
                                                                                     #text=sprintf("Device: %s<br>Date of event: %s<br>Event type: %s<br>Event location: %s<br>Event description: %s", a,b,c,d,e)))
                
                
                

                                }else{
                                
                                
                                        p <- ggplot(data = drug.data.temp , aes(x = end.year , y = PRR))+
                                                geom_line(colour = "purple", size = 2)+
                                                geom_point(colour = "navy", size = 4)+
                                                labs(x="Date", y = "PRR", title = paste0("Yearly PRR of adverse event ",gsub("\\+"," ",unique(drug.data$event))," for drug ",unique(drug.data$drug)))+
                                                theme(panel.background=element_rect(fill = "white",colour = "black"),
                                                      panel.border=element_rect(colour = "black", fill = NA),
                                                      plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
                                                      axis.title = element_text(size = 13, face = "bold"),
                                                      axis.text = element_text(size = 13, face = "bold"))
                                }      
                              
                                
                                
                                ggplotly(p)
                                
                             },message = "Preparing the timeseries chart") 
                        
         }) # end of the plot
                     
}) # End of the observeEvent

        
        



# output$PPRplot<- renderPlotly(expr = { 
#         
#         reTab2()        
# 
#         
# })  # renderplotly closure






})