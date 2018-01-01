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
# Actual computation for the first tab (time-series plots)       
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

observeEvent(c(input$device.checkbox,input$drugs),{

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
                        
                                #####################################################
                                # Input: data frame generated by yearlyPRR
                                drug.data <<- global.values$drug.data
                                drug.device.data <<- global.values$drug.device.data
                                drug.FLAG <<- global.values$drug.FLAG
                                
                                # Set the date range from the date input:
                                year.range <- input$date.range
                                global.values$start.date <- date(format(year.range[1]))
                                global.values$end.date <- date(format(year.range[2]))
                                
                                start.date <- global.values$start.date
                                end.date <- global.values$end.date
                                
                                cat("We make it until here? 1 \n")
                                
                                # Filter the data set based on the selected date range
                                drug.data.temp <<- dplyr::filter(drug.data,end.year >= start.date, end.date >= end.year)
                                
                                cat("We make it until here? 2 \n")
                                cat("drug.FLAG: ", drug.FLAG,"\n")
                                if(drug.FLAG){
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
                                        
                                       
                                              
                                } else if(input$device.checkbox & !is.null(drug.device.data)){

 
                                                        jitter <- rnorm(nrow(drug.device.data), mean = mean(drug.data.temp$PRR), sd = sd(drug.data.temp$PRR) )
                
                                                        a <- drug.device.data$device
                                                        b <- drug.device.data$date_of_event
                                                        c <- drug.device.data$event_type
                                                        d <- drug.device.data$event_location
                                                        e <- sapply(drug.device.data$event_description,substring,1,600) # Curently use first 600 character
                                                    
                
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
                                                                                 aes(x = date_of_event, y= jitter,
                                                                                     text=sprintf("Device: %s<br>Date of event: %s<br>Event type: %s<br>Event location: %s<br>Event description: %s", a,b,c,d,e)))
                
                
                                                        

                                } else {

                                        cat("We make it until here? 3 \n")
                                        
                                        p <- ggplot(data = drug.data.temp , aes(x = end.year , y = PRR))+
                                                geom_line(colour = "purple", size = 2)+
                                                geom_point(colour = "navy", size = 4)+
                                                labs(x="Date", y = "PRR" ,title = paste0("Yearly PRR of adverse event ",gsub("\\+"," ",unique(drug.data.temp$event))," for drug ",unique(drug.data.temp$drug)))+
                                                theme(panel.background=element_rect(fill = "white",colour = "black"),
                                                      panel.border=element_rect(colour = "black", fill = NA),
                                                      plot.title = element_text(hjust = 0.5, face = "bold",size = 15),
                                                      plot.subtitle = element_text(hjust = 0.5, face = "bold",size = 15, color ="red"),
                                                      axis.title = element_text(size = 13, face = "bold"),
                                                      axis.text = element_text(size = 13, face = "bold"))
                                               
                                } 
                                
                                ggplotly(p)
                                
                             },message = "Preparing the timeseries chart") 
                        
         }) # end of the plot
                     
}) # End of the observeEvent

        
observeEvent(c(input$device.checkbox,global.values$drug.device.data),{
        
        if(input$device.checkbox & is.null(global.values$drug.device.data)){  
                error.text <- "Currently no data is available for a companion Dx associated with this drug through openFDA API\nPlease check again at a later time!"
                
                output$PRRplotERROR <- renderText(error.text)
        }else{
                output$PRRplotERROR <- renderText(" ")
        }
})        



# output$PPRplot<- renderPlotly(expr = { 
#         
#         reTab2()        
# 
#         
# })  # renderplotly closure

###############################################################################        
# Actual computation for the second tab (PMA/510k submission maps)       
###############################################################################

output$allPMA <- renderPlotly({

withProgress({                
        PMA.data <- get.state.PMA()
        g <- ggplot(PMA.data, aes(map_id = state.name)) +
                geom_polygon(data=states_map, aes(x=states_map$long, y=states_map$lat, map_id = region),
                             size = 0.25,colour='violet', fill=NA)+
                geom_map(aes(fill = count), map = states_map, color=NA, size = 0.1) +
                scale_fill_continuous(low='thistle2', high='darkred', 
                                      guide='colorbar', name = "Number of\nsubmissions") +
                expand_limits(x = states_map$long, y = states_map$lat)+
                labs(x=NULL, y=NULL,
                     title = "Distribution of submissions across the United States")+
                #coord_map("albers", lat0 = 39, lat1 = 45)+ 
                theme(panel.border = element_blank())+
                theme(panel.background = element_blank())+
                theme(axis.ticks = element_blank())+
                theme(axis.text = element_blank())+
                theme(title = element_text(face = "bold",size = 8)) 
        ggplotly(g)
  
   },message = "Calculating PMAs from openFDA API submitted to date.")              
})



output$all510k <- renderPlotly({
    
withProgress({         
        
        data.510k <- get.state.510k()
        g <- ggplot(data.510k, aes(map_id = state.name)) +
                geom_polygon(data=states_map, aes(x=states_map$long, y=states_map$lat, map_id = region),
                             size = 0.25,colour='lightskyblue', fill=NA)+
                geom_map(aes(fill = count), map = states_map, color=NA, size = 0.1) +
                scale_fill_continuous(low='lightsteelblue2', high='navy',
                                      guide='colorbar', name = "Number of\nsubmissions") +
                expand_limits(x = states_map$long, y = states_map$lat)+
                labs(x=NULL, y=NULL,
                     title = "Distribution of submissions across the United States")+
                #coord_map("albers", lat0 = 39, lat1 = 45)+ 
                theme(panel.border = element_blank())+
                theme(panel.background = element_blank())+
                theme(axis.ticks = element_blank())+
                theme(axis.text = element_blank())+
                theme(title = element_text(face = "bold",size = 8)) 
        ggplotly(g)
      
},message = "Calculating 510(k)s from openFDA API submitted to date.")   

})


output$companionPMA <- renderPlotly({
        
        withProgress({                
                PMA.data.companion <- get.state.PMA.companion()
                g <- ggplot(PMA.data.companion, aes(map_id = state.name)) +
                        geom_polygon(data=states_map, aes(x=states_map$long, y=states_map$lat, map_id = region),
                                     size = 0.25,colour='violet', fill=NA)+
                        geom_map(aes(fill = count), map = states_map, color=NA, size = 0.1) +
                        scale_fill_continuous(low='plum2', high='purple3', 
                                              guide='colorbar', name = "Number of\nsubmissions") +
                        expand_limits(x = states_map$long, y = states_map$lat)+
                        labs(x=NULL, y=NULL,
                             title = "Distribution of submissions across the United States")+
                        #coord_map("albers", lat0 = 39, lat1 = 45)+ 
                        theme(panel.border = element_blank())+
                        theme(panel.background = element_blank())+
                        theme(axis.ticks = element_blank())+
                        theme(axis.text = element_blank())+
                        theme(title = element_text(face = "bold",size = 8)) 
                ggplotly(g)
                
        },message = "Calculating companion Dx PMAs from openFDA API submitted to date.")              
})










})


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
# try to add device data reactively. (ACHIEVED!)
# Next, we need to accomplish 3 goals
# 1. Write a systematic function that compiles relevant device information:(ACHIEVED!)
#                       a. it checks the current drug selected
#                       b. finds the right companion diagnostic(s) associated with the drug
#                       c. searches data associated with the device(s) from openFDA
#                       d. returns a data.frame with relevant data
#                       e. this function should be able to let json errors go and also return a FLAG if no information is found in API query
# 2. Find a neat way to add this time-series data into the drug PRR plot(ACHIEVED!)
# 3. Make this reactive to device.checkbox(ACHIEVED!)
#
# Phase III (Currently on hold because of previous experience)
#
# Add a heatmap after clustering PRR of drugs for a given year period, for numerous adverse events 
# We need to accomplish these goals
# 1. Write a function that will (for a given date range)
#               - extract all events associated with a drug, 
#               - calculate PRR for all adverse events
#               - returns a data.frame with Events and PRR columns
# 
# 2. Run this function for all available drugs
# 3. Merge by adverse event name and aggregate PRR data for all drugs
# 4. Present aggregated PRR matrix as a heatmap (col = drugs rows = adverse.events)
#
#
# Phase 4: Spatial distribution of device developers
# Simply plan to retrieve state data for all 510(k) and PMA submissions and display as U.S. state maps
# We need to accomplish 2 goals:
#  1. Write 3 functions:
#               - get.state.510k : gets state counts for all 510k submissions received to date(ACHIEVED!)
#               - get.state.PMA : gets state counts for all PMA submissions received to date(ACHIEVED!)
#               - get.state.PMA.companion: gets state counts for all PMA submissions for companion Dx received to date (ACHIEVED!)
#               
# 2. Try to display these as 3 distinct U.S. state maps (ACHIEVED!)
# 3. Incorporate them into a new tab (ACHIEVED!)