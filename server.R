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


# 4. Update the plot (observe 3 and date)


observeEvent(c(input$date.range,global.values$drug.data,input$drugs, global.values$drug.FLAG),{ 
                
                        
                        
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
                                
                                
                                
                                
                                
                                ##################################################################################
                                # Input: data frame generated by yearlyPRR
                                drug.data <- global.values$drug.data
                                
                                
                                # Set the date range from the date input:
                                year.range <- input$date.range
                                global.values$start.date <- date(format(year.range[1]))
                                global.values$end.date <- date(format(year.range[2]))
                                
                                start.date <- global.values$start.date
                                end.date <- global.values$end.date
                                
                                # Filter the data set based on the selected date range
                                drug.data.temp <- dplyr::filter(drug.data,end.year >= start.date, end.date >= end.year)
                                
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