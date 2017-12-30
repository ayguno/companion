################################################################################
# Author: Ozan Aygun
# Date: 12/27/2017 
#
# Purpose: this is the ui definition code for the Shiny app "companion Ver 0.1"
# 
# The ui interface is established by shinydashboard package in the form of
# a dashbard page. 
################################################################################


library(googleVis)
library(shiny)
library(shinydashboard)


shinyUI( dashboardPage( skin = "purple",
        
        dashboardHeader(title = "Companion Version 0.1", titleWidth = 350),
       
               
       
        dashboardSidebar(width = 330,
            sidebarMenu(id="tabitems",  
                hr(color = "blue"),
                menuItem("Welcome!",tabName = "welcome",icon = icon("thumbs-o-up"),badgeLabel = "start here",badgeColor = "blue"),
                menuItem("Yearly Drug-Device Data", tabName = "yearlysummary", icon = icon("bar-chart"),
                
                        menuSubItem("Click here to start analysis!",tabName = "yearlysummary", selected =NULL),
                                selectInput("drugs", label = "Select a drug",choices = drug.list , selected = "herceptin"),
                                selectInput("events", label = "Select a drug adverse effect",choices = event.list , selected = event.list[3]),
                                checkboxInput("device.checkbox","Add Companion Dx Data?"),
                                dateRangeInput("date.range", label = "Select a date range", start = "1998-01-01", end = as.character(Sys.Date()))
                
                        
                        ),
                
                hr(),

                menuItem("Get the Sourcecode", icon = icon("github"), tabName = "sourcecode"),
                menuItem("Support and bug reports",icon=icon("medkit"),tabName = "support"),
            
                hr()
                         
                )# End of sidebarMenu structure 
        ), #End of dashboardSidebar
        
        dashboardBody(
                tags$head(
                        tags$script(
                                HTML("
                                     window.onload = function() {
                                        resize();
                                     }
                                     window.onresize = function() {
                                     resize();
                                     }
                                     Shiny.addCustomMessageHandler ('triggerResize',function (val) {
                                     window.dispatchEvent(new Event('resize'));
                                     });
                                     function resize(){
                                     var h = window.innerHeight - $('.navbar').height() - 150; // Get dashboardBody height
                                     $('#box').height(h); 
                                     }"
                                        )
                                )
                                ),
                tabItems(
                
          
                  #Welcome-tab content  
                  tabItem(tabName = "welcome",class = "active",
                   fluidRow( 
                    
                   ),
                  
                   
                   box(title = "Welcome to Companion!",
                   width = 12,solidHeader = TRUE,status ="primary",background = "purple",

                   h4("Use data analytics to monitor adverse effects associated with anti-cancer drugs and companion diagnostics"),
                   tags$hr(),
                  
                   h5(icon("thumbs-o-up"),"Mass Spectrometry users inject
                      1 ug tryptic Jurkat cell peptide mixture to instruments and run a 110 min 
                      gradient with a standardized LC-MS/MS run method.",br(),br(),
                      icon("thumbs-o-up"),"This provides an objective quality control
                      that helps to monitor instrument performance by using powerful
                      Quality Metrics provided by Spectrum Mill.",br(),br(),
                      icon("thumbs-o-up"),"JurkatQC Scraper actively monitors 
                      Broad Proteomics servers to detect new Jurkat Quality Metrics,
                      provided that the raw files were searched in Spectrum Mill.",br(),br(),
                      icon("thumbs-o-up"),"JurkatQC Scraper archives Quality Metrics data,
                      extracts useful LC labels, time and user attributes, and present helpful data analytics.",br(),br(),
                      icon("thumbs-o-up"),"JurkatQC Scraper also provides a user interface to obtain, store and present
                      mass spectrometry user comments and maintanence records.",br(),br(),
                      icon("thumbs-o-up"),"Effective from Version 0.1.0, JurkatQC Scraper also monitors
                      instrument active archive records to estimate Mass Spectrometer downtime. It also integrates
                      the instruments' operational status with Jurkat QC quality metrics."),
                  
                   
                   
                   fluidRow(
                   
                   column(1,{}),                      
                        
                   actionLink("link_to_downtime",
                              label = uiOutput("downtime_box"),width = 3),
                      
                   infoBox(title="Sourcecode",color = "purple",width = 4,
                            icon = icon("github"), value = "Find us on GitHub!",subtitle = "Access the sourcecode",
                            href = "https://github.com/ayguno/JurkatQCScraper"),
                   
                   actionLink("link_to_tutorial",
                              label = uiOutput("tutorial_box"),width = 3),
                   
                   column(1,{}) 
                   
                   )
                   
                   
                   
                   
                   ),

                   
                   box(title = "Choose an app to explore the latest Jurkat QC data
                       generated by the mass spectrometers in the Broad Proteomics Platform",
                   width = 12,solidHeader = TRUE, status = "danger",
                   background = "navy",
                   
                   fluidRow(
                   
                   # Generating dynamic InfoBoxes as action links
                   actionLink("link_to_QC_summary",
                              label = uiOutput("QC_summary_box"),width =4),
                   actionLink("link_to_timeseries",
                              label = uiOutput("timeseries_box"),width =4),
                   actionLink("link_to_current",
                              label = uiOutput("current_box"),width =4)
                   
                   ),
                   
                   fluidRow(
                           
                           # Generating dynamic InfoBoxes as action links
                           actionLink("link_to_thelatest",
                                      label = uiOutput("thelatest_box"),width =4),
                           actionLink("link_to_facts",
                                      label = uiOutput("facts_box"),width =4),
                           actionLink("link_to_instrument_centric",
                                      label = uiOutput("instrument_centric_box"),width =4)
                           
                   ),
                   
                   fluidRow(
                           column(2,{}),
                           actionLink("link_to_thelatest2",
                                      label = uiOutput("comment_box"),width =3),
                           actionLink("link_to_support",
                                      label = uiOutput("support_box"),width =2)
                   )
                   

                   
                   )
                   
                          
                   
                ),  
                    
                ######################    
                # First tab content
                ######################
                tabItem(tabName = "yearlysummary",class= "active",

                    box(width= 12,title = "Summary of Drug and Device Adverse Events",
                        status = "primary",solidHeader = TRUE,
                        htmlOutput("PRRplotERROR"),
                        plotlyOutput("PPRplot")
                    
                    ),
     
                                box(title= "Tips for using this APP" ,
                                    width = 12,background = "navy",solidHeader = TRUE,
                                    status = "success",
                                      
                                column(6,         
                                          h5(icon("calendar")," The data describes the daily summary of
                                          the JurkatQC data obtained real-time.
                                          Each parameter is described as the 'Median'
                                          of the daily records observed for each instrument.
                                          ", br(),br(),
                                          icon("bar-chart")," The default view is a bar chart, which displays
                                          the median value of the selected quality metrics for a given date.",br(),br(),
                                          icon("sliders")," Use the sliding bar on the x-axis to change the dates.",br(),br(),
                                          icon("gears")," Click Y-axis label to switch between different quality metrics.")#, br(),br(),
                                        ),
                                       
                                       column(6,
                                          h5(icon("play")," For bar-chart only: click to play button on the x-axis to monitor changes over time in motion!",br(),br(),
                                          icon("line-chart")," Use the chart switch on the top right to change into a line-chart summary.", br(), br(),
                                          icon("thumbs-o-up"), " You can log transform the data by clicking top of the Y-axis.",br(),br(),
                                          icon("check-square-o"), " You can choose to display the data for all or some of the 
                                          instruments from the menu on the left side of the chart."))  
                                
                                )     
                       
                )
                
               
                  
            )# End of tabItems structure      
        )  # End of dashboard body
        
)  # End of dashboard page     
)  # end of UI      

        