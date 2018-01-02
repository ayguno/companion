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
                menuItem("Welcome!",tabName = "welcome",icon = icon("thumbs-o-up"),badgeLabel = "start here",badgeColor = "navy"),
                menuItem("Yearly Drug-Device Data", tabName = "yearlysummary", icon = icon("bar-chart"),
                
                        menuSubItem("Click here to start analysis!",tabName = "yearlysummary", selected =NULL),
                                selectInput("drugs", label = "Select a drug",choices = drug.list , selected = "herceptin"),
                                selectInput("events", label = "Select a drug adverse effect",choices = event.list , selected = event.list[3]),
                                checkboxInput("device.checkbox","Add Companion Dx Data?"),
                                dateRangeInput("date.range", label = "Select a date range", start = "1998-01-01", end = as.character(Sys.Date())), br()
                
                        
                        ),
                
                menuItem("Visualize PMAs and 510(k)s",tabName = "industry",icon = icon("thumbs-o-up")),
                hr(),

                menuItem("Get the Sourcecode", icon = icon("github"), tabName = "sourcecode"),
                menuItem("Support and bug reports",icon=icon("medkit"),tabName = "support"),
            
                hr()
                         
                )# End of sidebarMenu structure 
        ), #End of dashboardSidebar
        
        dashboardBody(
                
                tags$head(tags$style(HTML('.shiny-notification {
                                              background-color:#360160;
                                              color: #fff;
                                              font-size: 15pt;
                                              height: 100px;
                                              width: 700px;
                                              position:fixed;
                                              top: calc(50% - 50px);;
                                              left: calc(60% - 400px);;
                                            }
                                          .content-wrapper{
                                          background: #5e0ca0;
                                          }
                                          .skin-purple .main-header .navbar-static-top{
                                          background-color: #8710bc;
                                          }
                                          .box.box-solid.box-primary>.box-header {
                                          color: #fff;
                                          background: #5e0ca0
                                          }
                                          .box.box-solid.box-primary{
                                          border-bottom-color: #fff;
                                          border-left-color: #fff;
                                          border-right-color: #fff;
                                          border-top-color: #fff;
                                          color: #fff;
                                          background-color: #5e0ca0;
                                          }
                                          .skin-purple .main-header .logo{
                                          background-color: #8710bc;
                                          }
                                          .skin-purple .main-sidebar {
                                          background-color: #5e0ca0;
                                          color:#fff
                                          }
                                          .skin-purple .sidebar-menu>li.active>a, .skin-purple .sidebar-menu>li:hover>a {
                                          background-color: #1fd30e;
                                          }
                                          .skin-purple .main-sidebar {
                                          font-size: 13pt;
                                          }
                                          .skin-purple .main-sidebar .sidebar .sidebar-menu .treeview-menu {
                                          background-color: #8710bc;
                                          }                
                                          @import url("//fonts.googleapis.com/css?family=Lobster|Cabin:400,700");
                                          }
                                          ')),
                
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
                                     }
                                     "
                                    )
                                
                                
                                )
                           ),
                tabItems(
                
          
                  #Welcome-tab content  
                  tabItem(tabName = "welcome",class = "active",
                    column(4,{}),
                           h1("Welcome to Companion!", 
                              style = "font-family: 'Lobster', cursive;
                             font-weight: 500; line-height: 1.1; 
                             color: #fff;"),
                    column(4,{}),
                  
                   br(),
                   box(title = "Welcome to Companion!",
                   width = 12,solidHeader = T,status ="primary",

                   h4("Use data analytics to monitor adverse effects associated with anti-cancer drugs and companion diagnostics"),
                   tags$hr()
                  
                   # h5(icon("thumbs-o-up"),"Mass Spectrometry users inject
                   #    1 ug tryptic Jurkat cell peptide mixture to instruments and run a 110 min 
                   #    gradient with a standardized LC-MS/MS run method.",br(),br(),
                   #    icon("thumbs-o-up"),"This provides an objective quality control
                   #    that helps to monitor instrument performance by using powerful
                   #    Quality Metrics provided by Spectrum Mill.",br(),br(),
                   #    icon("thumbs-o-up"),"JurkatQC Scraper actively monitors 
                   #    Broad Proteomics servers to detect new Jurkat Quality Metrics,
                   #    provided that the raw files were searched in Spectrum Mill.",br(),br(),
                   #    icon("thumbs-o-up"),"JurkatQC Scraper archives Quality Metrics data,
                   #    extracts useful LC labels, time and user attributes, and present helpful data analytics.",br(),br(),
                   #    icon("thumbs-o-up"),"JurkatQC Scraper also provides a user interface to obtain, store and present
                   #    mass spectrometry user comments and maintanence records.",br(),br(),
                   #    icon("thumbs-o-up"),"Effective from Version 0.1.0, JurkatQC Scraper also monitors
                   #    instrument active archive records to estimate Mass Spectrometer downtime. It also integrates
                   #    the instruments' operational status with Jurkat QC quality metrics."))
                  
                )),  
                    
                ######################    
                # First tab content:yearlysummary
                ######################
                tabItem(tabName = "yearlysummary",class= "active",

                    box(width= 12,title = "Summary of Drug and Device Adverse Events",
                        status = "primary",solidHeader = TRUE,
                        
                        column(1,{}),htmlOutput("PRRplotERROR"), br(),
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
                       
                ),# End of yearlysummary tab
                
                ######################    
                # Second tab content: industry
                ######################
                tabItem(tabName = "industry",class= "active",
               
                        box(width= 6,title = "Distribution of all PMA submissions to date",
                            status = "primary",solidHeader = TRUE, 
                            
                            plotlyOutput("allPMA", height = "250px")
                        ),
                        box(width= 6,title = "Distribution of all 510(k) submissions to date",
                            status = "primary",solidHeader = TRUE,
                            
                            plotlyOutput("all510k", height = "250px")
                        ),
                        box(width= 6,title = "Distribution of companion Dx PMA submissions to date",
                            status = "primary",solidHeader = TRUE,
                            
                            plotlyOutput("companionPMA", height = "250px")
                        )
                        
                )# End of industry tab
                
            )# End of tabItems structure      
        )  # End of dashboard body
        
)  # End of dashboard page     
)  # end of UI      

        