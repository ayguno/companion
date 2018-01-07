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
       
               
       
        dashboardSidebar(width = 300,
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

                menuItem("Get the Sourcecode", icon = icon("github"), href = "https://github.com/ayguno"),
                menuItem("Data source API",icon=icon("medkit"), href = "https://open.fda.gov/drug/event/reference/"),
            
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
                                          border-bottom-color: #1fd30e;
                                          border-left-color: #1fd30e;
                                          border-right-color: #1fd30e;
                                          border-top-color: #1fd30e;
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
                    # column(4,{}),
                    #        h1("Welcome to Companion!", 
                    #           style = "font-family: 'Lobster', cursive;
                    #          font-weight: 500; line-height: 1.1; 
                    #          color: #fff;"),
                    #column(4,{}),
                  
                   #br(),
                   
                   #column(2,{}),
                   # box(#title = "Use data analytics to monitor adverse effects associated with selected anti-cancer drugs and their companion diagnostics",
                   #  width = 8,solidHeader = T,status ="primary",
                   # column(1,{}),
                   # h5("Use data analytics to monitor adverse effects associated with 
                   #    selected anti-cancer drugs and their companion diagnostics",
                   #    style = "color: #fff;"),
                   fluidRow(
                   column(6,{}),
                   tags$img(src='workflow.jpg', height = 500, width = 640 ),
                   column(2,{})
                   ),
                #),
                br(), br(),
                box(#title = "Disclaimer: not for clinical use",
                    width = 12,solidHeader = F,background = "purple",height = "100px",
                    h4("Disclaimer: not for clinical use"),
                    h5("The data presented here has not been extensively validated or verified. 
                       Therefore, it can not be used to establish a causal relationship between a product and adverse events. 
                       Therefore, it should not be considered as medically validated and should not be used for clinical decision making.
                       For a full disclaimer of the data please visit the data source by following the 'data source API' link on the left.")
                    )  
                 
                ),   
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
                   width = 6,background = "navy",solidHeader = TRUE,collapsible = T,
                   status = "success",
                                      
                                     
                                          h5(icon("line-chart")," The data describes the yearly summary of
                                          Proportional reporting ratio (PPR) calculated from data obtained real-time
                                          from openFDA API.", br(),br(),
                                          icon("bar-chart")," Select a drug and an adverse event to follow. Adverse events
                                          list will be updated for a given drug selection.",br(),br(),
                                          icon("check-square-o")," Use the checkbox to add data from companion diagnostics
                                          associated with the drug. When clicked, this will present each adverse event
                                          associated with a companion diagnostics that is linked to the selected drug.",br(),br(),
                                          icon("check-square-o"),"The events are reported on the dates represented by the date axis, but they are randomly scattered
                                          across the y-axis to improve data analysis",br(),br(),
                                          icon("gears")," The plots are interactive: hover the data points to get information about 
                                          the events.",br(),br(),
                                          icon("calendar")," Use the calendar provided to restrict your analysis to specific dates. Once a new date
                                          is specified, a new analysis will be performed using the date range you specified." )#, br(),br(),
                                ),
                                       
                    box(title= "What is Proportional reporting ratio (PPR)?" ,
                        width = 6,background = "navy",solidHeader = TRUE,collapsible = T,
                        status = "success",
                                       
                                          h5(icon("play"),"PPR is a common metric that is used to measure 
                                             the adverse events associated with a drug.",br(),br(),
                                          icon("line-chart")," Essentially PRR is defined as the ratio BETWEEN the frequency of a
                                          specific adverse event is reported for a given drug (relative to all adverse events reported for the drug in a database) 
                                          AND the frequency with which the same adverse event is reported for ALL drugs in the comparison group (relative to all adverse events for drugs in a database).", br(), br(),
                                          icon("thumbs-o-up"), " Mathematically, PPR is calculated as:",br(),br(),
                                          "PRR = (m/n)/ [ (M-m)/(N-n) ]"),
                                          column(1,{}),
                                          column(10,{
                                          h5( "Where:",br(),br(),
                                          "m = number of reports with drug and event in a database",br(),
                                          "n = number of reports reports with drug in a database",br(),
                                          "M = number of reports with event in database",br(),
                                          "N = number of reports in database") }) 
                                
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
                        column(3,{}),
                        box(width= 6,title = "Distribution of companion Dx PMA submissions to date",
                            status = "primary",solidHeader = TRUE,
                            
                            plotlyOutput("companionPMA", height = "250px")
                        )
                        
                )# End of industry tab
                
            )# End of tabItems structure      
        )  # End of dashboard body
        
)  # End of dashboard page     
)  # end of UI      

        