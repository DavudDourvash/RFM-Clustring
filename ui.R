##########################################
############# ui.R #######################
##########################################

#### packages and libraries ####
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("ggplot2")
# install.packages("jsonlite")
# install.packages("devtools")
# install.packages("dygraphs")
# install.packages("plotly")
# install.packages("googleVis")
# install.packages("plyr")
# install.packages("ggthemes")
# install.packages("extrafont")
# install.packages("scales")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(jsonlite)
library(devtools)
library(dygraphs)
library(plotly)
library(plyr)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)
library(radarchart)
library(googleVis)
#source header -----------
#### header in  - ui ####
header <- dashboardHeader(title="RFM Clustring",
                                  



                          
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "5 new users today",
                                         icon("users")
                                       ),
                                       notificationItem(
                                         text = "12 items delivered",
                                         icon("truck"),
                                         status = "success"
                                       ),
                                       notificationItem(
                                         text = "Server load at 86%",
                                         icon = icon("exclamation-triangle"),
                                         status = "warning"
                                       )
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 90, color = "green",
                                                "Documentation"
                                       ),
                                       taskItem(value = 17, color = "aqua",
                                                "Project X"
                                       ),
                                       taskItem(value = 75, color = "yellow",
                                                "Server deployment"
                                       ),
                                       taskItem(value = 80, color = "red",
                                                "Overall project"
                                       )
                          ),
                          tags$li(class = "dropdown",
                                  tags$a( tags$img(height = "40px", alt="SNAP Logo", src="dayche2.png", align="rigth")
                                  ))
)
# header$children$children <-  tags$a(tags$img(src='dayche.png',height='60',width='200',align="right"))

#### sidebar ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("dashboard", tabName = "dashboard", icon = icon("database")),
    menuItem("Overview", tabName = "over", icon = icon("database")),
    menuItem("Customers", tabName = "custom", icon = icon("database")),
    sidebarMenuOutput("menu")
  )
)
#### body ####
body <- dashboardBody(

  
  
  
  tags$style(HTML("
        .skin-blue .main-header .logo {
                  background-color: orange;
                  font-family: 'Georgia', Times, 'Times New Roman', serif;
                  font-weight: bold;
                  font-size: 20px;
                  }
                  
        .skin-blue .main-header .logo:hover {
                              background-color: orange;
        }

        .skin-blue .main-header .navbar {
                              background-color: green;
        } 
        .skin-blue .main-sidebar {
                              background-color: #f4b943;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #000000;
                  color: #000000;
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
          background-color: #ff69b4;
        }

        .content-wrapper,
        .right-side {
        background-color: #ffffff;
        }
# .shiny-progress {
#   
#                   top: 50% !important;
#                   left: 50% !important;
#                   margin-top: -100px !important;
#                   margin-left: -250px !important;
#                   
#                   
#                   color: #020202;
#                   font-size: 18px;
#                   background-color: #020202;
#                   text-align: center;
#                   
#                   
#                   }
                  
                  /* other links in the sidebarmenu when hovered */
                  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                  background-color: #ff69b4;
                  }
                  /* toggle button when hovered  */                    
                  .skin-blue .main-header .navbar .sidebar-toggle:hover{
                  background-color: #ff69b4;
                  }
                  ")),
  # tags$style(HTML("
  # 
  #                 body {
  #                 background-color: green;
  #                 }
  #                 
  #                 ")),
  
  tabItems(
#### setting tab ####
tabItem(tabName = "dashboard",
  fluidRow(
    column(12,
           checkboxInput("impdata",label = h3("import data from user"), value = FALSE),
           uiOutput("impdata2")
    )
  ),
  fluidRow(
    column(4,
           wellPanel(
             title = h3("No.Cluster Min-Max"),
             sliderInput("slider1", h3("No.Cluster Min-Max:"), 0, 10, c(3, 6))
           )),
    column(4,
           numericInput("numclust", label = h3("Fix No.Column"), value = 3)

    )
  ),
  fluidRow(
    column(4,
           wellPanel(
             headerPanel(title = h3("Scoring for R")),
             sliderInput("sliderc1r", "cluster 1 :", 1, 10, c(1, 5)),
             sliderInput("sliderc2r", "cluster 2 :", 1, 10, c(1, 5)),
             sliderInput("sliderc3r", "cluster 3 :", 1, 10, c(1, 5))
           )),
    column(4,
           wellPanel(
             headerPanel(title =h3("Scoring for F")),
             sliderInput("sliderc1r", "cluster 1 :", 1, 10, c(1, 5)),
             sliderInput("sliderc2r", "cluster 2 :", 1, 10, c(1, 5)),
             sliderInput("sliderc3r", "cluster 3 :", 1, 10, c(1, 5))
           )),
    column(4,
           wellPanel(
             headerPanel(title = h3("Scoring for M")),
             sliderInput("sliderc1r", "cluster 1 :", 1, 10, c(1, 5)),
             sliderInput("sliderc2r", "cluster 2 :", 1, 10, c(1, 5)),
             sliderInput("sliderc3r", "cluster 3 :", 1, 10, c(1, 5))
           ))
  )),
#### overview panel1 ui ####
tabItem(tabName = "over",
        
      fluidRow(
        column(12,
               fluidRow(
                 tabBox(width = 12,height = "500px", 

                   tabPanel("Frequency Of Clusters",
                              column(6,
                                     
            
tags$head(tags$style("#text1, #text2, #text3, #text4{color: black;
                                 font-size: 15px;
                                               font-style: italic;
                                               }"
                         )
                          ), 
textOutput("text1"),
                           # h4("The NEW cluster Customers : who have bought recently but in small quantities"),
                          br(), br(),
                                 textOutput("text2"),  
# h4("The PASSED cluster Customers : who have not bought recently but in small quantities"),
                          br(), br(),
textOutput("text3"),
                                   # h4("The VIP Cluster Customers : who have bought recently and in large quantities"),
                          br(), br(),
textOutput("text4")
                                  # h4("The CHURN Cluster Customers : who have not bought recently but in large quantities")
                                   ),
                          column(6,
                          plotlyOutput("bartotal")
                   ))
                 )),
                 hr(),
        fluidRow(
          column(12,
        tabBox(title = "Center Points", width = 12, height = "325px",
               tabPanel("New Cluster",
                        column(6, offset = 3,
                          plotlyOutput("bartoo",height = "250px", width = "400px"))),
               tabPanel("Passed Cluster",
                        column(6, offset = 3,
                          plotlyOutput("bartot",height = "250px", width = "400px"))),
               tabPanel("VIP Cluster",
                        column(6, offset = 3,
                          plotlyOutput("bartoth",height = "250px", width = "400px"))),
                   tabPanel("CHURN Cluster",
                            column(6, offset = 3,
                          plotlyOutput("bartofr",height = "250px", width = "400px")))
                 ))),
                 hr(),
fluidRow(
  column(12,
  tabBox(title = "Distribution", width = 12, height = "400px",
#### Cluster New RFM distribution ui ####

         tabPanel("New", width=24,
                  column(4,
                         
                         plotlyOutput("barRclone", height = "300px", width = "300px")
                  ),
                  column(4,
                         plotlyOutput("barFclone", height = "300px", width = "300px")
                  ),
                  column(4,
                         plotlyOutput("barMclone", height = "300px", width = "300px")
                  )
         ),
#### Cluster Passed RFM distribution ui ####
         
         tabPanel("Passed", width=24,
                  column(4,
                         plotlyOutput("barRcltwo", height = "300px", width = "300px")
                  ),
                  column(4,
                         plotlyOutput("barFcltwo", height = "300px", width = "300px")
                  ),
                  column(4,
                         plotlyOutput("barMcltwo", height = "300px", width = "300px")
                  )
         ),
#### Cluster VIP RFM distribution ui ####
         
         tabPanel("VIP", width=24,
                  column(4,
                         plotlyOutput("barRclthree", height = "300px", width = "300px")
                  ),
                  column(4,
                         plotlyOutput("barFclthree", height = "300px", width = "300px")
                  ),
                  column(4,
                         plotlyOutput("barMclthree", height = "300px", width = "300px")
                  )
         ),
#### Cluster Churn RFM distribution ui ####
         
         tabPanel("Churn", width=24,
                  column(4,
                         plotlyOutput("barRclfour", height = "300px", width = "300px")
                  ),
                  column(4,
                         plotlyOutput("barFclfour", height = "300px", width = "300px")
                  ),
                  column(4,
                         plotlyOutput("barMclfour", height = "300px", width = "300px")
                  )
         )
  ))
)))),
#### customers panel ####
 tabItem(tabName = "custom",
         
         fluidPage(
                  fluidRow(
        tabBox(title = "The Most Important Customers", width = 12, height = "350px",
#### create well panel of sorted distance for vip and churn clusters - ui ####
               tabPanel("Distance",
                        column(6,
                               h5("Customers who have Minimum Distance to center of VIP Cluster"),
                               wellPanel(style = "background-color: #ffffff;",
                                 headerPanel(title=
                                               h3("VIP")),
                                 verbatimTextOutput("distcthreeone"),
                                 verbatimTextOutput("distcthreetwo"),
                                 verbatimTextOutput("distcthreethree")

                               )),
                        column(6,
                               h5("Customers who have Minimum Distance to center of CHURN Cluster"),
                               wellPanel(style = "background-color: #ffffff;",
                                 headerPanel(title =
                                               h3("Churn")),
                                 verbatimTextOutput("distcfourone"),
                                 verbatimTextOutput("distcfourtwo"),
                                 verbatimTextOutput("distcfourthree")
                               ))
                        ),

#### create well panel of sorted speed for vip and churn clusters - ui ####
               tabPanel("speed",

                        column(6,
                               h5("Customers who are coming closer to center of VIP Cluster with Maximum Speed"),
                               wellPanel(style = "background-color: #ffffff;",
                                 headerPanel(title =
                                               h3("VIP")),
                                 verbatimTextOutput("vcthreeone"),
                                 verbatimTextOutput("vcthreetwo"),
                                 verbatimTextOutput("vcthreethree")
                               )),
                        column(6,
                               h5("Customers who are coming closer to center of Churn Cluster with Maximum Speed"),
                               wellPanel(style = "background-color: #ffffff;",
                                 headerPanel(title =
                                               h3("Churn")),
                                 verbatimTextOutput("vcfourone"),
                                 verbatimTextOutput("vcfourtwo"),
                                 verbatimTextOutput("vcfourthree")
                               ))
               ),

#### create well panel of sorted Acceleration for vip and churn clusters - ui ####
               tabPanel("Acceleration",
                        column(6,
                               h5("Customers who are coming closer to center of VIP Cluster with Maximum Acceleration"),
                               wellPanel(style = "background-color: #ffffff;",
                                 headerPanel(title =
                                               h3("VIP")),
                                 verbatimTextOutput("acthreeone"),
                                 verbatimTextOutput("acthreetwo"),
                                 verbatimTextOutput("acthreethree")
                               )),
                        column(6,
                               h5("Customers who are coming closer to center of CHURN Cluster with Maximum Acceleration"),
                               wellPanel(style = "background-color: #ffffff;",
                                 headerPanel(title =
                                               h3("Churn")),
                                 verbatimTextOutput("acfourone"),
                                 verbatimTextOutput("acfourtwo"),
                                 verbatimTextOutput("acfourthree")
                               ))
               )
            )

        ),
hr(),
#Create select input widgets ----------
tags$head(
  tags$style(HTML('#action1{background-color:yellow;
                  font-style: italic;}'))
),
fluidRow(
  column(4,width = 12, offset = 3,
         box(title = h1("Clusters"), background = "blue", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
             radioButtons("chclust", label = "", choices = c("New", "Passed", "VIP", "Churn"), selected = "New", inline = TRUE),
             # actionButton("importnew", label = "import CID"),
             selectInput("num1","",c("CID"=""), multiple = FALSE),
             actionButton("action1", label = "Go"),
             uiOutput("cid")
         ))),
#   column(4,width = 6,
# 
#          box(title = "Passed CID", background = "green", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
#              actionButton("importpassed", label = "import CID"),
#              selectInput("num2","",c("CID"=""), multiple = FALSE),
#              actionButton("action2", label = "Go"),
#              uiOutput("cid2")
#          ))),
# fluidRow(
#   column(4, width = 6,
#          box(title = "VIP CID", background = "green", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
#              actionButton("importVip", label = "import CID"),
#              selectInput("num3","",c("CID"=""), multiple = FALSE),
#              actionButton("action3", label = "Go"),
#              uiOutput("cid3")
#          )),
#   column(4, width = 6,
#          box(title = "Churn CID", background = "green", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,
#              actionButton("importChurn", label = "import CID"),
#              selectInput("num4","",c("CID"=""), multiple = FALSE),
#              actionButton("action4", label = "Go"),
#              uiOutput("cid4")
#          ))),
hr(),
#### information tabpanel #####
fluidRow(
         tabBox(title = "Customer Information",width = 12,
           tabPanel("Show",
fluidRow(
  column(6,

         dygraphOutput("vecplotc11",  height = "200px")),
         column(6,
                dygraphOutput("vecplotc12", height = "200px")

                )
         ),
hr(),
fluidRow(
  column(6,

         chartJSRadarOutput("radar1")),
         column(6,
                column(4,
                       htmlOutput("r")
                       ),
                column(4,
                       htmlOutput("f")
                ),
                column(4,
                       htmlOutput("m")
                )
                
                # plotlyOutput("RFMVecplot1", height = "250px")
         )
         ))
))
))))


# ))
# 
#  })
#  observeEvent(input$action2,{
#               dygraphOutput("vecplotc21",  height = "600px")
#  })
# 
#  observeEvent(input$action1, {
# 
#  dygraphOutput("vecplotc12",  height = "200px", width = "600px"))
#  })
#  observeEvent(input$action2,{
#               dygraphOutput("vecplotc22",  height = "200px")
#  })
# 
#    )),
#    fluidRow(),
#  flowLayout(
#           chartJSRadarOutput("radar1"),
#           plotlyOutput("RFMVecplot1", height = "250px")
#  )
#   )
#  ))
#  ))

#### shiny UI ####
shinyUI(
  
  dashboardPage(
      

    header, sidebar, body
  ))
  

