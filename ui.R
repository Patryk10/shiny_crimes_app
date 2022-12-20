library(shiny)
library(shinydashboard)
library(waiter)
library(DT)
library(shinycssloaders)
library(leaflet)



  dashboardPage(
  
  dashboardHeader(title = "San Francisco crimes"),
  
  dashboardSidebar(sidebarMenu(
    
    br(),
    br(),

    
    div(style="display:inline-block;width:100%;text-align: center;",submitButton(text = "Apply", width = "100px",icon("paper-plane"))),
    
    br(),
    br(),
    
    
    
    dateRangeInput(inputId = "date_range", label = "Select date range", language = "en", separator = "to",
                   start = "2016-01-01", end = "2016-12-31", min = "2016-01-01", max = "2016-12-31", weekstart = 1),
    
    sliderInput(inputId = "time_range", label = "Select time range", min = 0, max = 24, step = 1, value = c(0,24)),
    
    selectInput(inputId = "day", label = "Select day", choices = c("Friday","Monday","Tuesday","Saturday","Thursday","Sunday","Wednesday"),
                multiple = TRUE,
                selected = c("Sunday","Wednesday")),
    
    selectInput(inputId = "crime_category", label = "Select crime category", choices = c("WEAPON LAWS", "WARRANTS","NON-CRIMINAL","ASSAULT","OTHER OFFENSES","MISSING PERSON","LARCENY/THEFT","BURGLARY",                   
                                                                                         "STOLEN PROPERTY",             "ROBBERY",                     "FRAUD",                       "DRUG/NARCOTIC",              
                                                                                         "VEHICLE THEFT",               "RECOVERED VEHICLE",           "VANDALISM",                   "ARSON",                      
                                                                                         "PROSTITUTION",                "SECONDARY CODES",             "SUSPICIOUS OCC",              "DRUNKENNESS",                
                                                                                         "TRESPASS",                    "SEX OFFENSES, NON FORCIBLE",  "SEX OFFENSES, FORCIBLE",      "RUNAWAY",                    
                                                                                         "KIDNAPPING",                  "DISORDERLY CONDUCT",          "DRIVING UNDER THE INFLUENCE", "FORGERY/COUNTERFEITING",     
                                                                                         "EMBEZZLEMENT",                "BRIBERY",                     "FAMILY OFFENSES",             "GAMBLING",                   
                                                                                         "SUICIDE",                     "LIQUOR LAWS",                 "EXTORTION",                   "LOITERING",                  
                                                                                         "TREA",                        "BAD CHECKS",                  "PORNOGRAPHY/OBSCENE MAT"  ), 
                selected = "WEAPON LAWS", multiple = TRUE),
    menuItem(text = "Districts", icon = icon("chart-line"), badgeColor = "aqua",
    
    checkboxGroupInput(inputId = "crime_district", label = "Select district",
                       choices = c("SOUTHERN","BAYVIEW","TENDERLOIN", "MISSION","NORTHERN","TARAVAL","INGLESIDE","CENTRAL","RICHMOND","PARK"),
                       selected = c("SOUTHERN","BAYVIEW","TENDERLOIN", "MISSION","NORTHERN","TARAVAL","INGLESIDE","CENTRAL","RICHMOND","PARK")
                       )),
    
    br(),
    
    div(style="display:inline-block;width:100%;text-align: center;",downloadButton("downloadData_CSV", "Download CSV")),
    
    br(),
    br(),
    
    div(style="display:inline-block;width:100%;text-align: center;",downloadButton("downloadData_XLSX", "Download XLSX"))
  )),
  
  
  dashboardBody(
    
    use_waiter(),
    show_waiter_on_load(spin_3circles()),
    
    
    tabsetPanel(
      tabPanel(title = "Dashboard",
               br(),
    
    fluidRow(
    column(width = 12,
    box(withSpinner(plotOutput("plt")), width = 12, title = "Crimes divided into hours", status = "primary"))),
    
    fluidRow(
      column(width = 8,
             withSpinner(leafletOutput("map",height = 500))),
      column(width = 4,
             valueBoxOutput("total_crimes",width = 12),
             valueBoxOutput("total_crimes_rate",width = 12),
             box(tableOutput("table_descript"), status = "primary", title = "The most popular violations",width = 12))
    )),
    
    tabPanel(title = "Additional table",
             DT::dataTableOutput("tbl"))
    
    )
))