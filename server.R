

library(tidyverse)
library(leaflet)
library(shiny)
library(shinydashboard)
library(waiter)
library(DT)
library(shinycssloaders)
library(writexl)

options(scipen = 999)


data <- read.csv("crimes.csv",header = T, sep = ",") %>% 
  filter(!PdDistrict == "")



data$Location <- NULL
data$PdId <- NULL

data$Date <- str_sub(data$Date,start = 1,end = 10)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
data$hour <- str_sub(data$Time, start = 1, end = 2)
data$hour <- as.numeric(data$hour)


data$hour_factor <- factor(data$hour,levels = c("0","1","2","3","4","5","6","7","8","9","10",
                               "11","12","13","14","15","16","17","18","19","20",
                               "21","22","23"),
                    labels = c("00:00","01:00","02:00","03:00","04:00","05:00",
                               "06:00","07:00","08:00","09:00","10:00","11:00",
                               "12:00","13:00","14:00","15:00","16:00","17:00",
                               "18:00","19:00","20:00","21:00","22:00","23:00"))


function(input, output, session){
  
hide_waiter()

data_filtered <- reactive({
  
  data %>% 
    filter(Date >= input$date_range[1], Date <= input$date_range[2]) %>% 
    filter(hour >= input$time_range[1], hour <= input$time_range[2]) %>% 
    filter(DayOfWeek %in% input$day) %>% 
    filter(Category %in% input$crime_category) %>% 
    filter(PdDistrict %in% input$crime_district)
  
  
})  
  
  
output$plt <- renderPlot({
  
  
  data_filtered() %>% 
    group_by(hour_factor) %>% 
    summarise(n_cases = n()) %>% 
    ggplot(aes(x = hour_factor, y = n_cases)) + 
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
    labs(y = "Number of cases", x = "") + 
    theme_minimal()  + 
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5,margin = margin(r = 20)),
          axis.text.x = element_text(size = 10)) +
    geom_text(aes(x = hour_factor, y = n_cases,label = format(n_cases,big.mark = " "),angle = 90,hjust = 1.5), color = "white",size = 5)
  
})


output$map <- renderLeaflet({
  
  leaflet(data = data_filtered()) %>%
    addTiles() %>%
    addCircleMarkers(~X, ~Y,clusterOptions = markerClusterOptions(),
                     label = ~as.character(Descript))
  
  
  
})


output$tbl <- DT::renderDataTable({
  
  data_filtered() %>% 
    select(-c(X,Y,hour_factor))
  
  
})




TOTAL <- reactive({
  
  data_filtered() %>% 
    nrow()
})



output$total_crimes <- renderValueBox({
  
  valueBox(value = TOTAL(),
           subtitle = "Total number of crimes", icon = icon("skull"), color = "aqua")
  
})



TOTAL_RATE <- reactive({
  
  
  a <- data_filtered() %>% 
    nrow()
  
  b <- data_filtered() %>% 
    filter(!duplicated(IncidntNum)) %>% 
    nrow()
  
  wynik <- a/b
  
  round(wynik,3)
  
})

output$total_crimes_rate <- renderValueBox({
  
  valueBox(value = TOTAL_RATE(),
           subtitle = "Number of violations per each crime",icon = icon("percentage"), color = "maroon")
})



output$table_descript <- renderTable({
  
    data_filtered() %>% 
    group_by(Descript) %>% 
    summarise(N = n()) %>% 
    arrange(desc(N)) %>% 
    head(5) 
  

})


output$downloadData_CSV <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.table(x = data_filtered() %>% select(-hour,-hour_factor),
              file = file, quote = FALSE, row.names = FALSE,sep = ";")
  }
)




output$downloadData_XLSX <- downloadHandler(
  
  filename = function() {
    paste("data-", Sys.Date(), ".xlsx", sep="")
  },
  content = function(file) {
    write_xlsx(x = data_filtered() %>% select(-hour,-hour_factor),
               path = file)
  }
)



}







