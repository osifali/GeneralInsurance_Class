library(ggplot2)
library(shiny)
library(dplyr)
dt_KPI1 <- read.csv("C:\\Users\\Jakub\\Documents\\GeneralInsurance_Class\\Data\\lesson2_KPI.csv")
dt_KPI <- dt_KPI1 %>% filter_all(all_vars(!is.na(.)))
ui <- fluidPage(    

  titlePanel("Závislosť Premium a Expenses"),
  
  sidebarLayout(      
    
    sidebarPanel(
      selectInput("vyber", "Výber:", 
                  choices=colnames(dt_KPI[,1:5]))
    ),
    
    mainPanel(
      plotOutput("ggplot")  
    )
    
  )
)
