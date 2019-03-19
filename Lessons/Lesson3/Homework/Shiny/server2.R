library(shiny)
library(ggplot2)
library(dplyr)

dt_KPI1 <- read.csv("C:\\Users\\Jakub\\Documents\\GeneralInsurance_Class\\Data\\lesson2_KPI.csv")
dt_KPI <- dt_KPI1 %>% filter_all(all_vars(!is.na(.)))
server <- function(input, output){
  
  output$ggplot <- renderPlot({
    ggplot(data = dt_KPI,
           mapping = aes_string(x = "Premium", y = "Expenses", colour = input$vyber)) +
      geom_point() +
      geom_smooth()
  })
  
}
