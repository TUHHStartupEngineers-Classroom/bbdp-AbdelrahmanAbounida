library(rsconnect)

rsconnect::setAccountInfo(
  name='abdelrahmanabounida',
  token='AFF16870C5D3CF4B3B338FB4F70E9479',
  secret='x0/iMrG3mU4eDuXPKVsCvo71C0e0jrco+O3DsQhn')



library(tidyverse)
library(lubridate)
library(plotly)
library(dplyr)
library(raster)
library(sf)
library(shiny)
library(shinydashboard)

# 1- Data Preparation 

bikes_tbl      <- readRDS("../00_bike_data/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("../00_bike_data/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("../00_bike_data/orderlines_tbl.rds")

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  mutate(total_price = price_euro * quantity)




orderlines_tbl$order_date

ui <- fluidPage(
  
  checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                     choices = list("Mountain" = 'Mountain', "Road" = 'Road', "Hybrid/City" = 'Hybrid/City'),
                     selected = 1),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  
)

server <- function(input, output) {
  
  output$value <- renderPrint({ input$checkGroup })
  
}

shinyApp(ui = ui, server = server)



