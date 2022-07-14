# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(ggplot2)
library(rvest)
library(glue)
source(file = "00_scripts/stock_analysis_functions.R")

# UI ----
linebreaks <- function(n){HTML(strrep(br(), n))}  
ui <- fluidPage(title = "Stock Analyzer",
                # 1.0 HEADER ----
                div(
                  h1("Stock Analzer"),
                ),
                div(
                  column(
                    width = 4,
                    wellPanel(
                      
                      # Add content here 
                      pickerInput(inputId = "stock_selection", 
                                  label = "Stock Index",
                                  choices = get_indicies(),
                                  multiple = F),
                      div(actionButton(inputId = "analyze",label="Analyze",icon=icon("download"))),
                      linebreaks(1),
                      div(uiOutput("indices")),
                      hr(),
                      sliderInput("short", label = h3("Short Moving Average"), min = 5, 
                                  max = 40, value = 20),
                      sliderInput("long", label = h3("Long Moving Average"), min = 50, 
                                  max = 120, value = 50)
                      
                    )
                  ), 
                  column(
                    width = 8,
                    div(h4(textOutput("plot_header"))),
                    # verbatimTextOutput('stock_data'),
                    div(plotlyOutput("plotly_plot",width = "100%",
                                     height = "500px"))
                  )
                ),
                div(
                  column(
                    width = 12,
                    div(h3("Analyst Commentary")),
                    div(textOutput("comment"))
                  )
                ))

# RUN APP ----

# SERVER ----


server <- function(input, output, session) {
  
  ################################
  # indices 
  ###############################
  output$indices <- renderUI({
    choices = stock_data_tbl() %>% purrr::pluck("label")
    pickerInput(
      inputId = "index", 
      label = "Stocks",
      choices = get_stock_list(input$stock_selection)$label,
      multiple = F
    )
  })
  
  stock_symbol2 <- reactive({as.list(strsplit(input$index, ',')[[1]])[[1]]})
  
  ############################
  # stocks 
  ############################
  stock_symbol <- eventReactive(ignoreNULL = FALSE, input$analyze, {
    input$stock_selection
  })
  
  ###########################
  # stock data
  ###########################
  
  stock_data_tbl <- reactive({
    
    stock_symbol() %>% 
      get_stock_data(from = today() - days(180), 
                     to   = today(),
                     mavg_short = input$short,
                     mavg_long  = input$long)
    
  })
  ###########################
  # plot
  ###########################
  
  plot <- renderPlotly({
    plot_stock_data(stock_data_tbl())
  })
  
  
  output$selected_symbol <- renderText({stock_symbol()})
  output$plot_header <- renderText(stock_symbol())
  output$stock_data <- renderPrint({stock_data_tbl()})
  output$plotly_plot <- plot
  output$comment <-
    renderText(
      generate_commentary(
        data=get_stock_data(get_symbol_from_user_input(input$index)),
        user_input = input$index
      ))
  
  
}





# RUN APP ----

shinyApp(ui = ui, server = server)


