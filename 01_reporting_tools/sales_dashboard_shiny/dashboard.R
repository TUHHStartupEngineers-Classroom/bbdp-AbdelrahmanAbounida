
library(tidyverse)
library(lubridate)
library(plotly)
library(dplyr)
library(raster)
library(sf)
library(shiny)

# 1- Data Preparation 

bikes_tbl      <- readRDS("../00_bike_data/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("../00_bike_data/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("../00_bike_data/orderlines_tbl.rds")

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  mutate(total_price = price_euro * quantity)


x <- left_join(orderlines_tbl,bikes_tbl, by = c("product_id" = "bike_id"),header=TRUE)

bike_orderlines_wrangled_tbl <- x %>% left_join(bikeshops_tbl, by= c("customer_id" = "bikeshop_id"))

cat <- bike_orderlines_wrangled_tbl  %>% filter(str_detect(category_1, "Mountain")) # filter category here #### note


bike_orderlines_wrangled_tbl <- cat %>% mutate('Total.Price' = price_euro * quantity)

###############################
# sales by Location, Category
##############################

bike_orderlines_wrangled_tbl1 <- bike_orderlines_wrangled_tbl %>% dplyr::select(city,state, Total.Price)


tot_sales_by_state <- bike_orderlines_wrangled_tbl1 %>% 
  group_by(state) %>% 
  summarise(Total.Price = sum(Total.Price)) %>%
  arrange(desc(Total.Price)) 

tot_sales_by_state <- tot_sales_by_state %>% mutate(sales_text = scales::dollar(Total.Price, big.mark = ".", 
                                                                                decimal.mark = ",", 
                                                                                prefix = "", 
                                                                                suffix = " €"))


tot_sales_by_state %>%
  ggplot(aes(x = state, y = Total.Price)) +
  geom_col(fill = "#2DC6D6")  + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(
    title = "Revenue by State ",
  )

###############################
# sales by Location, Date
##############################


bike_orderlines_wrangled_tbl2 <- bike_orderlines_wrangled_tbl %>% 
  filter(order_date >= "2018-12-02" & order_date <= "2019-12-05")  %>%  #### filter date here 
            dplyr::select(order_date,state,city, Total.Price)  %>% 
            mutate(year = year(order_date)) %>% 
            dplyr::select(year,state,Total.Price)


tot_sales_by_state_year <- bike_orderlines_wrangled_tbl2 %>% group_by(year,state) %>%
  summarise(Total.Price = sum(Total.Price))


# Reactive Filter

'''
tot_sales_by_state_year <- reactive({
    req(input$dates),
    bike_orderlines_wrangled_tbl %>% 
    filter(class %in% input$checkGroup) %>%
    filter(date %>% between(left  = ymd(input$dates[1]), 
                            right = ymd(input$dates[2]))) %>%
    dplyr::select(order_date,state,city, Total.Price)  %>% 
    mutate(year = year(order_date)) %>% 
    dplyr::select(year,state,Total.Price) %>%
    group_by(year,state) %>%
   summarise(Total.Price = sum(Total.Price))
    
})


renderplot({

    tot_sales_by_state_year() %>%
      ggplot(aes(x = year, y = Total.Price, fill = state)) +
      geom_col() + 
      facet_wrap(~ state) +
      scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                        decimal.mark = ",", 
                                                        prefix = "", 
                                                        suffix = " €")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(
        title = "Revenue by year and state",
      )
}
)
'''



tot_sales_by_state_year %>%
  ggplot(aes(x = year, y = Total.Price, fill = state)) +
  geom_col() + 
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(
    title = "Revenue by year and state",
  )

###############################
# Over Time
##############################

plot_total_sales <- function(unit = "month", date_format = "%B %Y", interactive = TRUE) {
  
  # Handle Data
  data_tbl <- bike_orderlines_tbl %>%
    
    dplyr::select(order_date, total_price) %>%
    
    mutate(date_rounded = floor_date(order_date, unit = unit)) %>%
    
    group_by(date_rounded) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format(date_format)}"))
  
  # Make Plot
  g1 <- data_tbl %>%
    ggplot(aes(x = date_rounded, y = total_sales)) +
    
    # Geoms
    geom_point(aes(text = label_text), color = "#2C3E50") +
    geom_smooth(method = "loess", span = 0.2) +
    
    # Formatting
    scale_y_continuous(labels = euro_format()) +
    expand_limits(y = 0) +
    labs(
      title = "Total Sales",
      y = "Revenue (Euro)",
      x = ""
    )
  
  # Static vs Interactive Logic
  if (interactive) {
    return(ggplotly(g1, tooltip = "text"))
  } else {
    return(g1)
  }
  
}


format_to_euro <- function(x, suffix = " €") {
  
  scales::dollar(x,
                 suffix       = suffix,
                 prefix       = "",
                 big.mark     = ".",
                 decimal.mark = ",")
}

euro_format <- function(scale        = 1,
                        prefix       = "",
                        suffix       = " €",
                        big.mark     = ".",
                        decimal.mark = ",") {
  
  scales::dollar_format(suffix       = suffix,
                        prefix       = prefix,
                        big.mark     = big.mark,
                        decimal.mark = decimal.mark,
                        scale        = scale)
  
}

#### apply filter here 

# 2- Quarterly Total Sales
plot_total_sales(unit = period(months = 3), date_format = "%B %d, %Y", interactive = TRUE)

# 3- Monthly Total Sales
plot_total_sales(unit = "monthly", date_format = "%B %Y", interactive = TRUE)

# 4-Weekly Total Sales
plot_total_sales(unit = "weekly", date_format = "%B %d, %Y", interactive = TRUE)





















