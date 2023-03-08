#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(plotly)
library(readr)

car_data <- read_delim("car_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Used and New Car Data"),
  tabsetPanel(
    tabPanel("Welcome",
             h2("Welcome to the Used and New Car Dataset"),
             h4("In this dataset, there are over 100,000 cars for you to look at to figure out
                which car fits your needs perfectly!"),
             p("This web app shows information regarding", em("various car data from", min(car_data$Year), "to", max(car_data$Year))),
             p("There are", strong(format(nrow(car_data), big.mark = ","), "cars and", ncol(car_data), "variables") , "we will be looking at in this dataset."),
             p("This data consists of several variables necessary for buyers to look at before buying cars, such as the",
               em("Model, Year, Status, Mileage, Price, and MSRP.")),
             p("This data is found on", strong("Kaggle"), "and was updated by", strong("George Baffour.")),
             h5("Below is a", em("random sample of 10 car data values:")),
             dataTableOutput("welcome_table"),
    ),
    
    tabPanel("Scatter Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput("model",
                             "Select Car Model:",
                             choices = unique(car_data$Model)),
                 radioButtons("scatter_color", "Pick a color",
                              choices = c("red", "orange", "green",
                                               "blue","purple"))
               ),
               mainPanel(
                 plotOutput("scatter_plot"),
                 textOutput("scatter_plot_summary")
               )
             )
    ),
    tabPanel("Bar Plot",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("mileage", "Mileage on car", 
                             min = 0, 
                             max = 115000, 
                             value = 20000),
                 radioButtons("bar_color", "Choose color",
                              choices = c("skyblue", "lawngreen", "red", "purple", "gold"))
               ),
               mainPanel(
                 plotOutput("bar_plot"),
                 textOutput("bar_plot_summary"),
               ),
             )
    ),
    tabPanel("Table",
             sidebarLayout(
               sidebarPanel(
                 textOutput("table_summary"),
                 sliderInput("year", "Select your earliest year preference.",
                             min = 1949,
                             max = 2023,
                             value = 2000,
                             sep = ""),
               ),
               mainPanel(
                 dataTableOutput("table")
               )
             )
    )
  )
)

server <- function(input, output){
  
  output$welcome_table <- renderDataTable({
    car_data %>% 
      sample_n(10)
  })
  
  # Initialize a global variable for the slope
  slope <- NULL
  
  # Define the scatter plot output
  output$scatter_plot <- renderPlot({
    model_data <- car_data %>%
      filter(!grepl('Not available', Mileage)) %>%
      filter(Model == input$model) %>% 
      mutate(Mileage = as.numeric(str_replace_all(Mileage, "[^[:digit:]]", "")),
             Price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>%
      na.omit()
    p <- ggplot(model_data, aes(x = as.integer(gsub(" mi.", "", Mileage)), y = as.integer(gsub("$", "", Price)))) +
      geom_point(col = input$scatter_color) +
      labs(x = "Mileage of Car (in miles)", y = "Price of Car (in $)", 
           title = "Car Price vs Mileage for various car models")
    if (nrow(model_data) > 1) {  # check number of data points
      # Update the global slope variable
      slope <<- round(coef(summary(lm(Price ~ Mileage, data = model_data)))[2, 1], 2)
      p <- p + geom_smooth(method = "lm", se = FALSE) +  # add linear regression line
        geom_text(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                  label = paste0("Slope: ", slope))  # add slope value
    }
    p
  })
  
  # Define the scatter plot summary output
  output$scatter_plot_summary <- renderText({
    model_data <- car_data %>%
      filter(!grepl('Not available', Mileage)) %>% 
      filter(Model == input$model)
    n_total <- nrow(model_data)
    if (is.na(max(model_data$Price)) || is.na(min(model_data$Price)) || is.na(max(model_data$Mileage)))
    {
      paste0("There are ", n_total, " " , input$model, "s that have available data. 
          Due to this, we are unable to provide a graph comparing car price and mileage driven.
          However, check again later to see new information regarding the ", input$model, ".")
    }
    else
    {
      output_text <- paste0("There are ", n_total, " " , input$model, "s that have available data. ",
                            "The most expensive one costs ", max(model_data$Price), " and the cheapest one costs ", min(model_data$Price), 
                            ". The ", input$model, " with the highest mileage has driven ", max(model_data$Mileage))
      if (!is.null(slope) && slope < 0)
      {
        output_text <- paste0(output_text, " Given that we have a negative slope of ", slope, 
                              ", we can see the general trend that as the mileage increases, the price decreases.")
      }
      if (!is.null(slope) && slope >= 0)
      {
        output_text <- paste0(output_text, " Given that we have a positive slope of ", slope, ", the ", input$model, " goes against the general trend that 
                              as the mileage increases, the price decreases.")
      }
      output_text
    }
  })
  
  
  
  output$bar_plot <- renderPlot({
    mileage_data <- car_data %>%
      filter(Mileage <= input$mileage) %>%
      mutate(Mileage = as.numeric(str_replace_all(Mileage, "[^[:digit:]]", "")),
             Price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>%
      na.omit() %>% 
      ggplot(aes(Year, Price)) +
      geom_col(col=input$bar_color) +
      ggtitle(paste("Car Price vs Manufactured Year for various mileages"))
    mileage_data
  })
  
  
  output$bar_plot_summary <- renderText({
    mileage_data <- car_data %>%
      filter(Mileage <= input$mileage)
    n_total <- nrow(mileage_data)
    n_missing <- sum(is.na(mileage_data$Year) | is.na(mileage_data$Status))
    n_non_missing <- n_total - n_missing
    paste(format(n_non_missing, big.mark = ","), "cars have at most", format(input$mileage, big.mark = ","), "miles.")
  })
  
  output$table <- renderDataTable({
    car_data %>% 
      filter(Year >= input$year) %>% 
      arrange(Year)
  })
  
  output$table_summary <- renderText({
    year_data <- car_data %>% 
      filter(Year >= input$year)
    n_format <- format(nrow(year_data), big.mark = ",")
    paste0("There are ", n_format, " cars that were manufactured later than ", input$year, ".")
  })
}
shinyApp(ui = ui, server = server, options = list(height = 1080))