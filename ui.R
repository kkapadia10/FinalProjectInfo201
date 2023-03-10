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
library(shinyjs)
library(plotly)
library(readr)

car_data <- read_delim("car_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(to bottom, #ffffff 0%, #7fdfff 50%, #00bfff 100%);
      }
    "))),
  titlePanel("Used and New Car Data"),
  tabsetPanel(
    tabPanel("Welcome",
             h2("Welcome to the Used and New Car Dataset"),
             imageOutput("welcome_image"),
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
                 textOutput("scatter_goal"),
                 selectInput("model",
                             "Select A Car Model:",
                             choices = unique(car_data$Model)),
                 radioButtons("scatter_color", "Pick a color",
                              choices = c("red", "orange", "green",
                                               "brown","purple")),
                 textOutput("scatter_instructions"),
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
                 textOutput("bar_goal"),
                 sliderInput("mileage", "Mileage on car", 
                             min = 0, 
                             max = 150000, 
                             value = 20000),
                 sliderInput("year_bar", "Select Your Desired Year.",
                             min = 1967,
                             max = 2023,
                             value = 2000,
                             sep = ""),
                 radioButtons("bar_color", "Choose color",
                              choices = c("skyblue", "lawngreen", "red", "purple", "gold")),
                 textOutput("bar_instructions"),
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
                 textOutput("table_goal"),
                 textOutput("table_summary"),
                 sliderInput("year_table", "Select your earliest year preference.",
                             min = 1949,
                             max = 2023,
                             value = 2000,
                             sep = ""),
                 textOutput("table_instructions"),
               ),
               mainPanel(
                 dataTableOutput("table")
               )
             )
    ),
    tabPanel("Conclusion",
             h2("Thank you for looking through our web app!"),
             h3("Before you go, take a look at some trends we discovered through in dataset!"),
             h4("Analysis"),
             fluidRow(
               column(width = 4, plotOutput("acura1_scatter")),
               column(width = 4, plotOutput("bmw1_scatter")),
               column(width = 4, plotOutput("acura2_scatter"))
             ),
             p("Taking a look at the scatter plot tab, you can see the trend that as Mileage increases, the Price of the car
               decreases. The above graphs (2022 Acura TLX A-Spec, 2023 BMW 330 i, and 2023 Acura MDX Type S) are good examples of this trend. The datapoints
               above the blue trendline can be seen as overpriced; while, the datapoints below the trendline can be see as underpriced or good value purchases."),
             p("Taking a look at the bar plot, we discovered a few trends. First, as the maximum mileage allowed increases, the price of the car decreases (the 
             more miles on the car, the less it is worth). Next, the newer the car, the more expensive it is. In theory, if you wanted to find a more expensive
               car, you would look at a newer car with low mileage (and the opposite for a cheaper car)."),
             p("Finally, looking at the table, we can see all the cars of any given model. While this table does not provide us with a trend, it 
               is a great resource for buyers to use if they know which model car they want."),
             h4("Data Quality"),
             p("In terms of data quality and reliability, the dataset does not include a few values from the Mileage, Price, and MSRP categories.
              However, even with this the data is still very reliable because for every category a majority of the data is provided and accurate.
               This dataset definitely provides unbaised results because there is no way to skew or lie about the Mileage, Price, Status, Year, etc. 
               of a car. Additionally, there are no ethical issues with using this data as this is public information and no population groups were harmed to 
              collect this data."),
             h4("Future Ideas"),
             p("To advance this web app in the future, there are a few potential fixes or implementations. To start, we can add the dealer information for each car
               so that the user can easily purchase the car if they desire. Additionally, another improvement would be to inclue reviews of the car from the previous
               owners. If the car is used, we should try to get the previous owner to say what they liked/disliked about the car. Finally, we should try to expand
               this web app to also look at motorcycles. Doing so would expand our target market to include those searching for a motorcycle.")
    ),
  )
)

server <- function(input, output){
  
  output$welcome_image <- renderImage({
    
    list(src = "welcome-image.jpeg",
         width = 700,
         height = 400)
    
  }, deleteFile = F)
  
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
  
  output$scatter_goal <- renderText({
    paste("The goal of this scatter plot is to examine the relationship beween price and mileage for various car models.")
  })
  
  output$scatter_instructions <- renderText({
    paste("Instructions: On the left side of the page, there are two tools you can use to 
    adjust what the plot shows. The first tool (top to bottom) is to adjust the model of the 
    car you’re interested in (the list is alphabetical so the first brand is Acura and so on). 
    The second tool is used to adjust the color of the points (cars) on the graph and has no 
    impact on the data that is shown. On the graph, there is a trend line (the blue line going through the middle). 
    This shows a rough average of the price of the car given the model and mileage. A useful way to interpret the data 
    is that any car above the trend line is overpriced compared to the average, and similarly, any data under the trend 
    line would be considered a good deal and have a lower price than average.")
  })
  
  output$bar_plot <- renderPlot({
    mileage_data <- car_data %>%
      filter(Mileage <= input$mileage) %>%
      mutate(Price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>%
      na.omit() %>% 
      ggplot(aes(Year, Price/3450)) +
      geom_col(col=input$bar_color) + 
      labs(x = "Manufacture Year", y = "Average Price of Car (in $)", 
          title = "Average Car Price vs Manufacture Year for various mileages")
    mileage_data
  })
  
  output$bar_plot_summary <- renderText({
    mileage_data <- car_data %>%
      filter(Mileage <= input$mileage) %>%
      na.omit()
    n_total <- nrow(mileage_data)
    n_missing <- sum(is.na(mileage_data$Year) | is.na(mileage_data$Status))
    n_non_missing <- n_total - n_missing
    price_data <- mileage_data %>%
      filter(Year == input$year_bar) %>% 
      mutate(num_price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>% 
      filter(!is.na(num_price))
    avg_price <- mean(price_data$num_price)
    paste0(format(n_non_missing, big.mark = ","), " cars have at most ", 
          format(input$mileage, big.mark = ","), " miles. For ", input$year_bar,", the average price of cars under ", 
          format(input$mileage, big.mark = ","), " miles is $", format(round(avg_price), nsmall = 0, big.mark = ","), ". 
          As the maximum mileage allowed increases, the price of the car decreases (the more miles on the car, the less it is worth).
           Next, the newer the car, the more expensive it is.")
  })
  
  output$bar_goal <- renderText({
    paste("The goal of this bar plot is to examine the relationship between price and manufacture year.")
  })
  
  output$bar_instructions <- renderText({
    paste("Instructions: On the left side of the page, there are three tools you can use to adjust what the plot shows. 
          The first tool (top to bottom) is to adjust the maximum mileage a car can have. In order to change this value 
          slide the dot right and left (the minimum is 0 miles and the maximum is 150,000 miles). The second tool 
          (the middle) can be used to determine the year of the car you are interested in. Similar to the mileage tool, 
          slide the dot right and left to change the year of interest (the oldest models are 1943 and the newest is 2023). Changing the second tool,
          changes the year we are looking at to find the average car value.
          The last tool simply changes the color of the bars on the plot and has no effect on the data about the cars that are being presented.")
  })
  
  output$table <- renderDataTable({
    car_data %>% 
      filter(Year >= input$year_table) %>% 
      arrange(Year)
  })
  
  output$table_summary <- renderText({
    year_data <- car_data %>% 
      filter(Year >= input$year_table)
    n_format <- format(nrow(year_data), big.mark = ",")
    paste0("There are ", n_format, " cars that were manufactured later than ", input$year, ".")
  })
  
  output$table_goal <- renderText({
    paste("The goal of this table is to look at all the cars of a specific model.")
  })
  
  output$table_instructions <- renderText({
    paste("Instructions: Slide the slider to the desired minimum year to be displayed on the table.
    The table will then display the following information about the cars: Year, Status, Mileage, Price, and MSRP.
    You can also elect how many entries you want in the table. You can select either 10, 25, 50, and 100 entries to be displayed.
          The Model will display the model of the vehicle, 
          Year displays the year of the vehicle, 
          Status will display whether the vehicle is used or new,
          Mileage displays the mileage of the vehicle,
          Price displays the price of the vehicle,
          and MSRP specifies the drop or increase in the price of the vehicle’s price according to the manufacturer.")
  })
  
  output$acura1_scatter <- renderPlot({
    acura1_data <- car_data %>%
      filter(!grepl('Not available', Mileage)) %>%
      filter(Model == "2022 Acura TLX A-Spec") %>% 
      mutate(Mileage = as.numeric(str_replace_all(Mileage, "[^[:digit:]]", "")),
             Price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>%
      na.omit()
    p <- ggplot(acura1_data, aes(x = Mileage, y = Price)) +
      geom_point() +
      labs(x = "Mileage of Car (in miles)", y = "Price of Car (in $)", 
           title = "Car Price vs Mileage for the 2022 Acura TLX A-Spec")
      # Update the global slope variable
      slope <<- round(coef(summary(lm(Price ~ Mileage, data = acura1_data)))[2, 1], 2)
      p <- p + geom_smooth(method = "lm", se = FALSE) +  # add linear regression line
        geom_text(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                  label = paste0("Slope: ", slope))  # add slope value
    p
  })
  output$bmw1_scatter <- renderPlot({
    bmw1_data <- car_data %>%
      filter(!grepl('Not available', Mileage)) %>%
      filter(Model == "2023 BMW 330 i") %>% 
      mutate(Mileage = as.numeric(str_replace_all(Mileage, "[^[:digit:]]", "")),
             Price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>%
      na.omit()
    p <- ggplot(bmw1_data, aes(x = Mileage, y = Price)) +
      geom_point() +
      labs(x = "Mileage of Car (in miles)", y = "Price of Car (in $)", 
           title = "Car Price vs Mileage for the 2023 BMW 330 i")
    # Update the global slope variable
    slope <<- round(coef(summary(lm(Price ~ Mileage, data = bmw1_data)))[2, 1], 2)
    p <- p + geom_smooth(method = "lm", se = FALSE) +  # add linear regression line
      geom_text(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                label = paste0("Slope: ", slope))  # add slope value
    p
  })
  
  output$acura2_scatter <- renderPlot({
    acura2_data <- car_data %>%
      filter(!grepl('Not available', Mileage)) %>%
      filter(Model == "2023 Acura MDX Type S") %>% 
      mutate(Mileage = as.numeric(str_replace_all(Mileage, "[^[:digit:]]", "")),
             Price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>%
      na.omit()
    p <- ggplot(acura2_data, aes(x = Mileage, y = Price)) +
      geom_point() +
      labs(x = "Mileage of Car (in miles)", y = "Price of Car (in $)", 
           title = "Car Price vs Mileage for the 2023 Acura MDX Type S")
    # Update the global slope variable
    slope <<- round(coef(summary(lm(Price ~ Mileage, data = acura2_data)))[2, 1], 2)
    p <- p + geom_smooth(method = "lm", se = FALSE) +  # add linear regression line
      geom_text(x = Inf, y = Inf, hjust = 1, vjust = 1, 
                label = paste0("Slope: ", slope))  # add slope value
    p
  })
}
shinyApp(ui = ui, server = server, options = list(height = 1080))