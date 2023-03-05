library(shiny)
library(tidyverse)
library(ggplot2)

# load the data
data <- read_delim("data/lower.csv")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("About",
             h3("UAH lower troposphere"),
             p("This app uses satellite temperature data from", strong("UAH")),
             p("Variable temperature", em("temp"),
               "is measured as deviation (deg C) from 1991-2020 average"),
             p("The dataset contains", nrow(data), "rows and", 
               ncol(data), "columns."),
             p("The highest avg temperature occur in the data is", 
               max(data$temp, na.rm = TRUE), "."),
             p("The lowest avg temperature occur in the data is", 
               min(data$temp, na.rm = TRUE), "."),
             p("Here is a random sample of data:"),
             tableOutput("data_sample")
             ),
    
    tabPanel("Plots",
             h3("Plot of data"),
             sidebarLayout(
               sidebarPanel(
                 p("On this page, you can use the sidebar 
                   to modify the plot in the way you like.\n"),
                 p("This widget can set the", 
                   strong("year range"), "of the plot."),
                 sliderInput("year_range",
                             "Select the year range",
                             min = min(data$year, na.rm = TRUE),
                             max = max(data$year, na.rm = TRUE),
                             value = c(min(data$year, na.rm = TRUE),
                                       max(data$year, na.rm = TRUE))),
                 p("Choose if you would like to see the", 
                   strong("trend"), "of the region you selected."),
                 checkboxInput("line",
                               "Disply trend"),
                 p("In this widget, you can choose the", 
                   strong("region(s)"), "that you are interested in."),
                 uiOutput("region")
               ),
               mainPanel(
                 plotOutput("plot"),
                 textOutput("message")
               )
             )
            ),
    
    tabPanel("Tables",
             h3("Table of data"),
             sidebarLayout(
               sidebarPanel(
                 p("On this page, you can use the sidebar 
                   to modify the table in the way you like.\n"),
                 p("This widget can choose the average over the", 
                   strong("time period"), "of your choice."),
                 radioButtons("time",
                              "Average over:",
                              choices = c("Month", "Year"),
                              selected = "Month"),
                 selectInput("years",
                             "Choose the specific years:",
                             choices = unique(data$year),
                             multiple = TRUE,
                             selected = "1978"
                             )
               ),
               mainPanel(
                 textOutput("text"),
                 tableOutput("table")
               )
             ))
    )
)


server <- function(input, output) {
  # Opening page 
  output$data_sample <- renderTable({
    sample_n(data, 10)
  })
  
  # Plot page
  filter_data <- reactive({
    data %>% 
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
  })
  output$region <- renderUI({
    checkboxGroupInput("choice", 
                       "Choose region(s):", 
                       choices = unique(filter_data()$region),
                       selected = unique(filter_data()$region[1]))
  })
  output$plot <- renderPlot({
    plots <- ggplot(filter_data() %>% 
                      filter(region %in% input$choice),
                    aes(x = year, y = temp,
                        group = region, col = factor(region))) +
      geom_point(position = "jitter") +
      labs(x = "Year", y = "Temperature in Celsius",
           main = "Temperature over Time by Region(s)")  
    if (input$line) {
      plots <- plots + 
        geom_smooth(method = "lm", se = FALSE)
    }
    plots
  })
  output$message <- renderText({
    paste("The year range is between", input$year_range[1], 
        "and", input$year_range[2], ", and",
        nrow(filter_data()), 
        " rows of data has been selected and plotted.")
  })
  
  #Table page
  filtered_data <- reactive({
    data %>% 
      filter(year %in% input$years) %>% 
      select(year, month, temp)
  })
  output$table <- renderTable({
    if(input$time == "Month"){
      filtered_data() %>% 
        group_by(year, month) %>% 
        summarize(mean = mean(temp), .groups = "drop")
    } else if(input$time == "Year"){
      filtered_data() %>% 
        group_by(year) %>% 
        summarize(mean = mean(temp), .groups = "drop")
    }
  })
  output$text <- renderText({
    paste("Choose to show the average temperature in", input$time, 
          ", and", nrow(filtered_data()), "rows of data were used.")
  })
}


shinyApp(ui = ui, server = server)