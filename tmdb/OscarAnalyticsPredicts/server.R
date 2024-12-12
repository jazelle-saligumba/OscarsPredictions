#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(httr)
library(ggplot2)

# Define server logic required to draw a histogram
function(input, output, session) {
  # Dynamically render the year slider if "One Year" is selected
  output$dynamic_slider <- renderUI({
    if (input$timeframe == "One Year") {
      sliderInput(inputId = "selected_year", label = "Select a Year:", 
                  min = 1975, max = 2024, value = 2000)
    }
  })
  
  # Create a reactive expression to ensure slider input is detected
  selected_year <- reactive({
    if (input$timeframe == "One Year") {
      req(input$selected_year) # Ensure slider input is valid
      input$selected_year
    }
  })
  
  # Render the plot reactively based on user inputs
  output$dynamic_plot <- renderPlot({
    # Ensure inputs are valid
    req(input$variable, input$timeframe)
    
    # Use conditional logic for plotting
    if (input$timeframe == "All Years") {
      
      if(input$variable == "genre"){
        graph_genre_all_years()
        
      } else if (input$variable == "release_day"){
        graph_release_all_years()
        
      } else {
        ggplot(data, aes(x = budget)) +
          geom_histogram(bins = 30, fill = "orange") +
          labs(title = "Budget Distribution", x = "Budget", y = "Frequency")
      }
    } else if (input$timeframe == "One Year") {
      req(selected_year()) # Ensure year is selected
      
      if(input$variable == "genre"){
        print(selected_year())
        graph_genre_one_year(selected_year())
        
      } else if (input$variable == "release_day"){
        graph_release_one_year_scatter(selected_year())
        
      } else {
        ggplot(data, aes(x = budget)) +
          geom_histogram(bins = 30, fill = "orange") +
          labs(title = "Budget Distribution", x = "Budget", y = "Frequency")
      }
    }
  })
  
    
  # Random Forest prediction
#  data <- search_movie_extra(search_movie(movie, 2024)$id)
    
#  movie_rf_final |>
#    predict(new_data = data) |>
#    cbind(data)
    
  # get prediction (prediction probs too if we have them)
}

