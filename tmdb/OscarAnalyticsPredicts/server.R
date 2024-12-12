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

# Define server logic required to draw a histogram
function(input, output, session) {
  req(input$var)  # Ensure inputs are not null
 
  if (input$var == "genre"){
    if (input$all_or_one == "All Years, One Category"){
      output$scatterPlot <- renderPlot({
        graph_genre_all_years()
      })
    } else {
      output$dynamic_slider <- renderUI({
        tagList(
          sliderInput(inputId = "selected_year", label = "Select A Year:", 
                      min = 1975, max = 2024, value = 2000),
        )
      })
      output$scatterPlot <- renderPlot({
        req(input$year)  # Ensure inputs are not null
        graph_genre_one_year(year)
      })
    }
  }
    
    # Random Forest prediction
    data <- search_movie_extra(search_movie(movie, 2024)$id)
    
    movie_rf_final |>
      predict(new_data = data) |>
      cbind(data)
    
    # get prediction (prediction probs too if we have them)
}

