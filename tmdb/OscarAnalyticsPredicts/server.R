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
 
  if (input$var == "genre"){
    if (input$all_or_one == "All Years, One Category"){
      output$scatterPlot <- renderPlot({
        req(input$xcol, input$ycol, input$colorcol)  # Ensure inputs are not null
        plot_data(mtcars, input$xcol, input$ycol, input$colorcol)
      })
    } else {
      
    }
  }
  
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    
    
    # Random Forest prediction
    data <- search_movie_extra(search_movie(movie, 2024)$id)
    
    movie_rf_final |>
      predict(new_data = data) |>
      cbind(data)
    
    # get prediction (prediction probs too if we have them)
}

