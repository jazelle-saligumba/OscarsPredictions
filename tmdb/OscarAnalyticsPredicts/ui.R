#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
    # Application title
    titlePanel("Who wins the Oscars?"),
    
    tabsetPanel(
      # Tab for Data Visualization
      tabPanel("Data Visualization",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "variable", 
                     label = "Choose a Variable to Plot", 
                     choices = c("Genre" = "genre", "Release Day" = "release_day", "Budget" = "budget")
                   ),
                   selectInput(
                     inputId = "timeframe", 
                     label = "Choose Timeframe", 
                     choices = c("All Years", "One Year")
                   ),
                   uiOutput("dynamic_slider"),  # Dynamic slider placeholder
                   #actionButton(inputId = "render", label = "Render Plot")  # Render button
                 ),
                 
                 mainPanel(
                   plotOutput(outputId = "dynamic_plot")  # Plot output placeholder
                 )
               )
      ),
      
      # Tab for RF predictions
      tabPanel("Predicting",
        sidebarLayout(
          sidebarPanel(
            textInput(inputId="movie", label="Enter a 2024 movie", placeholder = "ex. Wicked"),
            actionButton("predict", "Predict")
          ),
          mainPanel(
            textOutput("movie"),
            textOutput("data"),
            textOutput("pred")
          )
        )
      )
    ),
)
