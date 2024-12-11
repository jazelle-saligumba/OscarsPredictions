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
                   selectInput(input_id = "var", 
                               label = "Select variable to plot", 
                               choices = c("genre", "release month", "budget"), 
                               selected = "genre"), 
                   selectInput(input_id = "all_or_one", 
                               label = "Select Filtered of Data", 
                               choices = c("All Years, One Category", "One Year, All Categories"), 
                               selected = "All Years, One Category")
                 ),
                 mainPanel(
                   uiOutput(outputId = "dynamic_slider"), # Placeholder for dynamic slider
                   plotOutput(outputId = "dynamic_plot")  # Placeholder for dynamic plot
                 )
               )
      ),
      
      # Tab for RF predictions
      tabPanel("Predicting",
               sidebarLayout(
                 sidebarPanel(
                   textInput(inputId="input", label="Enter a 2024 movie", placeholder = "ex. Wicked"),
                   #actionButton("predict", "Predict")
                 ),
                 mainPanel(
                   tableOutput("word_freq_table")
                 )
               )
      )
    ),
)
