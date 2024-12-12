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
library(dplyr)
library(tidyverse)
library(jsonlite)
library(tidymodels)

# Define server logic required to draw a histogram
function(input, output, session) {
  #--------------------------- Graphing Functions ------------------------------
  graph_data_pre75 <- readRDS("graph_data_pre75.rds")
  
  # graphing the genre data (one year, all categories)
  graph_genre_one_year <- function(selected_year){
    graph_data_pre75$winner <- factor(graph_data_pre75$winner, levels = c("True", "False"))
    
    graph_data_pre75 |>
      filter(year_ceremony == selected_year) |>
      unnest(genre_ids) |>
      distinct() |>
      mutate(genre_ids = genres_dict[as.character(genre_ids)]) |>
      ggplot(aes(x = genre_ids, fill = winner, alpha = winner)) + 
      geom_bar(position = "stack") +
      theme_minimal() + 
      scale_fill_manual(values = c("False" = "black", "True" = "#af9150")) + 
      scale_alpha_manual(values = c("False" = 0.9, "True" = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_text(size = 16, color = "black", face = "bold"), # Dark and bold axis titles
            axis.text = element_text(size = 12, color = "black"),
            legend.text = element_text(size = 12,  color = "black"), # Legend text font size
            legend.title = element_text(size = 16,  color = "black", face = "bold"))                # Dark axis text)
  }
  
  # graphing the genre data (all years, one category)
  graph_genre_all_years <- function(){
    graph_data_pre75 |>
      filter(category == "BEST PICTURE") |>
      unnest(genre_ids) |>
      distinct() |>
      mutate(genre_ids = genres_dict[as.character(genre_ids)]) |>
      ggplot(aes(x = year_film, y = genre_ids, color = winner, alpha = winner)) + 
      geom_jitter(position = position_jitter(width = 0.1, height = 0.2),
                  size = 3) +
      scale_color_manual(values = c("True" = "#af9150", "False" = "black")) + 
      scale_alpha_manual(values = c("True" = 1, "False" = 0.47)) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 16, color = "black", face = "bold"), # Dark and bold axis titles
            axis.text = element_text(size = 12, color = "black"),
            legend.text = element_text(size = 12,  color = "black"), # Legend text font size
            legend.title = element_text(size = 16,  color = "black", face = "bold"))    
  }
  
  # graphing the release, budget data (one year, all categories) 
  graph_one_year_scatter <- function(feature, selected_year) {
    graph_data_pre75 |>
      filter(year_ceremony == selected_year) |>
      ggplot(aes(x = !!sym(feature), y = 0, color = winner, alpha = winner)) +
      geom_jitter(position = position_jitter(height = 0.1),
                  size = 3) + 
      scale_color_manual(values = c("True" = "#af9150", "False" = "black")) + 
      scale_alpha_manual(values = c("True" = 1, "False" = 0.47)) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 16, color = "black", face = "bold"), # Dark and bold axis titles
            axis.text = element_text(size = 12, color = "black"),
            legend.text = element_text(size = 12,  color = "black"), # Legend text font size
            legend.title = element_text(size = 16,  color = "black", face = "bold"))    
  }
  
  # graphing the release, budget data (all years, one category)
  graph_all_years <- function(feature){
    graph_data_pre75 |>
      filter(category == "BEST PICTURE") |>
      ggplot(aes(x = year_film, y = !!sym(feature), color = winner, alpha = winner)) +
      geom_jitter(position = position_jitter(height = 0.1),
                  size = 3) + 
      scale_color_manual(values = c("True" = "#af9150", "False" = "black")) + 
      scale_alpha_manual(values = c("True" = 1, "False" = 0.47)) +
      theme_minimal() + 
      theme(axis.title = element_text(size = 16, color = "black", face = "bold"), # Dark and bold axis titles
            axis.text = element_text(size = 12, color = "black"),
            legend.text = element_text(size = 12,  color = "black"), # Legend text font size
            legend.title = element_text(size = 16,  color = "black", face = "bold"))    
  }
  
  
  #--------------------------- Data Visualization ------------------------------
  # Dynamically render the year slider if "One Year, All Categories" is selected
  output$dynamic_slider <- renderUI({
    if (input$timeframe == "One Year, All Categories") {
      sliderInput(inputId = "selected_year", label = "Select a Year:", 
                  min = 1975, max = 2024, value = 2000)
    }
  })
  
  # Create a reactive expression to ensure slider input is detected
  selected_year <- reactive({
    if (input$timeframe == "One Year, All Categories") {
      req(input$selected_year) # Ensure slider input is valid
      input$selected_year
    }
  })
  
  # Render the plot reactively based on user inputs
  output$dynamic_plot <- renderPlot({
    # Ensure inputs are valid
    req(input$variable, input$timeframe)
    
    # Use conditional logic for plotting
    if (input$timeframe == "All Years, Best Picture Category") {
      
      if(input$variable == "genre"){
        graph_genre_all_years()
        
      } else if (input$variable == "release_day"){
        graph_all_years("release_day_val")
        
      } else if (input$variable == "budget"){
        graph_all_years("budget")
        
      }
    } else if (input$timeframe == "One Year, All Categories") {
      req(selected_year()) # Ensure year is selected
      
      if(input$variable == "genre"){
        graph_genre_one_year(selected_year())
        
      } else if (input$variable == "release_day"){
        graph_one_year_scatter("release_day_val", selected_year())
        
      } else if (input$variable == "budget"){
        graph_one_year_scatter("budget", selected_year())
      }
    }
  })
  
  #------ Prepping API Connection and Functions to Help Make Prediction  -------
  # Set up TMDB auth
  url <- "https://api.themoviedb.org/3/authentication"
  
  personal_authorization <- paste0('Bearer ', Sys.getenv("TMDB_API_KEY"))

  response <- VERB("GET", url, add_headers('Authorization' =  personal_authorization), 
  content_type("application/octet-stream"), accept("application/json"))
  
  content(response, "text")
  
  search_movie <- readRDS("search_movie.rds")
  search_movie_extra <- readRDS("search_movie_extra.rds")
  movie_rf <- readRDS("movie_rf_final.rds")
  
  input_fields <- c("adult", "backdrop_path", "belongs_to_collection", "budget", 
                    "genres", "homepage", "id", "imdb_id", "original_language", 
                    "original_title", "overview", "popularity", "poster_path", 
                    "production_companies", "production_countries", "release_date", 
                    "revenue", "runtime", "spoken_languages", "status", "tagline", 
                    "title", "video", "vote_average", "vote_count")
  
  # Random Forest prediction
  observeEvent(input$predict, {
  output$movie <- renderText({
    input$movie
  })
  
  genres_dict <- readRDS("genres_dict.rds")
  genres_list <- readRDS("genres_list.rds")$name
  top_countries <- c("US", "GB", "FR",  "DE", "IT", "CA")
  top_languages <- c("English", "French", "German", "Spanish", "Italian")

  wider_by_genre <- function(df) {
    # make sure all values are vectors
    df$genre_ids <- map(df$genre_ids, ~ if (is.list(.)) unlist(.) else .)

    # now, change all the number values to be genres(char)
    new_df <- df |>
      unnest(genre_ids) |>
      distinct() |>
      mutate(genre_ids = genres_dict[as.character(genre_ids)]) |>
      filter(!is.na(genre_ids)) |>
      mutate(valid_id = 1) |> # create binary value for the pivot_wider to use
      pivot_wider(names_from = genre_ids, values_from = valid_id,
                  values_fill = 0) # use names_prefix if want "genre_"

    return(new_df)
  }

  # more generic flattening of lists
  flatten_lists <- function(df, feature, top_vector, prefix) {
    # make sure all values in the specified column are vectors
    df[[feature]] <- map(df[[feature]], ~ if (is.list(.)) unlist(.) else .)
    df$feature[is.null(df$feature)] <- NA

    # now, change all the number values
    new_df <- df |>
      unnest({{feature}}) |>  # unnest the specified column
      mutate(valid_value = 1, # create binary value for pivot_wider
              temp_feature = case_when(
                .data[[feature]] %in% top_vector ~ as.character(.data[[feature]]),
                TRUE ~ "Other")) |>
      select(-{{feature}}, -feature) |>
      distinct() |>
      pivot_wider(names_from = temp_feature, values_from = valid_value,
                  names_prefix = prefix, values_fill = 0)

    return(new_df)
  }

  data <- search_movie_extra(personal_authorization,
                              search_movie(personal_authorization, input$movie, 2024)$id,
                              input_fields)

  output$data <- renderText({
    data$overview
  })

  data <- data |>
    mutate(genre_ids = genres) |>
    select(-genres) |>
    wider_by_genre() |>
    flatten_lists("production_countries", top_countries, "country_") |>
    flatten_lists("spoken_languages", top_languages, "lang_") |>
    #flatten_lists("production_companies", top_companies "company_") |>
    select(-belongs_to_collection, -production_companies) |> # don't need these list columns
    mutate(release_month = as.integer(substr(release_date, 6, 7))) |>
    mutate(release_year = as.integer(substr(release_date, 1, 4)))

  for (col in genres_list) {
    if (!(col %in% names(data))) {
      data[[col]] <- 0
    }
  }

  for (col in c(top_countries, "Other")) {
    if (!(paste0("country_", col) %in% names(data))) {
      data[[paste0("country_", col)]] <- 0
    }
  }

  for (col in c(top_languages, "Other")) {
    if (!(paste0("lang_", col) %in% names(data))) {
      data[[paste0("lang_", col)]] <- 0
    }
  }

  pred <- movie_rf |>
    predict(new_data = data) |>
    cbind(data)
  
  output$pred <- renderText({
    as.logical(pred$.pred_class)
  })
  
})



 
}

