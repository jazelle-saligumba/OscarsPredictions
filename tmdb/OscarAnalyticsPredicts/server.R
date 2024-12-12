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
  
    
  # Set up TMDB auth
  url <- "https://api.themoviedb.org/3/authentication"
  
  personal_authorization <- paste0('Bearer ', config::get("tmdb_api_key"))

  response <- VERB("GET", url, add_headers('Authorization' =  personal_authorization), 
  content_type("application/octet-stream"), accept("application/json"))
  
  content(response, "text")
  
  search_movie <- readRDS("search_movie.rds")
  search_movie_extra <- readRDS("search_movie_extra.rds")
  movie_rf_final <- readRDS("movie_rf_final.rds")
  
  input_fields <- c("adult", "backdrop_path", "belongs_to_collection", "budget", "genres", "homepage", "id", "imdb_id", "original_language", "original_title", "overview", "popularity", "poster_path", "production_companies", "production_countries", "release_date", "revenue", "runtime", "spoken_languages", "status", "tagline", "title", "video", "vote_average", "vote_count")
  
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

  pred <- movie_rf_final |>
    predict(new_data = data) |>
    cbind(data)
  
  output$pred <- renderText({
    as.logical(pred$.pred_class)
  })
  
})



 
}

