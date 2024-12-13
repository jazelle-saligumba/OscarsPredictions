library(shiny)
library(DT)
library(dplyr)
library(spotifyr)

# CURRENTLY ASSUMING THAT ACCESS TOKEN IS ALREADY STORED IN ENVIRONMENT 
# each proportion through the setlist has an offset value that we will add to the calculated artists
# use get_mean_artist_features() to retrieve mean values of an artists tracks holds the mean values for each artist 
# then we will add the offset and use that as our target value in the get_recommendations call
  # we will do this for every proportion of the setlist 
acousticness_offset <- c()
danceability_offset <- c()
duration_ms_offset <- c()
energy_offset <- c()

# code for UI (the webpage that the user will interact with)
ui <- fluidPage(

    # Application title
    titlePanel("Spotify Setlist Recommendation"),
    
    sidebarLayout(
      sidebarPanel(
        # Asking for artist name 
        #TODO: find function that takes artist name and retrieves artist's spotify ID so that we can just ask for name 
        textInput(inputId="artist", label="SpotifyID of Artist Touring:", placeholder = "e.g. 06HL4z0CvFAxyc27GXpf02"),
        
        #textInput(inputId="num_songs", label="Number of Songs in Setlist", placeholder = "e.g. 20"),
        
        actionButton(inputId="get_recommendations", label="Get Recommendations")
      ),
      
      mainPanel(
        DTOutput("recommendations_table")
      )
    ),
)

# server = code for computer (what server should do w/ inputs as user changes)
server <- function(input, output) {

  # Reactive value to store the list of recommendations
  recommendations_list <- reactiveVal(list())
  
  # Generate recommendations when the button is clicked
  observeEvent(input$get_recommendations, {
    # Reactive expression to generate artist means based on input
    artist_means <- reactive({
      req(input$artist)  # Ensure input$artist is not null or empty
      get_mean_artist_features(input$artist, audio_features)
    })
    print("retrieve means")
    
    req(artist_means())  # Ensure artist_means is available
    
    recs <- list()  # Temporary list to store recommendations
    i <- 1
    print("before loop")
    while (i <= 10) {
      recommendation <- tryCatch({
        get_recommendations(
          limit = 100,
          seed_artists = ifelse(input$artist == "", NULL, input$artist),
          target_acousticness = artist_means()$acousticness[1] + acousticness_offset[i],
          #target_danceability = artist_means()$danceability[1] + danceability_offset[i],
          #target_duration_ms = artist_means()$duration_ms[1] + duration_ms[i],
          #target_energy = artist_means()$energy[1] + energy_offset[i],
          #target_instrumentalness = artist_means()$instrumentalness[1] + instrumentalness_offset[i],
          #target_key = artist_means()$key[1] + key_offset[i],
          #target_loudness = artist_means()$loudness[1] + loudness_offset[i],
          #target_mode = artist_means()$mode[1] + mode_offset[i],
          #target_tempo = artist_means()$tempo[1] + tempo_offset[i],
          #target_valence = artist_means()$valence[1] + valence_offset[i],
          authorization = access_token
        )
      }, error = function(e) {
        showNotification(paste("Error fetching recommendations:", e$message), type = "error")
        NULL
      })
      
      # Check if recommendation is valid and filter
      if (!is.null(recommendation)) {
        # if the recommendations produced contain a song by the artist desired, add it and move on to next proportion
        recommendation <- recommendation %>% 
          filter(ARTIST == input$artist)  # Filter by artist
        
        if (nrow(recommendation) >= 1) {
          # uses the first recommendation and puts at index i of the list
          recs[[i]] <- recommendation |>
            head(1)
  
          i <- i + 1
        }
        # otherwise, we want to rerun this iteration of the for loop (don't update i)
      }
    }
    
    # Store the final list of recommendations in the reactive value
    recommendations_list(do.call(rbind, recs))
  })
  
  # Render the recommendations table
  output$recommendations_table <- renderDT({
    req(recommendations_list())  # Ensure recommendations_list is not null
    
    recommendations_list() %>% 
      bind_rows() %>%
      select(ARTIST_NAME, TRACK_NAME, DURATION_MS, POPULARITY) %>% 
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })
}


# compiles everything and runs the application 
shinyApp(ui = ui, server = server)
