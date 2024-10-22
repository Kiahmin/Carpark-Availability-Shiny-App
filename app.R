library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(sf)             

ui <- fluidPage(
  titlePanel("Real-time Carpark Availability"),
   tabsetPanel(
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 helpText("This application displays real-time car park availability on an interactive map.")
               ),
               mainPanel(
                 leafletOutput("carparkMap"),
                 textOutput("updateTime")
               )
             )),
    tabPanel("Visualizations",
             sidebarLayout(
               sidebarPanel(
                 helpText("Visualizations of car park data.")
               ),
               mainPanel(
                 # Add plots here
               )
             ))
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Read carpark metadata from a CSV file
  carpark_metadata <- read.csv("CarparkInformation.csv")

  # Function to convert coordinates from SVY21 to WGS84 using sf
  convert_coords <- function(df) {
    # Create a simple feature object from coordinates
    st_as_sf(df, coords = c("x_coord", "y_coord"), crs = 3414) %>%  # SVY21 coordinate system
      st_transform(crs = 4326)  # Transform to WGS 84
  }

  # Convert the coordinates in the metadata
  converted_coords <- convert_coords(carpark_metadata)
  carpark_metadata$longitude <- st_coordinates(converted_coords)[, 1]
  carpark_metadata$latitude <- st_coordinates(converted_coords)[, 2]

  # Fetch carpark availability data
  carparkData <- reactivePoll(
    intervalMillis = 60000,  # Update every minute
    session = session,
    checkFunc = function() {
      Sys.time()
    },
    valueFunc = function() {
      url <- "https://api.data.gov.sg/v1/transport/carpark-availability"
      response <- GET(url)

      if (status_code(response) == 200) {
        data <- content(response, "parsed")
        
        if (length(data$items) > 0) {
          carpark_data <- data$items[[1]]$carpark_data
          
          # Process carpark data into a data frame
          output_list <- lapply(carpark_data, function(park) {
            if (!is.null(park$carpark_info) && length(park$carpark_info) > 0) {
              info <- park$carpark_info[[1]]  # Get the first item in carpark_info
              return(data.frame(
                carpark_number = park$carpark_number,
                total_lots = as.integer(info$total_lots),
                lots_available = as.integer(info$lots_available),
                lot_type = info$lot_type,
                update_datetime = park$update_datetime,
                stringsAsFactors = FALSE
              ))
            } else {
              return(NULL)  # Return NULL if no carpark_info is available
            }
          })
          
          # Combine all data frames into a single data frame, filtering out NULLs
          carpark_df <- do.call(rbind, Filter(Negate(is.null), output_list))
          
          # Include the timestamp of the data update
          return(list(data = carpark_df, timestamp = data$items[[1]]$timestamp))
        } else {
          return(list(data = NULL, timestamp = NULL))  # No items found
        }
      } else {
        return(list(data = NULL, timestamp = NULL))  # API call failed
      }
    }
  )

  output$carparkMap <- renderLeaflet({
    carpark_data <- carparkData()$data
    if (is.null(carpark_data) || nrow(carpark_data) == 0) {
      return(NULL)  # No data available
    }
    
    # Create leaflet map centered around a default location
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 103.851959, lat = 1.290270, zoom = 12)  # Center of Singapore

    # Add markers for each
        # Add markers for each carpark
    for (i in seq_len(nrow(carpark_data))) {
      # Find the matching carpark in the CSV metadata using carpark number
      matched_metadata <- carpark_metadata[carpark_metadata$car_park_no == carpark_data$carpark_number[i], ]

      if (nrow(matched_metadata) > 0) {
        lot_status <- paste("Carpark:", carpark_data$carpark_number[i],
                            "<br>Available Lots:", carpark_data$lots_available[i],
                            "<br>Total Lots:", carpark_data$total_lots[i])

        # Add markers using the longitude and latitude from matched metadata
        map <- map %>%
          addMarkers(lng = as.numeric(matched_metadata$longitude),  # Longitude from CSV
                     lat = as.numeric(matched_metadata$latitude),    # Latitude from CSV
                     popup = lot_status)
      }
    }

    return(map)
  })

  output$updateTime <- renderText({
    timestamp <- carparkData()$timestamp
    if (!is.null(timestamp)) {
      paste("Data last updated at:", timestamp)
    } else {
      "No update time available."
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)