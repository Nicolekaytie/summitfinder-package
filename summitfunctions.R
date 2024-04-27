#' Create csv from USGS excel data
#'
#' This function writes the summit data to a CSV file and imports it into RStudio.
#'
#' @param summits The summit data to be exported.
#' @param file The file path where the CSV will be saved.
#' @return A data frame containing the imported summit data from USGS.
#' @export
write.csv(summits, file = "C:\\Users\\nicol\\Documents\\summits.csv", row.names = FALSE)

#' Load state data and merge with summit data
#'
#' This function loads state data from RStudio, renames columns, and merges it with USGS summit data.
#'
#' @param state_data The state data loaded from RStudio.
#' @param summit_data The summit data to be merged.
#' @return Merged data frame with state and summit information.
#' @export
state <- map_data("state")
state <- state %>%
  rename(State = region)

summit_final <- left_join(state, summits, by = "State")
Merged_map <- summit_final %>% filter(!is.na( Elevation)) #merge state coordinates with summit data

#' Plot number of mountains above 2500 ft by region
#'
#' This function creates a bar plot showing the number of mountains above 2500 ft for each region.
#'
#' @param data The merged data frame containing state and summit information.
#' @return A ggplot object displaying the bar plot of regions.
#' @export
create_region_counts_plot <- function(data) {
  eastern_states <- c("maine", "new hampshire", "vermont", "massachusetts", "rhode island", "connecticut", "new york", "new jersey", "pennsylvania", "delaware", "maryland", "virginia", "north carolina", "south carolina", "georgia", "florida", "west virginia")
  western_states <- c("washington", "oregon", "california", "utah", "arizona", "nevada", "new mexico", "colorado", "wyoming", "idaho")
  midwestern_states <- c("michigan", "minnesota", "oklahoma", "nebraska", "north dakota", "south dakota", "kansas")

  # Filter mountains that are above 2500ft in elevation
  mountains_above_2500ft <- data %>%
    filter(State %in% c(eastern_states, western_states, midwestern_states),
           Elevation > 2500)

  # Create region counts
  region_counts <- mountains_above_2500ft %>%
    group_by(State) %>%
    summarise(Count = n())

  # Plot the bar graph
  ggplot(region_counts, aes(x = State, y = Count, fill = State)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of Mountains Above 2500 ft by Region",
         x = "State", y = "Number of Mountains",
         fill = "State") +
    theme_minimal()
}

#' Print Itinerary
#'
#' This function prints the itinerary.
#'
#' @param x An itinerary object with the travel plan
#' @export
print.itinerary <- function(x) {
  # Function implementation
}

#' Create coordinate objects
#'
#' This function creates a coordinate object with longitude and latitude coordinates
#'
#' @param longitude Longitude value.
#' @param latitude Latitude value.
#' @return A coordinate object.
#' @export
coordinate <- function(longitude, latitude) {
  class(c("coordinate", "numeric"))
  attr(coordinate, "longitude") <- longitude
  attr(coordinate, "latitude") <- latitude
  return(coordinate)
}

#' Calculate distance between two coordinates
#'
#' This function calculates the distance between two coordinates using Haversine formula.
#'
#' @param coord1 First coordinate.
#' @param coord2 Second coordinate.
#' @return Distance between the two coordinates in kilometers.
#' @export
distance <- function(coord1, coord2) {
  distHaversine(c(attr(coord1, "longitude"), attr(coord1, "latitude")),
                c(attr(coord2, "longitude"), attr(coord2, "latitude"))) / 1000  # Convert meters to kilometers
}

#' Generate itinerary
#'
#' This function generates an itinerary based on given destinations.
#'
#' @param destinations A list of coordinates representing custom destinations
#' @return A list containing the generated travel itinerary
#' @export
print.itinerary <- function(itinerary) {
  for (i in 1:(length(itinerary)-1)) {
    cat(itinerary[[i]], "\n")
  }
  cat(itinerary[[length(itinerary)]], "\n")
}
