library(mapproj)
library(ggplot2)
library(maps)
library(dplyr)
library(readxl)
library(htmltools)
library(devtools)
library(roxygen2)
library(geosphere)
library(usethis)




#' Summit data
#'
#' A dataset containing information about summits.
#'
#' @name summits
#' @docType data
#' @format A data frame with variables:
#' \describe{
#'   \item{summit_name}{Name of the summit}
#'   \item{elevation}{Elevation of the summit}
#'   \item{latitude}{Latitude of the summit}
#'   \item{longitude}{Longitude of the summit}
#' }
#' @export
"summits"

devtools::document()

#' Create csv from USGS excel data
#'
#' This function writes the summit data to a CSV file and imports it into RStudio.
#'
#' @param summits The summit data to be exported.
#' @param file The file path where the CSV will be saved.
#' @return A data frame containing the imported summit data from USGS.
#' @export
write.csv(summits, file = "C:\\Users\\nicol\\Documents\\summits.csv", row.names = FALSE)
ME_DF<- as.data.frame(summits) #create dataframe
data <- read.csv("C:\\Users\\nicol\\Documents\\summits.csv") #import csv file to Rstudio

write.csv(summits, file = "C:\\Users\\nicol\\Documents\\summit_final.csv", row.names = FALSE)
use_data(Merged_map, overwrite = TRUE)

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


ggplot(data = Merged_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Elevation), color = "green")

str(Merged_map)

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

# Call the region counts function with the dataset
create_region_counts_plot(summit_final)


#make itinerary of mountain peaks one would like to visit
#user chooses coordinates of peaks they would like to visit and uses functions below to make an itinerary

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
#'
print.itinerary <- function(itinerary) {
  for (i in 1:(length(itinerary)-1)) {
    cat(itinerary[[i]], "\n")
  }
  cat(itinerary[[length(itinerary)]], "\n")
}

# Create coordinate objects
# User chooses coordinates based on their preferred summits
Algonquin_peak <- coordinate(-72.41614, 40.94357)  # New York Algonquin peak coordinates
Hunter_mountain <- coordinate(-73.09222, 40.95502)  # New York Hunter Mtn coordinates
Mt_Rushmore <- coordinate(-96.49755, 43.01194)  # South Dakota Mt. Rushmore coordinates
Guadalupe_peak <- coordinate(-104.8144, 31.8917) # Texas Guadalupe Peak coordinates
Mt_Rainier <- coordinate(-121.6733, 45.69339) # Washington Mt. Rainier coordinates
Mt_Shasta <- coordinate(-122.1431, 37.70062) # California Mt. Shasta coordinates

# List of all coordinates
summit_coordinates <- list(
  "Algonquin" = Algonquin_peak,
  "Hunter" = Hunter_mountain,
  "Rushmore" = Mt_Rushmore,
  "Guadalupe" = Guadalupe_peak,
  "Rainier" = Mt_Rainier,
  "Shasta" = Mt_Shasta
)

# Function to generate itinerary
make_itinerary <- function(destinations) {
  itinerary <- list()
  total_distance <- 0

  for (i in 1:(length(destinations)-1)) {
    distance_km <- distance(destinations[[i]], destinations[[i+1]])
    total_distance <- total_distance + distance_km
    leg <- paste0("Travel from ", names(destinations)[i], " to ", names(destinations)[i+1], ": ", round(distance_km, 2), " km")
    itinerary[[i]] <- leg
  }

  itinerary[length(destinations)] <- paste0("Total distance traveled: ", round(total_distance, 2), " km")
  return(itinerary)
}

# Generate and print itinerary
itinerary <- make_itinerary(summit_coordinates)
print.itinerary(itinerary)

devtools::build()
devtools::install()
.Last.error
