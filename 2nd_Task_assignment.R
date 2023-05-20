##############################################################################################
#1st task
##############################################################################################

library(httr)
library(jsonlite)

# Set the desired location
location <- "Hamburg"

#the API request URL
url <- "https://api.open-meteo.com/v1/forecast?latitude=48.75&longitude=9.10&hourly=temperature_2m,relativehumidity_2m,precipitation_probability,rain"

# Send the API request and retrieve the response
response <- GET(url)


# Check if the request was successful (status code 200)
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Extract the relevant data
  temperature <- data$hourly$temperature_2m
  time <- data$hourly$time
  relativehumidity <- data$hourly$relativehumidity_2m
  rain <- data$hourly$rain
  precip_probability <- data$hourly$precipitation_probability
  
  # Print the weather data
  cat("Weather in", location, "\n")
  cat("Units:\n")
  cat("Temperature:","°C\n")
  cat("Relative Humidity:","%\n")
  cat("Precipitation:", "mm\n")
  cat("Rain:","mm\n")
  
  df <- data.frame("Date_T_Time" = time, "Temperature" = temperature, "Relative_Humidity" = relativehumidity,
                   "Rain" = rain, "Precipitation_Probability" = precip_probability)
  
  print(df)
  
} else {
  cat("Error:", status_code(response), "\n")
}


##############################################################################################
#2nd task
##############################################################################################
library(rvest)
library(dplyr)

# Define the URL of the competitor website
url <- "https://www.radon-bikes.de/en/mountainbike/hardtail/bikegrid/"

# Read the HTML content of the website
html <- read_html(url)

# Scrape the model names and prices
model_names <- html %>%
  html_nodes(".m-bikegrid__info .a-heading--small") %>%
  html_text()%>%
  trimws()

model_names
# 
prices <- html %>%
  html_nodes(".m-bikegrid__price--active") %>%
  html_text() %>%
  trimws() %>%
  gsub("[^0-9.,]", "", .) %>%
  gsub(",", ".", .) %>%
  as.numeric()
  
prices <- na.omit(prices)
prices

# Define the reasonable price range
max_price <- 2000  # Maximum reasonable price

# Filter out rows with unreasonable prices
reasonable_prices <- prices <= max_price

reasonable_list <- ifelse(reasonable_prices, "Reasonable", "Not Reasonable")

for (i in seq_along(prices)) {
  price <- prices[i]
  reasonable <- ifelse(reasonable_prices[i], "Reasonable", "Not Reasonable")
  print(paste("Price:", price, "| Reasonable:", reasonable))
}

# # Create a data frame with the scraped data
data <- data.frame(Model = model_names, Price = prices, Reasonability = reasonable_list)

barplot(as.numeric(data$Price), names.arg = data$Model, col = ifelse(data$Reasonability == "Reasonable", "green", "red"),
        xlab = "Model", ylab = "Price", main = "Price by Model and Reasonability")

# Print the first 10 rows of the data frame
head(data, n = 10)

