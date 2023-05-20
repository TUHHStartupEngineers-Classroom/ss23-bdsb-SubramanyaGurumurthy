# library(tidyverse)
# library(lubridate)
# 
# # Read the data from the CSV file
# data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# 
# # Filter data for the 5 continents
# filtered_data <- data %>%
#   filter(continent %in% c("Africa", "Asia", "Europe", "North America", "South America"))
# 
# # Convert date column to date format
# filtered_data$date <- ymd(filtered_data$date)
# 
# # Calculate the cumulative cases
# filtered_data <- filtered_data %>%
#   group_by(date, continent) %>%
#   summarize(cumulative_cases = sum(total_cases, na.rm = TRUE)) %>%
#   ungroup()
# 
# # Convert total cases to millions
# filtered_data$cumulative_cases <- filtered_data$cumulative_cases / 1e6
# 
# # Filter data until May 2022
# filtered_data <- filtered_data %>%
#   filter(date <= ymd("2022-05-01"))
# 
# # Find the highest values of cases for Asia, Europe, and Africa
# highest_cases <- filtered_data %>%
#   filter(continent %in% c("Asia", "Europe", "Africa")) %>%
#   group_by(continent) %>%
#   filter(cumulative_cases == max(cumulative_cases)) %>%
#   ungroup()
# 
# # Plot the cumulative cases
# ggplot(filtered_data, aes(x = date, y = cumulative_cases, color = continent)) +
#   geom_line() +
#   geom_point(data = highest_cases, aes(shape = continent), size = 3) +
#   geom_text(data = highest_cases, aes(label = round(cumulative_cases, 2), x = date, y = cumulative_cases),
#             color = "black", vjust = -0.5, size = 3) +
#   labs(x = "Timeline in Months", y = "Cumulative Cases (Millions)", title = "Covid-19 confirmed cases worldwide",
#        subtitle = "until 06-2022") +
#   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
#   scale_y_continuous(labels = scales::comma) +
#   scale_color_manual(values = c("Africa" = "red", "Asia" = "blue", "Europe" = "green",
#                                 "North America" = "purple", "South America" = "orange")) +
#   scale_shape_manual(values = c("Asia" = 17, "Europe" = 15, "Africa" = 19)) +
#   theme_dark() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


##############################################################################################
#1st task
##############################################################################################

library(tidyverse)
library(lubridate)

# Read the data from the CSV file
data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Filter data for the selected countries in Europe and the United States
countries <- c("Germany", "United Kingdom", "France", "Spain", "United States")
filtered_data <- data %>%
  filter(location %in% countries)
 
filetered_conti <- data %>%
  filter(location %in% c("Europe"))

# Convert date column to date format
filtered_data$date <- ymd(filtered_data$date)
filetered_conti$date <- ymd(filetered_conti$date)

# Calculate the cumulative cases
filtered_data <- filtered_data %>%
  group_by(date, location) %>%
  summarize(cumulative_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup()

filetered_conti <- filetered_conti %>%
  group_by(date, location) %>%
  summarize(cumulative_cases = sum(total_cases, na.rm = TRUE)) %>%
  ungroup()

filtered_data <- bind_rows(filtered_data, filetered_conti)


# Convert total cases to millions
filtered_data$cumulative_cases <- filtered_data$cumulative_cases / 1e6

# Filter data until May 2022
filtered_data <- filtered_data %>%
  filter(date <= ymd("2022-05-01"))

# Find the highest values of cases for Asia, Europe, and Africa
highest_cases <- filtered_data %>%
  filter(location %in% c("Europe", "United States")) %>%
  group_by(location) %>%
  filter(cumulative_cases == max(cumulative_cases)) %>%
  ungroup()

# Plot the cumulative cases
ggplot(filtered_data, aes(x = date, y = cumulative_cases, color = location)) +
  geom_line() +
  geom_text(data = highest_cases, aes(label = round(cumulative_cases, 2), x = date, y = cumulative_cases),
            color = "black", vjust = -0.5, size = 3) +
  labs(x = "Timeline in Months", y = "Cumulative Cases (Millions)", title = "Covid-19 confirmed cases worldwide",
       subtitle = "Until 06-2022") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  scale_color_manual(values = c("Germany" = "blue", "United Kingdom" = "red",
                                "France" = "green", "Spain" = "orange",
                                "United States" = "purple", "Europe" = "cyan"),
                     ) +
  # scale_shape_manual(values = c("Europe" = 17, "United States" = 15, "France" = 19)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################################
#2nd task
##############################################################################################
library(tidyverse)
library(scales)

# Read the data from the CSV file
data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Convert date column to date format
data$date <- as.Date(data$date)

# Filter data for the latest date
latest_data <- data %>%
  filter(date == max(date))

# Calculate mortality rate
latest_data <- latest_data %>%
  mutate(mortality_rate = total_deaths / population) %>%
  select(location, mortality_rate) %>%
  mutate(location = case_when(
    
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
    
  )) %>%
  distinct()

world <- map_data("world")
# Perform the left join
world <- left_join(world, latest_data, by = c("region" = "location"))

ggplot(world) +
  geom_map(aes(map_id = region, fill= mortality_rate), map = world) +
  expand_limits(x = world$long, y = world$lat) +
  coord_map()+
  scale_fill_continuous(labels = percent_format(), name = "Mortality Rate")+
  labs(title="Confirmed Covid-19 deaths relative to the size of the Population",
       subtitle = "Around 6.2 Million confirmed COVID-19 deaths worldwide")


