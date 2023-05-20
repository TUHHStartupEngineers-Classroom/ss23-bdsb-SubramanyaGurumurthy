library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

class(covid_data_dt)
##  "data.table" "data.frame"

# Create a large .csv file
test_df <- data.frame(matrix(runif(10000000), nrow=1000000))
write.csv(test_df, 'test_df.csv', row.names = F)

# Time taken by read.csv to import
system.time({test_df_base <- read.csv("test_df.csv")})
# Time taken by read_csv to import
system.time({test_df_readr <- read_csv("test_df.csv")})
# Time taken by fread to import
system.time({test_dt <- fread("test_df.csv")})

test_dt <- data.table(ID = c("b","b","b","a","a","c"),
                      a  = 1:6,
                      b  = 7:12,
                      c  = 13:18)
## FROM[WHERE, SELECT/ORDER BY/UPDATE, GROUP BY]

covid_data_dt[i, j, by]

# Example (filter by year, sum cases, group by continent)
covid_data_dt[year == 2019, sum(cases), by = continentExp]

covid_data_dt[countriesAndTerritories == "Germany" & 
                lubridate::month(dateRep, label = T, abbr = F) == "June"]
covid_data_dt[1:2]

covid_data_dt[order(year, month, day, -countriesAndTerritories)]

# Return as a vector
covid_data_dt[,geoId]
# Select multiple columns
covid_data_dt[,c("geoId", "countriesAndTerritories")]

# Return as a data.table
covid_data_dt[,list(geoId)]
# Short form using .
covid_data_dt[,.(geoId)]
# Select multiple columns
covid_data_dt[,.(geoId, countriesAndTerritories)]

# Rename them directly
covid_data_dt[,.(CountryCode = geoId, country = countriesAndTerritories)]

# select columns named in a variable using the ..prefix
select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols]

# Solution 1
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]

# Solution 2
setDT(airquality)
airquality[!is.na(Ozone), .(Solar.R, Wind, Temp)]


covid_data_dt[,sum(deaths > 1000)]

# to list the observations put it in i
covid_data_dt[deaths > 1000]

covid_data_dt[,  `:=`(deaths_per_capita = deaths / popData2019,
                      cases_per_capita = cases / popData2019,
                      deaths_per_cases = deaths / cases)]

# To delete a column, assign it to NULL
covid_data_dt[, deaths_per_cases := NULL]
covid_data_dt[,date := lubridate::dmy(date)]

data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

data("mtcars") # step not absolutely necessary
mtcars$carname <- rownames(mtcars)
mtcars_dt <- as.data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              length(day)
]

covid_data_dt[country == "United_States_of_America" & 
                month == 5 & deaths < 1000, 
              .N
]

covid_data_dt[deaths > 1000, .N, by = country]

covid_data_dt[deaths > 1000, .N, by = country]

covid_data_dt[continent == "Europe",
              .(mean(cases), mean(deaths)),
              by = .(country, month, year)
]

library(magrittr) # to use the pipe
mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]

covid_cases_means <- covid_data_dt[,.(m_cases  = mean(cases) %>% round(1), 
                                      m_deaths = mean(deaths) %>% round(1)), 
                                   by = .(country)
]

covid_cases_means <- covid_data_dt[,.(m_cases  = mean(cases) %>% round(1), 
                                      m_deaths = mean(deaths) %>% round(1)), 
                                   by = .(country)
]
