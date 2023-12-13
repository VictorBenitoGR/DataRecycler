# DataRecycler

> "Information is the oil of the 21st century, and analytics is the combustion engine."  
> — Peter Sondergaard (Gartner IT Symposium/Xpo, October 2011).

The intelligent collection, cleaning, processing and visualization of data is an indispensable process for the efficient, precise, productive and sharp **self-criticism** of any project.

This project seeks to be a tool to reuse a database without compromising its sensitive data, maintaining magnitudes, data types and mathematical relationships between columns in order to have better control over the degree of randomization.

## Current options

### POSIXct
```R
# ! Use POSIXct to manipulate date and time in seconds like 2023-12-31 23:59:59

# ! The code is adapted in case you will use more than one time zone

# * Verify if the class is POSIXct

DateTimeColumns <- c('DateTimeUTC', 'DateTimeLocal') # Replace column names

VerifyPOSIXct <- lapply(dataset[DateTimeColumns], class)

VerifyPOSIXct # The result should be "POSIXct" "POSIXt" for 2023-12-31 23:59:59

# * Transform them to POSIXct

convertToPOSIXct <- function(dataframe, columns) {
  for (col in columns) {
    if (!inherits(dataframe[[col]], "POSIXct")) {
      dataframe[[col]] <- as.POSIXct(dataframe[[col]],
                                     format = "%Y-%m-%d %H:%M:%S")
    }
  }
  return(dataframe)
}

dataset <- convertToPOSIXct(dataset, c('DateTimeUTC', 'DateTimeLocal'))
```


### Dates
```R
# * Randomize the entire date, specify some values
# * and randomize the rest, or specify everything

# The function allows us to randomize without losing the sequence of the data
randomizeDateTime <- function(dataset, randomize = TRUE, 
                              preferred_year = NULL,
                              preferred_month = NULL,
                              preferred_day = NULL, 
                              preferred_hour = NULL,
                              preferred_minute = NULL,
                              preferred_second = NULL) {
  
  # Sort by dates to maintain the sequence
  dataset <- dataset[order(dataset$DateTimeUTC), ]
  
  if (randomize) {
    # ! Define your preferred range
    preferred_years <- seq(2014, 2016)  # ! Example
    preferred_months <- seq(1, 12)
    preferred_days <- seq(1, 31) # ! make_datetime (lubridate) prevents errors
    preferred_hours <- seq(0, 23)
    preferred_minutes <- seq(0, 59)
    preferred_seconds <- seq(0, 59)
    
    # Randomly select values from the preferred ranges if not specified
    random_year <- ifelse(is.null(preferred_year),
                          sample(preferred_years, 1), preferred_year)
    
    random_month <- ifelse(is.null(preferred_month),
                           sample(preferred_months, 1), preferred_month)
    
    random_day <- ifelse(is.null(preferred_day),
                         sample(preferred_days, 1), preferred_day)
    
    random_hour <- ifelse(is.null(preferred_hour),
                          sample(preferred_hours, 1), preferred_hour)
    random_minute <- ifelse(is.null(preferred_minute),
                            sample(preferred_minutes, 1), preferred_minute)
    
    random_second <- ifelse(is.null(preferred_second),
                            sample(preferred_seconds, 1), preferred_second)
  } else {
    # Use the specified values if randomization is not required
    random_year <- ifelse(is.null(preferred_year),
                          sample(preferred_years, 1), preferred_year)
    
    random_month <- ifelse(is.null(preferred_month),
                           sample(preferred_months, 1), preferred_month)
    
    random_day <- ifelse(is.null(preferred_day),
                         sample(preferred_days, 1), preferred_day)
    
    random_hour <- ifelse(is.null(preferred_hour),
                          sample(preferred_hours, 1), preferred_hour)
    
    random_minute <- ifelse(is.null(preferred_minute),
                            sample(preferred_minutes, 1), preferred_minute)
    
    random_second <- ifelse(is.null(preferred_second),
                            sample(preferred_seconds, 1), preferred_second)
  }
  
  # Calculate the differences
  year_difference <- random_year - year(dataset$DateTimeUTC[1])
  month_difference <- random_month - month(dataset$DateTimeUTC[1])
  day_difference <- random_day - day(dataset$DateTimeUTC[1])
  hour_difference <- random_hour - hour(dataset$DateTimeUTC[1])
  minute_difference <- random_minute - minute(dataset$DateTimeUTC[1])
  second_difference <- random_second - second(dataset$DateTimeUTC[1])
  
  # Apply the differences uniformly across all rows using make_datetime
  dataset <- dataset %>% mutate(DateTimeUTC =
                                  make_datetime(year(DateTimeUTC),
                                                month(DateTimeUTC),
                                                day(DateTimeUTC),
                                                hour(DateTimeUTC),
                                                minute(DateTimeUTC),
                                                second(DateTimeUTC))
                                + years(year_difference)
                                + months(month_difference)
                                + days(day_difference)
                                + hours(hour_difference)
                                + minutes(minute_difference)
                                + seconds(second_difference))
  
  return(dataset)
}
```

```R
# * Example usage

# Randomize everything
dataset_randomized <- randomizeDateTime(dataset, randomize = TRUE)

# Specify some components, randomize others
dataset_mixed <- randomizeDateTime(dataset, randomize = TRUE,
                                   preferred_year = 2021,
                                   preferred_hour = 12,
                                   preferred_minute = 30)

# Specify everything
dataset_specified <- randomizeDateTime(dataset, randomize = FALSE,
                                       preferred_year = 2022,
                                       preferred_month = 6,
                                       preferred_day = 15,
                                       preferred_hour = 18,
                                       preferred_minute = 45,
                                       preferred_second = 30)

# Continue with your preferred dataframe
dataset <- dataset_randomized
dataset <- dataset_mixed
dataset <- dataset_specified
```

```R
# * Redo the timezones of the other columns if necessary

# In this example the difference between UTC and Monterrey MX
# is -5 hours, equal to -18000 seconds
dataset$DateTimeLocal <- dataset$DateTimeUTC - 18000

```


### Alphanumeric
```R
# ! Use this to before and after in order to confirm the expected changes
head(dataset$IDUser, 10)

# * Randomize the first 19 characters (example)

head(dataset$IDUser, 10)

dataset$IDUser <- ave(dataset$IDUser, dataset$IDUser, FUN = function(x) {
  new_id <- paste0(sample(c(0:9, letters, LETTERS),
                          19, replace = TRUE),collapse = "")
  sub("^.{19}", new_id, x)
})

head(dataset$IDUser, 10)
```

```R
# * Randomize the last 19 characters (example)

head(dataset$IDUser, 10)

dataset$IDUser <- ave(dataset$IDUser, dataset$IDUser, FUN = function(x) {
  new_id <- paste0(sample(c(0:9, letters, LETTERS),
                          19, replace = TRUE), collapse = "")
  sub(".{19}$", new_id, x)
})

head(dataset$IDUser, 10)
```

```R
# * Randomize a specific range of characters
# ! This segment requires a lot of time for large dataframes (+1M obs.)

head(dataset$IDUser, 10)

# set.seed(123) # Setting seed for reproducibility
range_start <- 20
range_end <- 30
dataset$IDUser <- ave(dataset$IDUser, dataset$IDUser, FUN = function(x) {
  new_id <- paste0(sample(c(0:9, letters, LETTERS),
                          length(x), replace = TRUE), collapse = "")
  substr(x, range_start, range_end) <- new_id
  x
})

head(dataset$IDUser, 10)
```


### Remove characters
```R
# * Delete range of characters counting from the beginning

unique(dataset$Version)
dataset$Version <- paste0(substr(dataset$Version, 1, 12),     # First
                          substr(dataset$Version, 15,         # Last
                                 nchar(dataset$Version)))
unique(dataset$Version)
```

```R
# * Delete range of characters counting from the end

unique(dataset$Version)
dataset$Version <- paste0(substr(dataset$Version, 1,
                                 nchar(dataset$Version) - 8), # Last
                          substr(dataset$Version,
                                 nchar(dataset$Version) - 3,  # First
                                 nchar(dataset$Version)))
unique(dataset$Version)

```


## Future work

- Local UX/UI.
- Connection with SQL Databases.
- Connection to GPT API key to randomize qualitative data.


## Contact

Feel free to reach out if you have any questions or feedback.

- **Email:** victorbenitogr@gmail.com
- **The subject must start with:**  [DataRecycler]