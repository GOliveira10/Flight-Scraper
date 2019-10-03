library(rvest)
library(RSelenium)
library(tidyverse)
library(lubridate)
library(reshape2)
library(geosphere)

airport_codes <- read_csv("airport-codes.csv")
base_url <- "https://skiplagged.com/flights"

top_airports <- read_csv("top_airports.csv")

top_airports <- top_airports %>% select(RANK, CODE)
names(top_airports) <- c("rank", "iata_code")

airport_codes <- left_join(airport_codes, top_airports, by = "iata_code")
coordinates <- str_split_fixed(airport_codes$coordinates, ",", n=2) %>% as_tibble()
names(coordinates) <- c("lat", "lon")
coordinates$l <- as.numeric(coordinates$lat)
coordinates$lon <- as.numeric(coordinates$lon)
airport_codes <- bind_cols(airport_codes, coordinates)


calculate_distance <- function(from, to){
  
  lat_1 <- airport_codes %>% filter(iata_code == from) %>% .$lat %>% as.numeric()
  lon_1 <- airport_codes %>% filter(iata_code == from) %>% .$lon %>% as.numeric()
  
  lat_2 <- airport_codes %>% filter(iata_code == to) %>% .$lat %>% as.numeric()
  lon_2 <- airport_codes %>% filter(iata_code == to) %>% .$lon %>% as.numeric()
  
  distance <- distm(c(lon_1, lat_1), c(lon_2, lat_2), fun = distVincentyEllipsoid) %>% as.numeric()
  return(distance)
  
}



create_flight_url <- function(from, to, leave = NULL, return = NULL){
  
  
  if(grepl("[A-Z]{3,4}", from)){
    from_airport <- airport_codes %>% 
      filter(type %in% c("large_airport", "medium_airport")) %>% 
      filter(iata_code == from)
    
  } else{
  
  from_airport <- airport_codes %>% 
    filter(type %in% c("large_airport", "medium_airport")) %>% 
    filter(municipality == from)
  
  }
  
  if(nrow(from_airport) == 0){
  
    from_airport <- airport_codes %>% 
      filter(!type %in% c("large_airport", "medium_airport"))  %>% 
      filter(municipality == from)
    
    if(nrow(from_airport) == 0){
      message("No airports found")
    }
    
  } else if(nrow(from_airport) > 1){
    message("Multiple Airports in Departure City.")
    
    from_options <- from_airport %>% 
      select(type, iata_code) %>% distinct()
    
    print(from_options)
    departure_prompt <- readline(prompt =  "Which one?")
    from_airport <- from_options$iata_code[as.numeric(departure_prompt)]
  } else if(nrow(from_airport) == 1){
    
  from_airport <- from_airport$iata_code[1]
    
  }

if(!is.null(to)){
if(grepl("[A-Z]{3,4}", to)){
    to_airport <- airport_codes %>% 
      filter(type %in% c("large_airport", "medium_airport")) %>% 
      filter(iata_code == to)
    
  } else{
to_airport <- airport_codes %>% 
    filter(type %in% c("large_airport", "medium_airport")) %>% 
    filter(municipality == to)
  }
  
  if(nrow(to_airport) == 0){
    
    to_airport <- airport_codes %>% 
      filter(!type %in% c("large_airport", "medium_airport"))  %>% 
      filter(municipality == to)
    
    if(nrow(to_airport) == 0){
      message("No airports found")
    }
    
  } else if(nrow(to_airport) > 1){
    message("Multiple Airports in Departure City.")
    
    to_options <- to_airport %>% 
      select(type, iata_code) %>% distinct()
    
    print(to_options)
    departure_prompt <- readline(prompt =  "Which one?")
    to_airport <- to_options$iata_code[as.numeric(departure_prompt)]
    
  } else if(nrow(to_airport) == 1){
    
    to_airport <- to_airport$iata_code[1]
    
  }
} else{
  to_airport <- ""
}
  
  if(is.null(leave) & is.null(return)){
  flight_url <- paste(base_url, from_airport, to_airport, sep="/")
  } else{
    flight_url <- paste(base_url, from_airport, to_airport, leave, return, sep="/")
    
  }

  return(flight_url)
  
  
}


extract_elements <- function(x, using = 'xpath'){

  elements <- remDr$findElements(using = using, x)

  extract_trip_from_element <- function(x){
    trip_data <- x$getElementText()
    trip_data <- str_split(trip_data, "\\n", simplify = TRUE) %>% unlist()
    return(trip_data)
  }

data <- list()
data <- sapply(elements, extract_trip_from_element)

return(data)
}





raw_list_to_dataframe <- function(x, leave, return){
  
  clear_blanks <- function(x){
    if(length(x) < 3){
      return(NULL)
    } else {
      return(x)
    }
  }
  
  clean_data <- lapply(x, clear_blanks)
  

  
  clean_data_frame <- tibble()
  for(i in 1:length(clean_data)){
    
    trip_item <- clean_data[[i]]
    if(is.null(trip_item)){
      next
    }
    
    row <- tibble(duration = trip_item[grepl("[0-9]{1,2}h", trip_item)],
                  route = paste(trip_item[grepl("[A-Z]{3,4}", trip_item)], collapse = "-"),
                  departure = trip_item[grepl("[0-9]{1,2}:[0-9]{2}[a-z]{2}", trip_item)][1],
                  arrival = trip_item[grepl("[0-9]{1,2}:[0-9]{2}[a-z]{2}", trip_item)][2],
                  price = trip_item[grepl("\\$[0-9]", trip_item)]
                  )
    clean_data_frame <- bind_rows(clean_data_frame, row)
    
  }



if(nrow(clean_data_frame) != 0){
 # print(clean_data_frame)
  clean_data_frame <- clean_data_frame[complete.cases(clean_data_frame),] 
  
  
  clean_data_frame$price <- as.character(clean_data_frame$price)
  clean_data_frame$price <- as.numeric(gsub("[^0-9.]", "", clean_data_frame$price))
  clean_data_frame$leave <- leave
  clean_data_frame$return <- return
  return(clean_data_frame)
} else{
  return(tibble())  
}

}






explore_prices <- function(from = NULL, 
                           to = NULL, 
                           depart_wday = NULL, 
                           return_wday = NULL, 
                           window = NULL,
                           window_start = Sys.Date(),
                           leave = NULL, 
                           return = NULL, 
                           duration = NULL, 
                           interval = NULL,
                           one_way = FALSE,
                           try_alternate_routes = TRUE){

  
if(!isTRUE(mget(x="scraper_environment", ifnotfound = FALSE)$scraper_environment)){
  scraper_environment <- new.env()
}  

if(!is.null(scraper_environment$results)){
  
  scraper_environment$last_run <- scraper_environment$results
  
}

  
# Find trip of x duration within a given window at a given interval  
if(is.null(leave) & !is.null(duration) & is.null(depart_wday)){  
  

message("Creating date sequence")

end_date <- window_start + days(window) 

if(is.null(interval)){
  interval <- duration
  message("Defaulting to Trip Duration as Interval")
}


start <- seq.Date(from=window_start, to = end_date, by = interval)
end <- start + days(duration)

dates <- tibble(start, end)
dates$start <- as.character(dates$start)
dates$end <- as.character(dates$end)

}

  
# Find prices for a given week segment    
if(!is.null(depart_wday) & !is.null(return_wday)){

  # depart_wday <- "1"
  # return_wday <- "7"
  
  init_dates <- seq.Date(from = window_start + days(1), to = window_start + days(14), by = 1)
  
  start <- min(init_dates[which(wday(init_dates) == depart_wday)])
  end <- min(init_dates[which(wday(init_dates) == return_wday & init_dates > start)])
  
  duration <- end - start
  start <- seq.Date(from=start, by = 7, length.out = window)
  end <- seq.Date(from=end, by = 7, length.out = window)
  dates <- tibble(start, end)
  dates$start <- as.character(dates$start)
  dates$end <- as.character(dates$end)
  
}  

  
if(isTRUE(one_way)){
  
  skiplagged_url <- create_flight_url(from = from, 
                                      to = to)
  
  dates$end <- ""
  
} else{ 
  
skiplagged_url <- create_flight_url(from = from, 
                                      to = to)
    
}
 
all_dates <- tibble()






for(row in 1:nrow(dates)){
  


  path <- paste0("/", dates$start[row], "/", dates$end[row])

  new_url <- paste0(skiplagged_url, path)


  remDr$navigate(new_url)


  Sys.sleep(2)

  trip_xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "no-touch", " " ))]'


  data <- list()
  timeout <- FALSE
  start_time <- Sys.time()

repeat{
  data <- extract_elements(trip_xpath)

  Sys.sleep(.5)


  clean_data <- data %>% raw_list_to_dataframe(leave = dates$start[row], 
                                             return = dates$end[row])

  

  # clean_data$price <- as.numeric(gsub("\\$", "", clean_data$price))

if(nrow(clean_data) > 1){
  clean_data$url <- new_url
  break
} 

if(Sys.time() - start_time > 15){
  timeout <- TRUE
  break
}
}

if(timeout == TRUE){
  message("Exiting due to page timeout")
  break
}

all_dates <- bind_rows(all_dates, clean_data)

}


all_dates <- all_dates %>% arrange(price)

attr(all_dates, which = "trip") <- paste0(from, "-", to)
attr(all_dates, which = "window") <- paste0(as.character(wday(start, label = TRUE)), " to ", as.character(wday(end, label = TRUE)))
attr(all_dates, which = "duration") <- paste0("Trip Duration: ", duration, " Days")

all_dates$number_of_stops <- str_count(all_dates$route, "-")

scraper_environment$results <- all_dates

return(all_dates)



}






driver <- rsDriver(port = 4782L, browser = c("chrome"), chromever="76.0.3809.25")

remDr <- driver[["client"]]




options <- explore_prices("LAX", "GIG", duration = 14, interval = 1, window = 7, window_start = as.Date("2020-02-16"))


summarized_options <- options %>% group_by(leave, number_of_stops) %>% 
  summarize(min_price = min(price),
            median_price = median(price),
            max_price = max(price)) %>% melt(id.vars = c("leave", "number_of_stops"))

summarized_options %>%
  ggplot(aes(x=as.Date(leave), y = value, color = variable)) +
  geom_line() + 
  geom_label(aes(x=as.Date(leave), y=value, color = "lowest_prices", label = paste0("$", value)), size = 3, 
             data = (summarized_options %>% arrange(value) %>% .[c(1:10),])) + 
  scale_y_continuous(labels = scales::dollar) + 
  scale_x_date(date_labels = "%a %b %d", breaks = "14 days") +
  labs(x="Date", y = "Price", color = "") + 
  ggtitle(paste0(attr(options, which = "trip"), " ", attr(options, which = "duration"))) +
  facet_grid(~paste0(number_of_stops, " Stops")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # + ggthemes::theme_fivethirtyeight()


# airlines_xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "airlines-lg", " " ))]'

# extract_elements('//*span//[contains(concat( " ", @class, " " ), concat( " ", "airlines-lg", " " ))]', "xpath")







