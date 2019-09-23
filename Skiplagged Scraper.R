library(rvest)
library(RSelenium)
library(tidyverse)
library(lubridate)
library(reshape2)

airport_codes <- read_csv("airport-codes.csv")
base_url <- "https://skiplagged.com/flights"



create_flight_url <- function(from, to, leave = NULL, return = NULL){
  
  
  from_airport <- airport_codes %>% 
    filter(type %in% c("large_airport", "medium_airport")) %>% 
    filter(municipality == from)
  
  
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


 # to = "Fresno"
to_airport <- airport_codes %>% 
    filter(type %in% c("large_airport", "medium_airport")) %>% 
    filter(municipality == to)
  
  
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
  
  
  if(is.null(leave) | is.null(return)){
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
     
clean_data_frame <- clean_data_frame[complete.cases(clean_data_frame),] 
clean_data_frame$leave <- leave
clean_data_frame$return <- return
  
return(clean_data_frame)

}






explore_prices <- function(from, 
                           to, 
                           depart_wday, 
                           return_wday, 
                           window, 
                           leave = NULL, 
                           return = NULL, 
                           duration = NULL, 
                           interval = NULL){

  
if(is.null(leave) & !is.null(duration)){  
  

message("Creating date sequence")

end_date <- Sys.Date() + days(window) 

if(is.null(interval)){
  interval <- duration
  message("Defaulting to Trip Duration as Interval")
}


start <- seq.Date(from=Sys.Date(), to = end_date, by = interval)
end <- start + days(duration)

dates <- tibble(start, end)
dates$start <- as.character(dates$start)
dates$end <- as.character(dates$end)

}
  
if(!is.null(depart_wday) & !is.null(return_wday)){

  init_dates <- seq.Date(from = Sys.Date() + days(1), to = Sys.Date() + days(7), by = 1)
  
  start <- init_dates[which(wday(init_dates) == depart_wday)]
  end <- init_dates[which(wday(init_dates) == return_wday)]
  
  duration <- end - start
  start <- seq.Date(from=start, by = 7, length.out = window)
  end <- seq.Date(from=end, by = 7, length.out = window)
  dates <- tibble(start, end)
  dates$start <- as.character(dates$start)
  dates$end <- as.character(dates$end)
  
}  
  
  

skiplagged_url <- create_flight_url(from = from, 
                                      to = to)
    
  
all_dates <- tibble()

for(row in 1:nrow(dates)){

path <- paste0("/", dates$start[row], "/", dates$end[row])

new_url <- paste0(skiplagged_url, path)


remDr$navigate(new_url)


Sys.sleep(2)

trip_xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "no-touch", " " ))]'


# data <- list()
# repeat{
data <- extract_elements(trip_xpath)

Sys.sleep(1)
if(length(data) == 0){
  
  data <- extract_elements(trip_xpath)
}


clean_data <- data %>% raw_list_to_dataframe(leave = dates$start[row], 
                                             return = dates$end[row])

Sys.sleep(1)

if(nrow(clean_data) == 0){
  clean_data <- data %>% raw_list_to_dataframe(leave = dates$start[row], 
                                               return = dates$end[row])
  
}


all_dates <- bind_rows(all_dates, clean_data)

}

all_dates$price <- as.numeric(gsub("\\$", "", all_dates$price))
all_dates <- all_dates %>% arrange(price)
return(all_dates)

}

driver <- rsDriver(port = 4779L, browser = c("chrome"), chromever="76.0.3809.25")

remDr <- driver[["client"]]




options <- explore_prices("Los Angeles", "Seattle", depart_wday = "5", return_wday = "2", window = 12)


options %>% group_by(leave) %>% 
  summarize(min_price = min(price),
            median_price = median(price),
            max_price = max(price)) %>% melt(id.vars = "leave") %>% 
  ggplot(aes(x=as.Date(leave), y = value, color = variable)) +
  geom_line() + scale_y_continuous(labels = scales::dollar) + scale_x_date(date_labels = "%a %B %d", breaks = "7 days") +
  labs(x="Date", y = "Price") + ggtitle("Thurs - Monday Trips fromn LAX to Seattle")


# airlines_xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "airlines-lg", " " ))]'

# extract_elements('//*span//[contains(concat( " ", @class, " " ), concat( " ", "airlines-lg", " " ))]', "xpath")







