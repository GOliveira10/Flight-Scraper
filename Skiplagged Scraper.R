library(rvest)
library(RSelenium)
library(tidyverse)

airport_codes <- read_csv("airport-codes.csv")
base_url <- "https://skiplagged.com/flights"


create_flight_url <- function(from, to, leave, return){
  
  from_airport <- airport_codes[which(airport_codes$municipality == from),]
  
  if(nrow(from_airport > 1)){
    message("Multiple Airports in Departure City.")
    from_options <- from_airport %>% select(type, iata_code) %>% distinct()
    print(from_options)
    departure_prompt <- readline(prompt =  "Which one?")
    from_airport <- from_options$iata_code[as.numeric(departure_prompt)]
    
  }
  
  
  to_airport <- airport_codes[which(airport_codes$municipality == to),]
  
  if(nrow(to_airport) > 1){
    message("Multiple Airports in Arrival City.")
    to_options <- to_airport %>% select(type, iata_code) %>% distinct()
    print(to_options)
    arrival_prompt <- readline(prompt =  "Which one?")
    to_airport <- to_options$iata_code[as.numeric(arrival_prompt)]
    
  }
  
  flight_url <- paste(base_url, from_airport, to_airport, leave, return, sep="/")
  return(flight_url)
  
  
}


driver <- rsDriver(port = 4774L, browser = c("chrome"), chromever="76.0.3809.25")

remDr <- driver[["client"]]

remDr$navigate(skiplagged_url)


trip_xpath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "no-touch", " " ))]'


extract_elements <- function(x){

  elements <- remDr$findElements(using = 'xpath', x)

  extract_trip_from_element <- function(x){
    trip_data <- x$getElementText()
    trip_data <- str_split(trip_data, "\\n", simplify = TRUE) %>% unlist()
    return(trip_data)
  }

data <- list()
data <- sapply(elements, extract_trip_from_element)

return(data)
}



raw_list_to_dataframe <- function(x){
  
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
  
return(clean_data_frame)

}







skiplagged_url <- create_flight_url(from = "Seattle", 
                                    to = "Fresno",
                                    leave = "2019-09-27", 
                                    return = "2019-09-30")


data <- extract_elements(trip_xpath)

clean_data <- data %>% raw_list_to_dataframe()










