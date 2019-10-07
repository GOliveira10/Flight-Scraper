source("Skiplagged Scraper.R")


driver <- rsDriver(port = 4444L, browser = c("chrome"), chromever="76.0.3809.126")

remDr <- driver[["client"]]




options <- explore_prices("FAT", "SJU", duration = 4, interval = 1, window = 7, window_start = "2019-12-27")


construct_depart_options <- function(x, depart_city, arrival_city){
  
  #x <- options 

  
  depart_options <- x %>% filter(type == "depart_options")
  
  depart_options <- depart_options %>% 
    mutate(departure_time = as.POSIXct(paste0(leave, " ", departure))) %>%
    mutate(arrival_time = departure_time + hours(as.numeric(gsub("h", "", duration))))
  
  first_leg <- depart_options %>% filter(from == depart_city)
  second_leg <- depart_options %>% filter(to == arrival_city)
  
  trips <- full_join(first_leg, second_leg, by = c("to" = "from"))
  
  possible_trips <- trips %>% filter(arrival_time.x < departure_time.y)
  
  possible_trips <- possible_trips %>% mutate(price = price.x + price.y,
                                              full_route = paste0(route.x, route.y, collapse = "-"),
                                              full_duration = as.numeric(gsub("h", "", duration.x)) + as.numeric(gsub("h", "", duration.y)))
  

  
  summary <- possible_trips %>% group_by(leave.x) %>% summarize(min_price = min(price),
                                                     min_duration = min(duration))

  
  result <- list(possible_trips,
                 summary)
  
  return(result)
}



round_trips <- options %>% filter(type == "normal")


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

