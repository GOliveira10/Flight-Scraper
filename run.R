source("Skiplagged Scraper.R")


driver <- rsDriver(port = 4447L, browser = c("chrome"), chromever="76.0.3809.126")

remDr <- driver[["client"]]



options <- explore_prices("CDG", "NRT", duration = 7, interval = 3, window = 60, window_start = "2019-12-27")


depart_options <- construct_options(x = options, depart_city = "CDG", arrival_city = "NRT", which = "depart_options")

arrive_options <- construct_options(x = options, depart_city = "NRT", arrival_city = "CDG", which = "return_options")

round_trips <- options %>% filter(type == "normal")

round_options <- round_trips %>% group_by(leave, return) %>% summarize(min_price = min(price),
                                              min_duration = min(duration),
                                              type = "round_trip")



round_options$leave <- as.Date(round_options$leave)
round_options$return <- as.Date(round_options$return)


one_way_options <- crossing(depart_options[[2]], arrive_options[[2]]) 

one_way_options$leave_departure_one_way <- as.Date(one_way_options$leave_departure_one_way) 
one_way_options$leave_arrival_one_way <- as.Date(one_way_options$leave_arrival_one_way)

one_way_options <- one_way_options %>% 
  filter(leave_departure_one_way < leave_arrival_one_way) %>% group_by(leave = leave_departure_one_way, return = leave_arrival_one_way) %>% 
  summarize(min_price = sum(min_price, min_price1),
            min_duration = sum(min_duration, min_duration1),
            type = "constructed")


all_options <- bind_rows(round_options, one_way_options)

all_options %>% group_by(leave, return) %>% filter(min_price == min(min_price)) %>% ggplot(aes(x=return, y= min_price, fill = type)) + geom_col() + facet_wrap(~as.factor(leave))


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

