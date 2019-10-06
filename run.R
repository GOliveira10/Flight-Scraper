source("Skiplagged Scraper.R")


driver <- rsDriver(port = 4444L, browser = c("chrome"), chromever="76.0.3809.126")

remDr <- driver[["client"]]




options <- explore_prices("LAX", "SEA", duration = 14, interval = 1, window = 2)


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

