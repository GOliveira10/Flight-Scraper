library(igraph)

network <- options

network <- network %>% filter(type == "depart_options")

edges <-  network$route

extract_edges <- function(x){

  split_edges <- str_split(x, "-")
  
  all_possible_trips <- function(x){
    all_edges <- list()
      for(i in 1:(length(x) - 1)){
    

        edge <- paste0(x[i], "-", x[i+1])
    
        all_edges[[i]] <- edge
    
      }
  
    return(all_edges)
  }
  
  lapply(split_edges, all_possible_trips) %>% unlist()
  
}

edges <- extract_edges(edges)
edges <- edges %>% str_split("-", simplify = TRUE) %>% as_tibble()

names(edges) <- c("from", "to") 
g <- graph_from_data_frame(d=edges)

g <- simplify(g, remove.multiple = F, remove.loops = T) 

plot(g)





