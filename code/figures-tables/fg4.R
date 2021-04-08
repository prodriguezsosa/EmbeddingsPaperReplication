## load packages
library(dplyr)
library(purrr)
library(ggplot2)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## functions
source('./code/functions.R')

## list embedding models
embeddings_file <- as.list(list.files('./data/cr/glove/models/'))
embeddings_file_time <- embeddings_file[grepl(pattern = "time", x = embeddings_file)]  # identify time files
embeddings_file_cost <- embeddings_file[grepl(pattern = "cost", x = embeddings_file)]  # identify cost files

## extract model names
models <- gsub("cost_history_", "", embeddings_file_cost) %>% gsub(".rds", "", .)
models <- gsub("_[[:digit:]]+$", "", models)
models <- unique(models)

## custom function to extract time and cost
extract_computation_data <- function(model){
  
  # subset model files
  cost_files <- embeddings_file_cost[grepl(pattern = model, x = embeddings_file_cost)]
  time_files <- embeddings_file_time[grepl(pattern = model, x = embeddings_file_time)]   
  
  # load files
  cost_data <- paste0('./data/cr/glove/models/', cost_files) %>% map(readRDS) 
  time_data <- lapply(time_files, function(x) readRDS(paste0('./data/cr/glove/models/', x))) %>% lapply(., function(x) x[[1]] %>% as.numeric) %>% unlist
  
  # align units of time data
  time_data_units <- lapply(time_files, function(x) readRDS(paste0('./data/cr/glove/models/', x))) %>% lapply(., function(x) attributes(x[[1]])$units) %>% unlist
  time_data[which(time_data_units == "secs")] <- time_data[which(time_data_units == "secs")]/60 # convert seconds to minutes
  time_data[which(time_data_units == "hours")] <- time_data[which(time_data_units == "hours")]*60 # convert hours to minutes
  
  # minimum cost achieved by each initialization
  min_cost <- lapply(cost_data, min) %>% unlist
  
  # output
  out <- tibble(model = model, # model name
                mean_cost = mean(min_cost), se_cost = sd(min_cost)/sqrt(length(min_cost)), # mean minimum computation cost achieved
                mean_time  = mean(time_data), se_time  = sd(time_data)/sqrt(length(time_data))) # mean computation time
  return(out)
}

## apply custom function to extract time and cost
computation_data <- lapply(models, function(x) extract_computation_data(x)) %>% do.call(rbind, .)

## add window and dim to time data
computation_data$window <- substr(computation_data$model, 1, 2) %>% gsub("_$", "", .) %>% as.character
computation_data$dim <- substr(computation_data$model, 3, 6) %>% gsub("_", "", .) %>% as.character
computation_data <- computation_data[order(as.numeric(computation_data$window), as.numeric(computation_data$dim)),]
computation_data <- transform(computation_data, window = factor(window, levels = unique(window)))
computation_data <- transform(computation_data, dim = factor(dim, levels = unique(dim)))

## plot cost/loss curve
ggplot(computation_data, aes(x = as.numeric(as.character(dim)), y = mean_cost, group = window)) +
  geom_point(aes(shape = window), size = 5) + geom_line(aes(), show.legend = FALSE) +
  xlab("Embedding Dimensions") + ylab("Mean Minimum Loss Achieved") +
  labs(shape='Window-Size')  +
  scale_colour_discrete(name  ="Window-Size",
                        breaks=c(1, 6, 12, 24, 48),
                        labels=c("1", "6", "12", "24", "48")) +
  theme(plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=20, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20),
        legend.key=element_blank(),
        legend.position = "top")
ggsave(paste0('./figures/fg4a.eps'), dpi = 1200)

## plot run time
ggplot(computation_data, aes(x = as.numeric(as.character(dim)), y = mean_time, group = window)) +
  geom_point(aes(shape = window), size = 5) + geom_line(aes(), show.legend = FALSE) +
  xlab("Embedding Dimensions") + ylab("Mean Computation Time \n (minutes)") +
  labs(shape='Window-Size')  +
  scale_colour_discrete(name  ="Window-Size",
                        breaks=c(1, 6, 12, 24, 48),
                        labels=c("1", "6", "12", "24", "48")) +
  theme(plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=20, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20),
        legend.key=element_blank(),
        legend.position = "top")
ggsave(paste0('./figures/fg4b.eps'), dpi = 1200)





