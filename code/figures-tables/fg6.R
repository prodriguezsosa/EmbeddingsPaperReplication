## load packages
library(dplyr)
library(purrr)
library(ggplot2)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## load data
within_corr_politics <- readRDS('./data/cr/glove/correlations/within_corr_politics.rds')
within_corr_random <- readRDS('./data/cr/glove/correlations/within_corr_random.rds')

## custom function to generate plot data
generate_plot_data <- function(within_corr){
  
  ## get model names
  models <- names(within_corr)
  
  # convert to tibble
  within_corr_dt <- lapply(models, function(x) tibble(model = x, value = within_corr[[x]])) %>% do.call(rbind,.)
  
  # add window and dim values
  within_corr_dt <- within_corr_dt %>% mutate(window = substr(model, 1, 2) %>% gsub("_", "", .) %>% as.integer())
  within_corr_dt <- within_corr_dt %>% mutate(dim = substr(model, 3, 6) %>% gsub("_", "", .) %>% as.integer())
  
  # order
  within_corr_dt <- within_corr_dt %>% arrange(window, dim)
  
  # fix order for visualization
  within_corr_dt <- transform(within_corr_dt, window = factor(window, levels = unique(window)))
  within_corr_dt <- transform(within_corr_dt, dim = factor(dim, levels = unique(dim)))
  within_corr_dt <- transform(within_corr_dt, model = factor(model, levels = unique(model)))
  
  # summary stats
  within_corr_dt <- within_corr_dt %>% group_by(window, dim) %>% summarise(avg = mean(value), se = sd(value))
  
  # output
  return(within_corr_dt)
}


## fg6a: stability plot for random cues
within_corr_dt <- generate_plot_data(within_corr_random)

ggplot(within_corr_dt, aes(x = dim, y = avg, color = window, position_dodge(width=0.5))) + 
  geom_pointrange(aes(ymin = avg - 1.96*se, ymax = avg + 1.96*se), position = position_dodge(width=0.5), size = 0.6) +
  xlab("Embeddings Dimensions") + ylab("Pearson Correlation") +
  coord_cartesian(ylim = c(0.82, 1)) +
  labs(color='Window-Size')  +
  scale_color_manual(values = c("grey80", "grey60", "grey40", "grey20", "black")) +
  theme(plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=20, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20),
        legend.key=element_blank(),
        legend.position = "top")
ggsave(paste0('./figures/fg6a.eps'), dpi = 800)

## fg6b: stability plot for politics cues
within_corr_dt <- generate_plot_data(within_corr_politics)

ggplot(within_corr_dt, aes(x = dim, y = avg, color = window, position_dodge(width=0.5))) + 
  geom_pointrange(aes(ymin = avg - 1.96*se, ymax = avg + 1.96*se), position = position_dodge(width=0.5), size = 0.6) +
  xlab("Embeddings Dimensions") + ylab("Pearson Correlation") +
  coord_cartesian(ylim = c(0.82, 1)) +
  labs(color='Window-Size')  +
  scale_color_manual(values = c("grey80", "grey60", "grey40", "grey20", "black")) +
  theme(plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=20, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=18),
        legend.title=element_text(size=20),
        legend.key=element_blank(),
        legend.position = "top")
ggsave(paste0('./figures/fg6b.eps'), dpi = 800)

