## load packages
library(dplyr)
library(purrr)
library(ggplot2)
library(igraph)
library(reshape2)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## load data
corr_politics <- readRDS('./data/cr/glove/correlations/across_corr_pearson_politics.rds')
corr_random <- readRDS('./data/cr/glove/correlations/across_corr_pearson_random.rds')

## custom function to generate heatmap data
generate_heatmap_data <- function(corr_data){
  
  ## get model names
  models <- c(unique(corr_data$model1), "pretrained")
  models <- models[!grepl(paste("12", "24", "100", "200", sep = "|"), models)] # reduce number of models for visualization purposes
  
  # create edgelist and add diagonal
  el <- rbind(corr_data, cbind(model1 = models, model2 = models, mean = 1, se = NA))
  
  # clean edgelist
  el <- el %>% 
    select(model1, model2, mean) %>% 
    filter((model1 %in% models) & (model2 %in% models)) %>% 
    mutate(mean = as.numeric(mean)) %>% 
    setNames(c("V1", "V2", "weigth"))
  
  # convert to adjancency matrix (this part is to plot heat heatmap in a specified order)
  g = graph.edgelist(as.matrix(el[,1:2]))
  E(g)$weight = el$weigth
  adj = as.matrix(get.adjacency(g, attr='weight'))
  
  # add lower diagonal for easy ordering
  adj[lower.tri(adj, diag = FALSE)] <- t(adj)[lower.tri(adj, diag = FALSE)]
  
  # order
  adjorder <- c("1_50", "1_300", "1_450", "6_50", "6_300", "6_450", "48_50", "48_300", "48_450", "pretrained")
  adj <- adj[adjorder, adjorder]
  
  # keep upper triangle
  adj[lower.tri(adj)] <- NA   
  
  # reshape
  melted_adj<- melt(adj, na.rm = TRUE)
  
  # correct pre-trained model name
  levels(melted_adj$Var1)[levels(melted_adj$Var1)=="pretrained"] <- "pre-trained"
  levels(melted_adj$Var2)[levels(melted_adj$Var2)=="pretrained"] <- "pre-trained"
  
  # output
  return(melted_adj)
}

## fg5a: heatmap for random cues
melted_adj <- generate_heatmap_data(corr_random)
ggplot(data = melted_adj, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "gray100", high = "gray40", mid = 0.65, limit = c(0.2,1), space = "Lab", name="Pearson\nCorrelation") + # 0.2 is around min for random set
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 18, hjust = 1)) +
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = round(value, 2)), color = "black", size = 6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    panel.grid.major = element_blank())
ggsave(paste0('./figures/fg5a.tiff'), dpi = 300)

## fg5b: heatmap for politics cues
melted_adj <- generate_heatmap_data(corr_politics)
ggplot(data = melted_adj, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "gray100", high = "gray40", mid = 0.65, limit = c(0.2,1), space = "Lab", name="Pearson\nCorrelation") + # 0.2 is around min for random set
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 18, hjust = 1)) +
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = round(value, 2)), color = "black", size = 6) +
  theme(
    legend.position="none",
    plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    panel.grid.major = element_blank())
ggsave(paste0('./figures/fg5b.tiff'), dpi = 300)

