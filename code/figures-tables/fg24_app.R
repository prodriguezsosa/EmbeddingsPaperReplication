## load packages
library(dplyr)
library(purrr)
library(ggplot2)
library(pbapply)
library(text2vec)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## functions
source('./code/functions.R')

## load pretrained models
pretrained <- list('word2vec' = readRDS("./data/pre-trained/word2vec.rds"), 'glove' = readRDS("./data/pre-trained/glove.rds"))

## parameters
algorithms <- c('glove', 'word2vec')
cues <- c("democracy", "freedom", "equality", "justice", "immigration", "abortion", "welfare", "taxes", "republican", "democrat")
models <- c('1_300', '6_300', '12_300', '24_300', '48_300')

## custom function to compute similarity vectors
compute_sim_vectors <- function(algorithm, models, cues){
  
  # list embedding models
  embeddings_file <- as.list(list.files(paste0('./data/cr/', algorithm,'/models/')))
  embeddings_file <- embeddings_file[grepl(pattern = "word", x = embeddings_file)] # identify model files (250 in total)
  
  # avg cos similarity over initializations
  avg_sim_vectors <- pblapply(models, function(x){
    model_inits <- embeddings_file[grepl(pattern = paste0("word_vectors_", x), x = embeddings_file)] # identify relevant models from list of model names
    embeds_list <- paste0('./data/cr/', algorithm,'/models/', model_inits) %>% map(readRDS) # load models
    out <- avg_cos_similarity(embeds_list, cues, method = "cosine", norm = 'l2') # compute cosine similarity and average over all initializations
    return(out)
  })
  names(avg_sim_vectors) <- models
  
  # add pretrained similarity vectors
  pretrained_sim <- pblapply(cues, function(y) sim2(x = pretrained[[algorithm]], y = pretrained[[algorithm]][y, , drop = FALSE], method = "cosine", norm = 'l2') %>% .[,1]) %>% do.call(rbind,.)
  rownames(pretrained_sim) <- cues
  avg_sim_vectors[["pretrained"]] <- pretrained_sim
  
  # output
  return(avg_sim_vectors)
}

## apply function
sim_vectors <- lapply(algorithms, function(x) compute_sim_vectors(algorithm = x, models = models, cues = cues))
names(sim_vectors) <- algorithms

## compute correlations across algorithms
models <- c(models, "pretrained")
across_correlations <- pblapply(1:length(models), function(x) corr_sims(sims1 = sim_vectors$word2vec[[models[x]]], sims2 = sim_vectors$glove[[models[x]]], type = "pearson"))

## bind
across_correlations <- do.call(rbind, across_correlations)

## add model names
across_corr_pearson <- tibble(model1 = models,
                              model2 = models, 
                              mean = across_correlations$mean, 
                              se = across_correlations$se)

## plot
across_corr_pearson <- transform(across_corr_pearson, model1 = factor(model1, levels = c("1_300", "6_300", "12_300", "24_300", "48_300", "pretrained")))
levels(across_corr_pearson$model1)[levels(across_corr_pearson$model1)=="pretrained"] <- "pre-trained"
ggplot(across_corr_pearson, aes(x = model1, y = mean)) +
  geom_bar(position = position_dodge(), stat = "identity", fill = "gray70") + 
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, position = position_dodge(0.9), size = 0.75) +
  xlab("") + ylab("Avg. Pearson Correlation") +
  theme(plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
        axis.text.x = element_text(size=18, angle = 90, margin = margin(t = -10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size=18),
        axis.ticks.x=element_blank(),
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=20, margin = margin(t = 15, r = 0, b = 15, l = 0)))
ggsave(paste0('./figures/fg24_app.eps'), dpi = 800)



