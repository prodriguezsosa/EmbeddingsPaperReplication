## load packages
library(dplyr)
library(purrr)
library(pbapply)
library(ggplot2)
library(text2vec)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## functions
source('./code/functions.R')

## load semantic fluency task corpus
sscorpus <- readRDS('./data/mturk/semantic_fluency_task/processed/sscorpus.rds')

## list of corpora and corresponding figure numbers
corpora <- c('ps', 'sotu')
fig_numbers <- c('a', 'b')

## loop through all corpora
for(i in 1:length(corpora)){
  
  ## define cues
  if(corpora[i] == 'ps'){
    cues <- c("democracy", "freedom", "equality", "justice", "immigration", "abortion", "welfare", "taxes", "republican", "democrat")
  }else{ # abortion not in the sotu corpus
    cues <- c("democracy", "freedom", "equality", "justice", "immigration", "welfare", "taxes", "republican", "government", "congress")
  }
  
  ## list embedding models
  embeddings_file <- as.list(list.files(paste0('./data/', corpora[i], '/glove/models/'))) # for each parameter combination we estimate 10 models
  embeddings_file <- embeddings_file[grepl(pattern = "word", x = embeddings_file)] # identify model files (250 in total)
  
  ## extract model names
  models <- gsub("word_vectors_", "", embeddings_file) %>% gsub(".rds", "", .)
  models <- gsub("_[[:digit:]]+$", "", models)
  names(embeddings_file) <- models
  models <- unique(models)
  
  ## avg cos similarity over initializations
  avg_sim_vectors <- pblapply(models, function(x){
    model_inits <- embeddings_file[grepl(pattern = paste0("word_vectors_", x), x = embeddings_file)] # identify relevant models from list of model names
    embeds_list <- paste0('./data/', corpora[i], '/glove/models/', model_inits) %>% map(readRDS) # load models
    out <- avg_cos_similarity(embeds_list, cues, method = "cosine", norm = 'l2') # compute cosine similarity and average over all initializations
    return(out)
  })
  names(avg_sim_vectors) <- models
  
  ## convert to rank vectors
  avg_rank_vectors <- lapply(avg_sim_vectors, function(x) apply((-1)*x, 1, rank, ties.method = "random") %>% t(.)) # mult by -1 b/c the base rank fcn. works in increasing order
  names(avg_rank_vectors) <- models
  
  # compute rank deviation for every model averaging over cues
  cues <- intersect(cues, unique(sscorpus$fluency$cue)) # keep only cues for which we have human data (all except 1 `congress` in the case of SOTU)
  model_rank_dev <- vector('list', length(models)) %>% setNames(models)
  for(model in models){
    cue_rank_dev <- vector('list', length(cues)) %>% setNames(cues)
    for(cue_i in cues){
      cue_rank_dev[[cue_i]] <- compute_rank_deviation(cue_i, model, avg_rank_vectors, sscorpus)
    }
    cue_rank_dev <- do.call(rbind, cue_rank_dev) %>% summarise(model = unique(model), std.error = sd(mu)/sqrt(n()), mu = mean(mu))
    model_rank_dev[[model]] <- cue_rank_dev
  }
  
  # bind output
  plot_tibble <- do.call(rbind, model_rank_dev)
  
  # get plotting variables from model names
  plot_tibble$window <- substr(plot_tibble$model, 1, 2) %>% gsub("_$", "", .) %>% as.character()
  plot_tibble$dim <- substr(plot_tibble$model, 3, 6) %>% gsub("_", "", .) %>% as.character()
  plot_tibble <- plot_tibble[order(as.numeric(plot_tibble$window), as.numeric(plot_tibble$dim)),]
  plot_tibble <- transform(plot_tibble, window = factor(window, levels = unique(window)))
  plot_tibble <- transform(plot_tibble, dim = factor(dim, levels = unique(dim)))
  plot_tibble <- transform(plot_tibble, model = factor(model, levels = unique(model)))
  
  # plot
  ggplot(plot_tibble, aes(x = window, y = mu)) + 
    geom_pointrange(aes(ymin = mu - 1.96*std.error, ymax = mu + 1.96*std.error, shape = dim), position = position_dodge(width=0.5), size = 0.75) +
    xlab("Window-Size") + ylab("Mean Log Rank Deviations from Human List") +
    labs(shape='Embedding Dimensions')  +
    scale_colour_discrete(name  ="Embedding Dimensions",
                          breaks=c(50, 100, 200, 300, 450),
                          labels=c("50", "100", "200", "300", "450")) +
    theme(plot.title = element_text(size=20, margin = margin(t = 20, r = 0, b = 0, l = 0)), panel.background = element_blank(),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18),
          axis.title.y = element_text(size=20, margin = margin(t = 0, r = 15, b = 0, l = 15)),
          axis.title.x = element_text(size=20, margin = margin(t = 15, r = 0, b = 15, l = 0)),
          legend.text=element_text(size=18),
          legend.title=element_text(size=20),
          legend.key=element_blank(),
          legend.position = "top")
  ggsave(paste0('./figures/fg22', fig_numbers[i], '_app.eps'), dpi = 1200)
  
  # verbose
  cat('done with corpus', corpora[i], '\n')
}

