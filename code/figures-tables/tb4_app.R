## load packages
library(dplyr)
library(purrr)
library(pbapply)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## functions
source('./code/functions.R')

## cues
cues <- c('democracy', 'freedom', 'equality', 'justice', 'immigration')

## custom function to find nearest neighbors
compute_cos_sim <- function(model, cues){
  
  # load local model
  embeddings_file <- as.list(list.files(paste0('./data/cr/', model, '/models/')))
  model_inits <- embeddings_file[grepl(pattern = "word_vectors_6_300", x = embeddings_file)] # identify relevant models from list of model names
  embeds_list <- paste0('./data/cr/', model, '/models/', model_inits) %>% map(readRDS) # load models
  
  ## avg cos similarity over local model initializations
  local_cos_sim <- avg_cos_similarity(embeds_list, cues, method = "cosine", norm = 'l2') # compute cosine similarity and average over all initializations
  
  # output
  return(local_cos_sim)
}

## apply function
w2v_cos_sim <- compute_cos_sim(model = 'word2vec', cues)
glove_cos_sim  <- compute_cos_sim(model = 'glove', cues)

## find common vocab
common_vocab <- intersect(colnames(w2v_cos_sim), colnames(glove_cos_sim))
w2v_cos_sim <- w2v_cos_sim[,common_vocab]
glove_cos_sim <- glove_cos_sim[,common_vocab]

## nearest neighbors local model
nns_local_glove <- lapply(cues, function(x) glove_cos_sim[x,] %>% .[order(-.)] %>% names(.) %>% .[2:11]) %>% do.call(cbind,.)
nns_local_w2v <- lapply(cues, function(x) w2v_cos_sim[x,] %>% .[order(-.)] %>% names(.) %>% .[2:11]) %>% do.call(cbind,.)
colnames(nns_local_glove) <- cues
colnames(nns_local_w2v) <- cues



