## load packages
library(dplyr)
library(purrr)
library(pbapply)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## functions
source('./code/functions.R')

## cues
cues <- c('immigration', 'abortion')

## custom function to find nearest neighbors
compute_cos_sim <- function(model, cues, corpus){
  
  # define path
  file_path <- paste0('./data/', corpus, '/glove/models/')
 
   # load local model
  embeddings_file <- as.list(list.files(file_path))
  model_inits <- embeddings_file[grepl(pattern = paste0("word_vectors_", model), x = embeddings_file)] # identify relevant models from list of model names
  embeds_list <- paste0(file_path, model_inits) %>% map(readRDS) # load models
  
  ## avg cos similarity over local model initializations
  local_cos_sim <- avg_cos_similarity(embeds_list, cues, method = "cosine", norm = 'l2') # compute cosine similarity and average over all initializations

  # output
  return(local_cos_sim)
}

## apply function
cr_1_50  <- compute_cos_sim(model = '1_50', 'immigration', corpus = 'cr')
cr_6_300  <- compute_cos_sim(model = '6_300', 'immigration', corpus = 'cr')
hansard_1_50  <- compute_cos_sim(model = '1_50', 'abortion', corpus = 'ps')
hansard_6_300  <- compute_cos_sim(model = '6_300', 'abortion', corpus = 'ps')

## nearest neighbors local model
nns_cr_1_50 <- cr_1_50['immigration',] %>% .[order(-.)] %>% names(.) %>% .[2:6]
nns_cr_6_300 <- cr_6_300['immigration',] %>% .[order(-.)] %>% names(.) %>% .[2:6]
nns_hansard_1_50 <- hansard_1_50['abortion',] %>% .[order(-.)] %>% names(.) %>% .[2:6]
nns_hansard_6_300 <- hansard_6_300['abortion',] %>% .[order(-.)] %>% names(.) %>% .[2:6]




