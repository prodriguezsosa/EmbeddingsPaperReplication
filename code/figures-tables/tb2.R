## load packages
library(dplyr)
library(purrr)
library(text2vec)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## functions
source('./code/functions.R')

## define cues
cues <- c("abortion", "democrat", "justice")

## load pre-trained glove model
glove <- readRDS('./data/pre-trained/glove.rds')

## load local model
embeddings_file <- as.list(list.files('./data/cr/glove/models/'))
model_inits <- embeddings_file[grepl(pattern = "word_vectors_6_300", x = embeddings_file)] # identify relevant models from list of model names
embeds_list <- paste0('./data/cr/glove/models/', model_inits) %>% map(readRDS) # load models

## avg cos similarity over local model initializations
local_cos_sim <- avg_cos_similarity(embeds_list, cues, method = "cosine", norm = 'l2') # compute cosine similarity and average over all initializations

## nearest neighbors pre-trained glove model (note 1st nearest neighbor is always the target word)
nns_glove <- lapply(cues, function(x) nearest_neighbors(x, embeds = glove, N = 10, norm = 'l2', drop_cue = TRUE)) 
nns_glove <- do.call(cbind, nns_glove)
colnames(nns_glove) <- cues

## nearest neighbors local model
nns_local <- lapply(cues, function(x) local_cos_sim[x,] %>% .[order(-.)] %>% names(.) %>% .[2:11]) # drop top nearest neighbor which is just the cue itself
nns_local <- do.call(cbind, nns_local)
colnames(nns_local) <- cues

