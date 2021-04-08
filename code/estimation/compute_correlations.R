# ================================
# load libraries
# ================================
library(text2vec)
library(stringr)
library(pcaPP)
library(dplyr)

## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

## functions
source('./code/functions.R')

# ================================
# arguments (used to facilitate HPC processing)
# ================================
corpus <- 'cr' # corpora include: cr, ps, sotu, sp, gr
type <- 'politics' # cue type is either 'politics' or 'random'

# ================================
# define paths
# ================================
in_path_glove <- './data/pre-trained/'
in_path_embeddings <- paste0('./data/', corpus, '/glove/models/')
out_path <- paste0('./data/', corpus, '/glove/correlations/')

# ================================
# load pretrained
# ================================
if(corpus %in% c('cr', 'ps', 'sotu')) pretrained <- readRDS(paste0(in_path_glove, "glove.rds"))
if(corpus %in% c('sp')) pretrained <- readRDS(paste0(in_path_glove, "glove_spanish.rds"))
if(corpus %in% c('gr')) print('there were no readily available pretrained GloVe embeddings in German')

# ================================
# local embeddings file list
# ================================
embeddings_file <- as.list(list.files(in_path_embeddings))
embeddings_file <- embeddings_file[grepl(pattern = "word", x = embeddings_file)]
embeddings_model_names <- gsub("word_vectors_", "", embeddings_file) %>% gsub(".rds", "", .)
embeddings_model_names <- gsub("_[[:digit:]]+$", "", embeddings_model_names)
names(embeddings_file) <- embeddings_model_names
embeddings_model_names <- unique(embeddings_model_names)

# ================================
# cues
# ================================
# load one embedding model to get vocab
vocab_local <- readRDS(paste0(in_path_embeddings, embeddings_file[[1]])) %>% rownames(.)
vocab_pretrained <- rownames(pretrained)
vocab <- intersect(vocab_local, vocab_pretrained)
set.seed(42L)
cues_list <- list()
cues_list[["random"]] <- sample(vocab, 100, replace = FALSE)
cues_list[["politics"]] <- c("democracy", "freedom", "equality", "justice", "immigration", "abortion", "welfare", "taxes", "republican", "democrat")

# select cues type
cues <- cues_list[[type]]

# ================================
# within correlations (local-models)
# ================================
# initialize list to fill with results
within_corr <- vector("list", length(embeddings_model_names)) 
names(within_corr) <- embeddings_model_names
for(i in embeddings_model_names){
  # local model names
  model_names <- embeddings_file[which(names(embeddings_file) == i)] %>% unlist() %>% unname()
  # load all local models
  local_embeddings <- lapply(model_names, function(x) readRDS(paste0(in_path_embeddings, x)))
  names(local_embeddings) <- model_names
  # get all possible model pairs
  model_pairs <- expand.grid(model_names, model_names, stringsAsFactors = FALSE) %>% setNames(c("model1", "model2"))
  # rm equivalent models
  model_pairs <- model_pairs %>% filter(!(model1 == model2))
  # rm duplicate pairs
  model_pairs <- unique(t(apply(model_pairs, 1, sort)))
  # compute correlations
  corr_list <- lapply(1:nrow(model_pairs), function(x){
    model1 <- local_embeddings[[model_pairs[x, 1]]]
    model2 <- local_embeddings[[model_pairs[x, 2]]]
    # compute correlations for all cues
    mean_corr <- lapply(cues, function(y) corr_embeds(model1, model2, cue = y, type = "pearson", norm = "l2")) %>% unlist() %>% mean()
    return(mean_corr)
  })
  within_corr[[i]] <- unlist(corr_list)
  cat('done with model', i, '\n')
}

# save
saveRDS(within_corr, file = paste0(out_path, "within_corr_", type, ".rds"))

# ================================
# avg cos similarity over initializations
# ================================
# initialize list to fill with results
avg_sim_vectors <- vector("list", length(embeddings_model_names)) 
names(avg_sim_vectors) <- embeddings_model_names
for(i in embeddings_model_names){
  model_inits <- embeddings_file[grepl(pattern = paste0("word_vectors_", i), x = embeddings_file)]
  # load all initializations
  embeds_list <- lapply(model_inits, function(x) readRDS(paste0(in_path_embeddings, x)))
  avg_sim_vectors[[i]] <- avg_cos_similarity(embeds_list, cues, method = "cosine", norm = 'l2') # compute cosine similarity and average over all initializations
  cat('done with model', i, '\n')
}

# add pretrained similarity vectors
if(corpus!='gr'){ # no pretrained GloVe embeddings in german at time of writing
pretrained_sim <- lapply(cues, function(y) cue_sim(y, pretrained, norm = "l2", rank = FALSE)) %>% do.call(rbind,.)
rownames(pretrained_sim) <- cues
avg_sim_vectors[["pretrained"]] <- pretrained_sim
}

# ================================
# across correlations - pearson
# ================================
# get all possible model pairs
model_pairs <- expand.grid(c(embeddings_model_names, "pretrained"), c(embeddings_model_names, "pretrained"), stringsAsFactors = FALSE) %>% setNames(c("model1", "model2"))
# rm equivalent models
model_pairs <- model_pairs %>% filter(!(model1 == model2))
# rm duplicate pairs
model_pairs <- unique(t(apply(model_pairs, 1, sort)))

# compute correlations
across_correlations <- lapply(1:nrow(model_pairs), function(x) corr_sims(sims1 = avg_sim_vectors[[model_pairs[x,1]]], sims2 = avg_sim_vectors[[model_pairs[x,2]]], type = "pearson"))
# bind
across_correlations <- do.call(rbind, across_correlations)
# add model names
across_corr_pearson <- tibble(model1 = model_pairs[, 1],
                              model2 = model_pairs[, 2], 
                              mean = across_correlations$mean, 
                              se = across_correlations$se)

# save
saveRDS(across_corr_pearson, file = paste0(out_path, "across_corr_pearson_", type, ".rds"))

# ================================
# across correlations - rank (local)
# ================================
# compute correlations
across_correlations <- lapply(1:nrow(model_pairs), function(x) corr_sims(sims1 = avg_sim_vectors[[model_pairs[x,1]]], sims2 = avg_sim_vectors[[model_pairs[x,2]]], type = "rank"))
# bind
across_correlations <- do.call(rbind, across_correlations)
# add model names
across_corr_rank <- tibble(model1 = model_pairs[, 1],
                           model2 = model_pairs[, 2], 
                           mean = across_correlations$mean, 
                           se = across_correlations$se)

# save
saveRDS(across_corr_rank, file = paste0(out_path, "across_corr_rank_", type, ".rds"))

# ================================
# jaccard index
# ================================
N_set <- c(5, 10, 15, 20, 50, 100)
across_jaccard_N <- vector("list", length(N_set))
names(across_jaccard_N) <- as.character(N_set)
# compute jaccard index
for(i in N_set){
  across_jaccard <- lapply(1:nrow(model_pairs), function(x) jaccard_sims(sims1 = avg_sim_vectors[[model_pairs[x,1]]], sims2 = avg_sim_vectors[[model_pairs[x,2]]], cue, N = i, common_vocab = FALSE))
  # bind
  across_jaccard <- do.call(rbind, across_jaccard)
  # add model names
  across_jaccard_N[[as.character(i)]] <- tibble(model1 = model_pairs[, 1],
                                                model2 = model_pairs[, 2], 
                                                mean = across_jaccard$mean, 
                                                se = across_jaccard$se)
}

# save 
saveRDS(across_jaccard_N, file = paste0(out_path, "across_jaccard_", type, ".rds"))







