## load packages
library(text2vec)
library(dplyr)
library(pbapply)
library(progress)

## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

## functions
source('./code/functions.R')

# ================================
# define paths
# ================================
in_path_pretrained <- "./data/pre-trained/"
in_path_embeddings <- "./data/cr/glove/models/"
in_path_human <- "./data/mturk/semantic_fluency_task/processed/"
out_path <- "./data/mturk/triad_task/input_data/"

# ================================
# define cues
# ================================
cues <- c("democracy", "freedom", "equality", "justice", "immigration", "abortion", "welfare", "taxes", "republican", "democrat")

# ================================
# avg cos sim over initializations local embeddings
# ================================
local_models <- c("6_300", "48_300")
embeddings_file <- as.list(list.files(in_path_embeddings))
local_sims <- vector("list", length(local_models)) %>% setNames(local_models)

for(i in local_models){
  model_inits <- embeddings_file[grepl(pattern = paste0("word_vectors_", i), x = embeddings_file)]
  
  # load all initializations
  embeds_list <- lapply(model_inits, function(x) readRDS(paste0(in_path_embeddings, x)))
  
  # average cosine similarity vector over all initializations
  local_sims[[i]] <- avg_cos_similarity(embeds_list, cues, norm = "l2") # package fcn
}

# ================================
# find nearest neighbors
# ================================
models <- c("glove", "w2v", "6_300", "48_300", "human")
nn_list <- vector("list", length(models)) %>% setNames(models)

# local models
nn_list[["6_300"]] <- pblapply(cues, function(x) nn_sims(x, sims = local_sims[["6_300"]], N = 10, drop_cue = TRUE)) %>% setNames(cues)  # applying a package fcn
nn_list[["48_300"]] <- pblapply(cues, function(x) nn_sims(x, sims = local_sims[["48_300"]], N = 10, drop_cue = TRUE)) %>% setNames(cues)  # applying a package fcn

# pre-trained models
glove <- readRDS(paste0(in_path_pretrained, "glove.rds"))
word2vec <- readRDS(paste0(in_path_pretrained, "word2vec.rds"))
common_vocab <- intersect(rownames(glove), rownames(word2vec)) # subset to common vocab
nn_list[["glove"]] <- pblapply(cues, function(x) nearest_neighbors(x, glove[common_vocab,], N = 10, norm = "l2", drop_cue = TRUE)) %>% setNames(cues)
nn_list[["w2v"]] <- pblapply(cues, function(x) nearest_neighbors(x, word2vec[common_vocab,], N = 10, norm = "l2", drop_cue = TRUE)) %>% setNames(cues)
rm(glove)
rm(word2vec)

# human
sscorpus <- readRDS(paste0(in_path_human, "sscorpus.rds"))
nn_list[["human"]] <- pblapply(cues, function(x){
  nn <- sscorpus$fluency %>% filter(cue == x) %>% group_by(fluency) %>% tally() %>% arrange(-n) %>% slice(2:11)
  return(nn$fluency)
})

names(nn_list[["human"]]) <- cues

# ================================
# trial & screening data
# ================================
trial_data <- list(#c("food", "nutrition", "ship", TRUE, FALSE))#,
c("forest", "juice", "woodland", FALSE, TRUE))
#c("sick", "doctor", "brick", TRUE, FALSE))

# screening data
screening_data <- list(#c("church", "kitten", "temple", FALSE, TRUE),
                       c("gradual", "slow", "quick", TRUE, FALSE),
#c("laugh", "elect", "giggle", FALSE, TRUE),
c("soccer", "goal", "mousetrap", TRUE, FALSE))#,
#c("brochure", "water", "booklet", FALSE, TRUE))
#c("dial", "ring", "drive", TRUE, FALSE),
#c("break", "fracture", "mail", TRUE, FALSE))
#c("party", "wooden", "celebration", FALSE, TRUE))
#c("sick", "doctor", "brick", TRUE, FALSE),
#c("anatomy", "automobile", "body", FALSE, TRUE))

# ================================
# save
# ================================
saveRDS(nn_list, paste0(out_path, "nn_list.rds"))
saveRDS(trial_data, paste0(out_path, "trial_data.rds"))
saveRDS(screening_data, paste0(out_path, "screening_data.rds"))


