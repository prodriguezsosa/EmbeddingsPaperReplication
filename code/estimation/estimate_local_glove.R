#!/usr/bin/env Rscript
library(text2vec)

## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

# for GloVE default parameters see: https://www.rdocumentation.org/packages/text2vec/versions/0.5.1/topics/GlobalVectors
start_time_full <- Sys.time()
# ================================
# arguments (used to facilitate HPC processing)
# ================================
#args <- c(6, 300, 1)
args <- commandArgs(trailingOnly = TRUE)
if(length(args)!=3) stop(paste0("Not the right number of arguments!", args))
args <- as.integer(args)

# ================================
# choice parameters
# ================================
WINDOW_SIZE <- args[1]
DIM <- args[2]
INIT <- args[3]
ITERS <- 100
MIN_COUNT <- 10
corpus <- 'cr' # corpora include: cr, ps, sotu, sp, gr

# ================================
# define paths
# ================================
in_path <- paste0('./data/', corpus, '/')
out_path <- paste0('./data/', corpus, '/glove/models/')

# ================================
# load data
# ================================
text <- readRDS(paste0(in_path, "corpus.rds"))
if(corpus == "cr") text <- text$speech

# shuffle text
set.seed(42L)
text <- sample(text)

# ================================
# create vocab
# ================================
tokens <- space_tokenizer(text)
rm(text)
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = MIN_COUNT)  # keep only words that meet count threshold

# ================================
# create term co-occurrence matrix
# ================================
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric")

# ================================
# set model parameters
# ================================
glove <- GlobalVectors$new(word_vectors_size = DIM, 
                         vocabulary = vocab, 
                         x_max = 100,
                         lambda = 1e-5)

# ================================
# fit model
# ================================
start_time_est <- Sys.time()
word_vectors_main <- glove$fit_transform(tcm, 
                                         n_iter = ITERS,
                                         convergence_tol = 1e-3, 
                                         n_check_convergence = 1L,
                                         n_threads = RcppParallel::defaultNumThreads())
comp_time_est <- Sys.time() - start_time_est

# ================================
# get output
# ================================
word_vectors_context <- glove$components
word_vectors <- word_vectors_main + t(word_vectors_context) # word vectors
cost_history <- glove$get_history() %>% .[["cost_history"]]  # cost history
comp_time_full <- Sys.time() - start_time_full
comp_time <- list(comp_time_est, comp_time_full)

# ================================
# save
# ================================
saveRDS(cost_history, file = paste0(out_path, "cost_history_", args[1], "_", args[2], "_", args[3], ".rds"))
saveRDS(word_vectors, file = paste0(out_path, "word_vectors_", args[1], "_", args[2], "_", args[3], ".rds"))
saveRDS(comp_time, file = paste0(out_path, "comp_time_", args[1], "_", args[2], "_", args[3], ".rds"))
