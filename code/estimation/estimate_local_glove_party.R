#!/usr/bin/env Rscript
library(text2vec)
library(dplyr)
# for GloVE default parameters see: https://www.rdocumentation.org/packages/text2vec/versions/0.5.1/topics/GlobalVectors
start_time_full <- Sys.time()
# ================================
# arguments (used to facilitate HPC processing)
# ================================
#args <- c(1, 6, 300, 1)
args <- commandArgs(trailingOnly = TRUE)
if(length(args)!=4) stop(paste0("Not the right number of arguments!", args))
args <- as.integer(args)

# ================================
# choice parameters
# ================================
GROUPS <- c("D", "R", "F", "M")
GROUP <- GROUPS[args[1]]
WINDOW_SIZE <- args[2]
DIM <- args[3]
INIT <- args[4]
ITERS <- 100
MIN_COUNT <- 10

# ================================
# define paths
# ================================
in_path <- paste0('./data/cr/')
out_path <- paste0('./data/cr_groups/glove/models/')

# ================================
# load data
# ================================
corpus <- readRDS('./data/cr/corpus.rds')

# subset to group of interest
if(GROUP %in% c("D", "R")) corpus <- corpus %>% filter(party == GROUP)
if(GROUP %in% c("F", "M")) corpus <- corpus %>% filter(gender == GROUP)
text <- corpus$speech
rm(corpus)

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
saveRDS(cost_history, file = paste0(out_path, "cost_history_", GROUP, "_", args[2], "_", args[3], "_", args[4], ".rds"))
saveRDS(word_vectors, file = paste0(out_path, "word_vectors_", GROUP, "_", args[2], "_", args[3], "_", args[4], ".rds"))
saveRDS(comp_time, file = paste0(out_path, "comp_time_", GROUP, "_", args[2], "_", args[3], "_", args[4], ".rds"))

