## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

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
WINDOW_SIZE <- args[1]  # how many words to consider left and right
DIM <- args[2]  # dimension of the embedding vector
INIT <- args[3]
NEGATIVE_SAMPLES <- 1  # number of negative examples to sample for each word
EPOCHS <- 5L
MIN_COUNT <- 10L
WORKERS <- as.integer(RcppParallel::defaultNumThreads())
print(WORKERS)

# ================================
# define paths
# ================================
in_path <- paste0('./data/cr/')
out_path <- paste0('./data/cr/word2vec/models/')

# ================================
# load data
# ================================
corpus <- readRDS(paste0(in_path, "corpus.rds"))
text <- corpus$speech
text <- unique(text) # remove duplicates
rm(corpus)

# prepare for gensim model
library(stringr)
text <- (str_split(text, " "))

# ================================
# load W2V model
# ================================
library(reticulate)
gensim <- import("gensim") # import the gensim library
Word2Vec <- gensim$models$Word2Vec # Extract the Word2Vec model
multiprocessing <- import("multiprocessing") # For parallel processing

start_time_est <- Sys.time()
basemodel = Word2Vec(text, 
                     workers = WORKERS,
                     size = DIM, 
                     window = WINDOW_SIZE, 
                     sg = 1L,
                     #max_final_vocab = 10000L,
                     min_count = MIN_COUNT,
                     #max_vocab_size = 10000L,
                     iter = EPOCHS,
                     seed = INIT,
                     compute_loss = TRUE)
comp_time_est <- Sys.time() - start_time_est

# ================================
# get output
# ================================
library(Matrix)
embeds <- basemodel$wv$syn0
rownames(embeds) <- basemodel$wv$index2word

# computation time
loss <- basemodel$get_latest_training_loss()
comp_time_full <- Sys.time() - start_time_full
comp_time <- list(comp_time_est, comp_time_full)
training_time <- basemodel$total_train_time

# ================================
# save
# ================================
saveRDS(loss, file = paste0(out_path, "loss_", args[1], "_", args[2], "_", args[3], ".rds"))
saveRDS(training_time, file = paste0(out_path, "training_time_", args[1], "_", args[2], "_", args[3], ".rds"))
saveRDS(embeds, file = paste0(out_path, "word_vectors_", args[1], "_", args[2], "_", args[3], ".rds"))
saveRDS(comp_time, file = paste0(out_path, "comp_time_", args[1], "_", args[2], "_", args[3], ".rds"))


