library(stringr)
library(dplyr)
library(text2vec)
library(quanteda.corpora)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## custom functo to create summary
corpusSummary <- function(texts){
  # compute types and vocab size
  tokens <- space_tokenizer(texts)
  it <- itoken(tokens, progressbar = TRUE)
  vocab <- create_vocabulary(it)
  vocab <- prune_vocabulary(vocab, term_count_min = 10)
  # number of documents
  num_docs <- length(texts)
  # number of tokens
  num_tokens <- sum((str_count(texts, " ") + 1))
  mean_tokens <- round(num_tokens/num_docs)
  # number of types
  num_types <- nrow(vocab)
  # lexical diversity (type/token ratio)
  lexical_div <- num_types/num_tokens
  return(tibble(num_docs = num_docs, num_tokens = num_tokens, mean_tokens = mean_tokens, num_types, lexical_div))
}

# intialize vector
summary_list <- vector("list", 5)
names(summary_list) <- c("cr", "ps", "sotu", "sp", "gr")

# CR
corpus <- readRDS("./data/cr/corpus.rds")
texts <- corpus$speech
summary_list[["cr"]] <- corpusSummary(texts)

# PS
texts <- readRDS("./data/ps/corpus.rds")
summary_list[["ps"]] <- corpusSummary(texts)

# SOTU
corpus <- data_corpus_sotu
texts <- corpus$documents$texts
summary_list[["sotu"]] <- corpusSummary(texts)

# SP
texts <- readRDS("./data/sp/corpus.rds")
summary_list[["sp"]] <- corpusSummary(texts)

# GR
texts <- readRDS("./data/gr/corpus.rds")
summary_list[["gr"]] <- corpusSummary(texts)

# bind results
summary_tibble <- do.call(rbind, summary_list)



