library(progress)
library(stringr)
library(dplyr)
library(pbapply)

## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

# ================================
# define paths
# ================================
in_path <- "./data/gr/raw/"
out_path <- "./data/gr/"

# ================================
# load data
# ================================
# original data source: https://github.com/prodriguezsosa/Text-Data
corpus <- readRDS(paste0(in_path, "gr_data.rds"))

# keep text only
text <- unlist(corpus, recursive = FALSE)
text <- text[text!=""] # remove empty cells

# clean up
rm(corpus)

# ================================
# PRE-PROCESSING
# ================================
PreProcessText <- function(text_i){
  text_i <- gsub("[^[:alpha:]]", " ", text_i) # remove all non-alpha characters
  text_i <- tolower(text_i)  # lowercase
  text_i <- str_replace_all(text_i, "^ +| +$|( ) +", "\\1")  # remove excess white space
  text_i <- text_i[text_i!=""]
  return(text_i)
}

corpus <- pblapply(text, PreProcessText)
# remove empty texts
corpus <- corpus[!(lapply(corpus, length) == 0)]
# bind
corpus <- unlist(corpus) %>% unname

# ================================
# SAVE
# ================================
saveRDS(corpus, paste0(out_path, "corpus.rds"))

