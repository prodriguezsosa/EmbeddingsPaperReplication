library(progress)
library(stringr)
library(dplyr)
library(pbapply)

## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

# ================================
# define paths
# ================================
in_path <- "./data/sp/raw/"
out_path <- "./data/sp/"

# ================================
# load data
# ================================
# original data source: https://github.com/prodriguezsosa/Text-Data
corpus <- readRDS(paste0(in_path, "sp_data.rds"))

# keep text only
text <- unlist(corpus, recursive = FALSE) %>% unlist(recursive = FALSE)
text <- text[names(text) == "text"]

# clean up
rm(corpus)

# ================================
# PRE-PROCESSING
# ================================
# function to remove speakers
remove_speakers <- function(string){
  string <- str_split(string, ":") %>% unlist
  if(length(string)==2){string <- string[2]}else{
    string <- paste(string, collapse = " ")
  }
  return(string)
}

PreProcessText <- function(text_i){
  text_i <- lapply(text_i, remove_speakers) %>% unlist # remove speakers
  text_i <- chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", text_i) # remove accents
  text_i <- str_replace_all(text_i, "^ +| +$|( ) +", "\\1")  # remove excess white space
  text_i <- text_i[!(grepl("^[A-Z][A-Z]", text_i))]  # remove sentences with all capitals
  text_i <- text_i[!(grepl("^Pagina", text_i))]  # remove segments denoting page
  text_i <- str_replace_all(text_i, "NA", " ")
  text_i <- str_replace_all(text_i, "\\[.*?\\]", " ") # remove all text_i in [], these are descriptive (e.g. [pausa])
  text_i <- str_replace_all(text_i, "\\(.*?\\)", " ") # remove all text_i in (), these are descriptive (e.g. [pausa])
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

