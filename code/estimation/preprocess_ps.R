rm(list = ls())
library(readtext)
library(data.table)
library(stringr)
library(progress)
library(pbapply)
library(magrittr)
library(jsonlite)
library(xlsx)

## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

# ================================
# define paths
# ================================
in_path <- "./data/ps/raw/"
out_path <- "./data/ps/"

# ================================
# list of files
# ================================
file_list <- readRDS(paste0(in_path, "file_list.rds")) # list of files to retrieve
file_list$url <- gsub("=0", "=1", file_list$url) # change url such that dropbox allows download
file_list$year <- substr(file_list$file_name, 11, 14) %>% as.integer # extract year (in case we want to subset)

# ================================
# process data
# ================================
# function to proces urls linking to data
processPS <- function(url){
  #url <- "https://www.dropbox.com/sh/f59nv6f8nsscyih/AAAe5__tG137zqVYm-oAsTPIa/json/uk-topic-1950-10.json?dl=1"
  out <- lapply(readLines(url), fromJSON)
  text <- lapply(out, function(x) x[["text"]]) %>% do.call(rbind, .) %>% as.vector
  text <- gsub("\\\n", " ", text) # spacing matter
  text <- gsub("[^[:alpha:]]", " ", text) # remove all non-alpha characters
  text <- str_replace_all(text, "^ +| +$|( ) +", "\\1")  # remove excess white space
  text <- tolower(text)  # lowercase
  text <- text[text!=""] # remove nuls
}

# apply function
corpus <- pblapply(file_list$url, processPS)
corpus <- unlist(corpus)
# ================================
# save
# ================================
saveRDS(corpus, paste0(out_path, "corpus.rds"))
