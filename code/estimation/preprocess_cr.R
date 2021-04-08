## load packages
library(progress)
library(stringr)
library(dplyr)

## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

# ================================
# define paths
# ================================
in_path <- "./data/cr/raw/"
out_path <- "./data/cr/"

# ================================
# list of file names
# ================================
files <- as.list(list.files(in_path))
files_meta <- files[grepl(pattern = "SpeakerMap", x = files)]  # meta data
files_text <- files[grepl(pattern = "speeches", x = files)]  # text

# ================================
# load and pre-process data
# ================================
text_meta <- vector("list", length(files_text))
pb <- progress_bar$new(total = length(files_text))
for(i in 1:length(files_text)){
  # ================================
  # upload text
  # ================================
  text <- read.table(paste(in_path, files_text[[i]], sep =""), 
                     header = FALSE, sep = "|", skip = 1,
                     colClasses = "character", quote = "", 
                     col.names = c("speech_id", "speech"),
                     blank.lines.skip = TRUE, skipNul = TRUE, 
                     strip.white = TRUE, fill = TRUE)
  
  # pre-process
  text$speech <- gsub("[^[:alpha:]]", " ", text$speech) # remove all non-alpha characters
  text$speech <- str_replace_all(text$speech, "^ +| +$|( ) +", "\\1")  # remove excess white space
  text$speech <- tolower(text$speech)  # lowercase
  text <- text[text$speech!="",] # remove nuls
  
  # ================================
  # upload meta data
  # ================================
  meta <- read.table(paste(in_path, files_meta[[i]], sep =""), 
                     header = FALSE, sep = "|", skip = 1, 
                     colClasses = "character", quote = "", 
                     col.names = c("speakerid", "speech_id", "lastname", "firstname", "chamber", "state", 
                                   "gender", "party", "district", "nonvoting"), 
                     blank.lines.skip = TRUE, skipNul = TRUE, 
                     strip.white = TRUE, fill = TRUE)
  
  # add session id
  meta$session_id <- unlist(str_split(files_meta[[i]], pattern = "_"))[1]
  
  # ================================
  # merge text and meta
  # ================================
  text_meta[[i]] <- left_join(text, meta, by = "speech_id")  # keeps all text
  pb$tick()
}

# bind
corpus <- do.call(rbind, text_meta)

# keep only text with meta-data
corpus <- corpus[!is.na(corpus$party),]

# save
saveRDS(corpus, paste0(out_path, "corpus.rds"))
