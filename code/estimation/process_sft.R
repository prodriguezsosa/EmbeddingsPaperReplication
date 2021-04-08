library(dplyr)
library(magrittr)
library(readxl)
library(tidyverse)
library(data.table)
library(pbapply)

## set working directory to the location of the master "word_embeddings" folder
setwd("/Volumes/Potosi/Research/EmbeddingsProject/dataverse/word_embeddings/")

# upload mturk hits
in_path <- "./data/mturk/semantic_fluency_task/output/"
out_path <- "./data/mturk/semantic_fluency_task/processed/"

# define pre-processing options (in addition to lower-casing and removing punctuation)
PLURALS <- FALSE
COLLOCATIONS <- FALSE
STEM <- FALSE

# --------------------------
# LOAD LISTS                ----
# --------------------------
mturk_hits <- as.list(list.files(in_path))
surveys <- mturk_hits[grepl("survey", mturk_hits)] # survey files
fluency <- mturk_hits[grepl("fluency", mturk_hits)] # fluency files

# function to clean HITs
cleanHIT <- function(hit_file){
  hit <- read.table(paste0(in_path, hit_file), sep = ",", header = TRUE)  # load hit
  cues <- as.character(unique(hit$cue))
  if(length(cues) == 11){
    workerid <- as.character(unique(hit$workerid))
    # fluency data
    fluency <- t(hit[hit$variable == "Fluency", -c(1,3)]) # keep fluency rows and cue
    fluency[trimws(fluency) == ""] <- NA  # NA empty entries
    fluency <- data.table(fluency) %>% set_colnames(cues)
    fluency <- gather(fluency) %>% set_colnames(c("cue", "fluency"))
    hit <- data.table(workerid, fluency)
    hit <- hit[!is.na(hit$fluency),]
    return(hit)}else{return(data.table(workerid = NA, cue = NA, fluency = NA))}
}

# function to clean SURVEY FILES
cleanSURVEY <- function(hit_file){
  hit <- read.table(paste0(in_path, hit_file), sep = ",", header = TRUE)  # load hit
  return(hit)
}

# apply function and bind results
hitsFluency <- pblapply(fluency, function(x) cleanHIT(x)) %>% .[!is.na(.)] %>% do.call(rbind, .)
hitsSurvey <- pblapply(surveys, function(x) cleanSURVEY(x)) %>% do.call(rbind,.)

# save results to excel to apply spell check
#openxlsx::write.xlsx(hitsFluency, paste0(out_path,"hitsFluency.xlsx"), col.names = TRUE)

# --------------------------
# PRE-PROCESSING            ----
# --------------------------
hitsFluency <- data.table(read_excel(paste0(out_path, "hitsFLUENCY_spellchecked.xlsx"), sheet = "Sheet1"), stringsAsFactors = FALSE)
names(hitsFluency)[names(hitsFluency) == 'workerid'] <- 'pid'
names(hitsFluency)[names(hitsFluency) == 'workerid'] <- 'pid'
hitsFluency$fluency <- tolower(hitsFluency$fluency)
hitsFluency$fluency <- gsub("'", "", hitsFluency$fluency)  # remove apostrophes
hitsFluency$fluency <- gsub("\\.", "", hitsFluency$fluency)  # remove full stops
hitsFluency$fluency <- gsub("[^[:alnum:]]", " ", hitsFluency$fluency) # remove all non-alpha characters
hitsFluency$fluency <- str_replace_all(hitsFluency$fluency, "^ +| +$|( ) +", "\\1")  # remove excess white space
hitsFluency$fluency <- gsub("^us$", "united states", hitsFluency$fluency) # combine multiple word entries
hitsFluency$fluency <- gsub(" ", "_", hitsFluency$fluency) # combine multiple word entries
hitsFluency <- hitsFluency %>% filter(fluency!="")

if(PLURALS){
  vocab_plurals <- hitsFluency$fluency
  vocab_plurals <- vocab_plurals[!grepl("_", vocab_plurals)]
  vocab_plurals <- vocab_plurals[grepl("s$", vocab_plurals)]
  vocab_plurals <- tibble(pattern = unique(vocab_plurals), replacement = gsub("s$", "", unique(vocab_plurals)))
  vocab_plurals <- vocab_plurals %>% filter(replacement %in% hitsFluency$fluency)
  for(i in 1:nrow(vocab_plurals)){hitsFluency$fluency <- gsub(paste0("\\<", vocab_plurals$pattern[i],"\\>"), vocab_plurals$replacement[i], hitsFluency$fluency)} # slower but safer
}

# collocations
if(COLLOCATIONS){
  vocab_collocs <- hitsFluency$fluency
  vocab_collocs <- vocab_collocs[grepl("_", vocab_collocs)]
  vocab_collocs <- tibble(pattern = unique(vocab_collocs), replacement = gsub("_", "", unique(vocab_collocs)))
  vocab_collocs <- vocab_collocs %>% filter(replacement %in% hitsFluency$fluency)
  for(i in 1:nrow(vocab_collocs)){hitsFluency$fluency <- gsub(paste0("\\<", vocab_collocs$pattern[i],"\\>"), vocab_collocs$replacement[i], hitsFluency$fluency)} # slower but safer
}

if(STEM){
  hitsFluency$fluency <- wordStem(hitsFluency$fluency, language = "en", warnTested = FALSE)
}

# drop repetitions in fluency lists (technically not allowed in SFT)
fluency_list <- hitsFluency %>% group_by(pid, cue) %>% distinct(fluency, .keep_all = TRUE) %>% ungroup() %>% select(pid, cue, fluency) # model will not work w/o deleting duplicates

# --------------------------
# CLEAN SURVEY         ----
# --------------------------
names(hitsSurvey)[names(hitsSurvey) == 'workerid'] <- 'pid'
# recode party
hitsSurvey$party2 <- hitsSurvey$party
hitsSurvey$party2[hitsSurvey$party2 %in% c(8,9)] <- NA
hitsSurvey$party2[hitsSurvey$party2 %in% c(1,2,3)] <- "democrat"
hitsSurvey$party2[hitsSurvey$party2 == 4] <- "independent"
hitsSurvey$party2[hitsSurvey$party2 %in% c(5,6,7)] <- "republican"

# recode ideology
hitsSurvey$ideology2 <- hitsSurvey$ideology
hitsSurvey$ideology2[hitsSurvey$ideology ==8] <- NA
hitsSurvey$ideology2[hitsSurvey$ideology %in% c(1,2,3)] <- "liberal"
hitsSurvey$ideology2[hitsSurvey$ideology == 4] <- "independent"
hitsSurvey$ideology2[hitsSurvey$ideology %in% c(5,6,7)] <- "conservative"

# recode gender
hitsSurvey$gender <- hitsSurvey$sex
hitsSurvey$gender[hitsSurvey$gender == 3] <- NA
hitsSurvey$gender[hitsSurvey$gender ==1] <- "male"
hitsSurvey$gender[hitsSurvey$gender == 2] <- "female"

# --------------------------
# CONVERT TO SS CORPUS      ----
# --------------------------
# keep only relevant survey data
survey <- hitsSurvey %>% select(pid, ideology, ideology2, party2, gender)
survey <- unique(survey)

# add random grouping
set.seed(1984L)
random_pid <- sample(survey$pid, length(survey$pid)/2, replace = FALSE)
survey$gang <- ifelse(survey$pid %in% random_pid, "jets", "sharks")

# tags
tags <- survey
tags$tags <- apply(tags[ , c("ideology2", "party2", "gender", "gang")] , 1 , paste , collapse = " ")
tags <- tags[,c("pid", "tags")]

# ss corpus
sscorpus <- list("fluency" = fluency_list,
                 "tags" = tags,
                 "survey" = survey)

# save data
saveRDS(sscorpus, paste0(out_path, "sscorpus.rds"))
