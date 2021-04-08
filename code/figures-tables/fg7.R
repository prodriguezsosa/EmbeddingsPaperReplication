## load packages
library(text2vec)
library(stringr)
library(progress)
library(dplyr)
library(ggplot2)

## set working directory to the location of the master "word_embeddings" folder
setwd("")

## functions
source('./code/functions.R')

# define cues of interest
cues <- c("abortion", "taxes")

# find all pairs of model
# we have 10 initializations of the 6-300 model for each party
# so 100 pairs in total
model_pairs <- expand.grid(1:10,1:10)

# initialize empty list vector
nns_differences_list <- vector('list', 10) %>% setNames(cues)

for(cue in cues){
  
  # initialize differences list vector to fill with each pair comparison
  nns_differences <- vector('list', nrow(model_pairs))
  pb <- progress_bar$new(total = nrow(model_pairs))
  for(i in 1:nrow(model_pairs)){
    
    # laod word vectors
    wv_rep <- readRDS(paste0('./data/cr_groups/glove/models/', "word_vectors_R_6_300_", model_pairs$Var1[i], ".rds"))
    wv_dem <- readRDS(paste0('./data/cr_groups/glove/models/', "word_vectors_D_6_300_", model_pairs$Var2[i], ".rds"))
    
    # nearest neighbors
    dem_nn <- nearest_neighbors(cue, embeds = wv_dem, N = 11, norm = "l2") # first nearest neighbor is always the cue
    rep_nn <- nearest_neighbors(cue, embeds = wv_rep, N = 11, norm = "l2") # first nearest neighbor is always the cue
    
    # set differences
    dem_unique = setdiff(dem_nn, rep_nn)
    rep_unique = setdiff(rep_nn, dem_nn)
    
    # return output
    nns_differences[[i]] <- rbind(tibble(party = 'dem', token = dem_unique),
                                  tibble(party = 'rep', token = rep_unique))
    
    # progress
    pb$tick()
  }
  
  # --------------------------------
  # table
  # --------------------------------
  nns_differences <- do.call(rbind, nns_differences)
  nns_differences <- nns_differences %>% summarise(token_count = n()) %>% arrange(party, -token_count) %>% mutate(cue = cue)
  
  # store
  nns_differences_list[[cue]] <- nns_differences
  cat('done with cue:', cue)
  
}

# bind results
nns_differences_tibble <- do.call(rbind, nns_differences_list) %>% 
  group_by(cue) %>% 
  arrange(-token_count) %>% 
  distinct(token, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(token_count_neg = if_else(party == 'dem', token_count, as.integer(token_count*(-1)))) %>%
  arrange(cue, -token_count_neg)

# plot
plots <- vector('list', length(cues)) %>% setNames(cues)
for(cue in cues){
  sub_tibble <- nns_differences_tibble[nns_differences_tibble$cue == cue,]
  sub_tibble <- sub_tibble %>% mutate(token = factor(token, levels = token))
  plots[[cue]] <- ggplot(sub_tibble, aes(x = token, y = token_count)) + 
    geom_bar( stat = "identity", fill = ifelse(sub_tibble$token_count_neg < 0, 'grey20', 'grey80')) + 
    scale_y_continuous(breaks = c(0,25,50,75,100), limits = c(0,100)) +
    theme_minimal() + 
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size=16, hjust = 0.5), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, size = 14, hjust = 1),
      axis.text.y = element_text(size=14))
}

# fg7a: abortion
plots$abortion
ggsave(paste0('./figures/fg7a.eps'), dpi = 800)


# fg7b: taxes
plots$taxes
ggsave(paste0('./figures/fg7b.eps'), dpi = 800)


