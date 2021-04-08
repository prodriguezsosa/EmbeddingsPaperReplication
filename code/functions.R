# -----------------------
# turingTT package
# -----------------------

#' Bootstrap the performance statistic as explained in Rodriguez & Spirling 2020
#'
#' @param tt_data processed triad task app output
#' @param x the cue for which performance statistic is to be bootstrapped
#' @param overlap_prob numeric between 0-1, output of `compute_overlap_prob` for the corresponding models
#' @param num_iters number of bootstraps
#' @param seed seed for replication purposes
#' @return 
#'
#' @export
bootstrap_relative_performance <- function(tt_tibble, x = NULL, models = NULL, overlap_prob = NULL, num_iters = 100, seed = 1984L){
  set.seed(seed)
  tt_data <- tt_tibble %>% filter(left.source %in% models & right.source %in% models) %>% filter(cue == x)
  tt_data <- replicate(num_iters, sample_n(tt_data, size = nrow(tt_data), replace = TRUE), simplify = FALSE) 
  relative_performance <- tt_data %>% map(compute_relative_performance, models = models, overlap_prob = overlap_prob) %>% unlist()
  out <- tibble(cue = x, mu = mean(relative_performance), std.error = sd(relative_performance))
  return(out)
}

#' Compute the probability that the same word is drawn by chance given two lists of nearest neighbors
#'
#' @param nns1 character vector of nearest neighbors 1
#' @param nns2 character vector of nearest neighbors 2
#' @return the probability (numeric) that the same word is drawn by chance
#'
#' @export
compute_overlap_prob <- function(nns1, nns2){
  prob <- length(intersect(nns1, nns2))/length(nns1) # = p = probability of selecting a token that is in both lists
  overlap_prob <- prob^2 # probability of selecting the same token from each list = p*p
  return(overlap_prob)
}

#' Compute the performance statistic as explained in Rodriguez & Spirling 2020
#'
#' @param tt_data processed triad task app output
#' @param nns2 character vector specifying pair of models to compare (must match labels used in tt_data)
#' @param overlap_prob numeric between 0-1, output of `compute_overlap_prob` for the corresponding models
#' @return 
#'
#' @export
compute_relative_performance <- function(tt_data, models, overlap_prob = NULL){
  group_tallies <- table(tt_data$winner) # count the number of times each model was selected
  if(!is.null(overlap_prob)){
    non_overlap_stat <- (group_tallies[models[1]]/sum(group_tallies)) # models[2] is baseline
    ajusted_stat <- overlap_prob*0.5 + (1 - overlap_prob)*non_overlap_stat
    out <- unname(ajusted_stat/0.5) # divide by 0.5 such that 1 = indifference
  }else{
    out <- unname((group_tallies[models[1]]/sum(group_tallies))/0.5)
  }
  return(out)
}

#' Load and clean triad task app tt files
#'
#' @param file path to file
#' @return 
#'
#' @export
loadTT <- function(file){
  hit <- read.table(file, sep = ",", header = TRUE, stringsAsFactors = FALSE) # load hit
  hit <- hit[,2:ncol(hit)]  # remove row numbers
  hit$winner <- ifelse(hit$left.choice == "TRUE", hit$left.source, hit$right.source) # identify winner (i.e. model that was selected)
  return(hit)
}

# -----------------------
# weeval package
# -----------------------

#' Computes the average similarity vector between a set of cues and the full vocab over multiple embedding models
#'
#' @param embeds_list list of trained word embedding models (each is a V by D matrix)
#' @param cues a character vector of cue words for which the similarity vector will be computed
#' @param method a `param` of `text2vec`'s `sim2`: character, the similarity measure to be used. One of c("cosine","jaccard").
#' @param norm a `param` of `text2vec`'s `sim2`: character = c("l2","none") - how to scale input matrices. If they already scaled - use "none"
#' @return a matrix of similarity vectors, one row for each cue
#'
#' @note embedding models should not be averaged directly, instead, users should average
#' the similarity vectors. This function makes this easier. See Rodriguez & Spirling.
#'
#' @export
avg_cos_similarity <- function(embeds_list, cues, method = "cosine", norm = 'l2'){
  cos_sim <- lapply(embeds_list, function(embeds) sim2(x = embeds[cues, , drop = FALSE], y = embeds, method = method, norm = norm)) # compute cosine similarity vectors
  out <- Reduce('+', cos_sim)/length(cos_sim) # take mean over all initializations
  return(out)
}

#' Compute the correlation of a given cue's similarity vectors for two embedding models
#'
#' @param embeds1 embeddings model 1 (V by D matrix)
#' @param embeds2 embeddings model 2 (V by D matrix)
#' @param cue cue word for which the correlation will be computed
#' @param type `pearson` or `rank` correlation
#' @param method a `param` of `text2vec`'s `sim2`: character, the similarity measure to be used. One of c("cosine","jaccard").
#' @param norm a `param` of `text2vec`'s `sim2`: character = c("l2","none") - how to scale input matrices. If they already scaled - use "none"
#' @return a numeric value between -1 and 1
#'
#' @note embedding models will be subsetted to common vocabulary
#'
#' @export
corr_embeds <- function(embeds1, embeds2, cue, type = "pearson", method = "cosine", norm = "l2"){
  
  # identify common vocabulary (to be added: option to compare non-fully-overlapping vocabs)
  common_vocab <- intersect(rownames(embeds1), rownames(embeds2))
  
  # subset embeddings to common vocabulary
  embeds1 <- embeds1[common_vocab,]
  embeds2 <- embeds2[common_vocab,]
  
  # compute cosine distances (to cue) for each set of embeddings
  rank <- ifelse(type == "pearson", FALSE, TRUE)
  cos_dist <- lapply(list(embeds1, embeds2), function(x) cue_sim(cue, x, method = method, norm = norm, rank = rank))
  
  # bind vectors of cosine distances
  cos_dist <- do.call(cbind, cos_dist)
  
  # compute correlation
  if(type == "pearson"){
    corr_value <- cor(cos_dist, use = "complete.obs", method = "pearson")}else{
      corr_value <- cor.fk(cos_dist, y = NULL)
    }
  
  # keep only the diagonal value
  output <- corr_value[upper.tri(corr_value, diag = FALSE)]
  
  # return output
  return(output)
}

#' Compute correlations between two matrices of similarity vectors
#'
#' @param sims1 matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)
#' @param sims2 matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)
#' @param type `pearson` or `rank` correlation
#' @return a tibble with mean and standard error of correlation overt set of cues
#'
#' @note embedding models will be subsetted to common vocabulary
#'
#' @export
corr_sims <- function(sims1, sims2, type = "pearson"){
  
  
  # identify common vaocabulary (to be added: option to compare non-fully-overlapping vocabs)
  common_vocab <- intersect(colnames(sims1), colnames(sims2))
  
  # subset embeddings to common vocabulary
  sims1 <- sims1[,common_vocab]
  sims2 <- sims2[,common_vocab]
  
  # check cues are the same
  if(!all(nrow(sims1) == nrow(sims2) & all(rownames(sims1) %in% rownames(sims2)))){
    
    # identify common cues (to be added: option to compare non-fully-overlapping vocabs)
    common_cues <- intersect(rownames(sims1), rownames(sims2))
    
    # subset similarities to common cues
    sims1 <- sims1[common_cues,]
    sims2 <- sims2[common_cues,]}
  
  # compute correlation
  if(type == "pearson"){
    corr_value <- lapply(1:nrow(sims1), function(y) cor(sims1[y, ], sims2[y, ], use = "complete.obs", method = "pearson"))}else{
      corr_value <- lapply(1:nrow(sims1), function(y) cor.fk(sims1[y, ], sims2[y, ]))
    }
  
  # unlist
  output <- unlist(corr_value)
  
  # output
  if(length(output) == 1){
    return(tibble(mean = output, se = NA))}else{
      return(tibble(mean = mean(output), se = sd(output)/sqrt(length(output))))}
}

#' Compute similarity vector between a cue word and all words in the vocabulary of an embeddings model
#'
#' @param embeds embeddings model (V by D matrix)
#' @param cue cue word for which similarity vector will be computed
#' @param method a `param` of `text2vec`'s `sim2`: character, the similarity measure to be used. One of c("cosine","jaccard").
#' @param norm a `param` of `text2vec`'s `sim2`: character = c("l2","none") - how to scale input matrices. If they already scaled - use "none"
#' @param rank logical value indicating whether to return a vector of similarities or ranks
#' @return a numeric vector of cosine similarities (or ranks if indicated) between all words in the vocabulary and the cue word
#'
#' @export
cue_sim <- function(cue, embeds, method = "cosine", norm = "l2", rank = FALSE){
  cos_sim <- sim2(x = embeds, y = embeds[cue, , drop = FALSE], method = method, norm = norm)
  if(rank){return(rank((-1)*cos_sim[,1], ties.method = "random"))}else{  # mult by -1 b/c the base rank fcn. works in increasing order
    return(cos_sim[,1])
  }
}

#' Compute jaccard index of two embeddings models for N nearest neighbors
#'
#' @param sims1 matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)
#' @param sims2 matrix of similarities (e.g. cosine) between a set of cues (rows) and the full vocab (columns)
#' @param N number of nearest neighbors with which to compute jaccard index
#' @param common_vocab whether to use a common vocabulary to compute jaccard index
#' @return a tibble with mean and standard error of jaccard index overt set of cues
#'
#' @export
jaccard_sims <- function(sims1, sims2, N = 10, common_vocab = FALSE){
  
  # check N is less than or equal to smallest vocab
  if(N > min(ncol(sims1), ncol(sims2))) stop("N must be smaller than or equal to the smallest vocabulary")
  
  # check cues are the same
  if(!all(nrow(sims1) == nrow(sims2) & all(rownames(sims1) %in% rownames(sims2)))) stop("similarity matrices have different rownames (cues)")
  
  # if common_vocab == TRUE, subset to common vocab
  if(common_vocab){
    
    # identify common vocabulary
    common_vocab <- intersect(colnames(sims1), colnames(sims2))
    
    # subset embeddings to common vocabulary
    sims1 <- sims1[,common_vocab]
    sims2 <- sims2[,common_vocab]}
  
  # compute jaccard index
  jaccard_value <- lapply(1:nrow(sims1), function(x){
    nn1 <- sims1[x,][order(-sims1[x,])] %>% names() %>% .[2:(N + 1)] # exclude cue, always nearest neighbor
    nn2 <- sims2[x,][order(-sims2[x,])] %>% names() %>% .[2:(N + 1)] # exclude cue, always nearest neighbor
    return(length(intersect(nn1, nn2))/length(unique(nn1, nn2)))
  })
  
  # unlist
  output <- unlist(jaccard_value)
  
  # output
  if(length(output) == 1){
    return(tibble(mean = output, se = NA))}else{
      return(tibble(mean = mean(output), se = sd(output)/sqrt(length(output))))}
}

#' Find the nearest neighbors of a cue given a trained word embedding model
#'
#' @param embeds matrix of trained embeddings
#' @param cue cue word for which we want nearest neighbors
#' @param N number of nearest neighbors to return
#' @param method a `param` of `text2vec`'s `sim2`: character, the similarity measure to be used. One of c("cosine","jaccard").
#' @param norm a `param` of `text2vec`'s `sim2`: character = c("l2","none") - how to scale input matrices. If they already scaled - use "none"
#' @return a character vector of nearest neighbors of length N
#'
#' @note `context` the first nearest neighbor will be the cue itself.
#'
#' @export
nearest_neighbors <- function(cue, embeds, N = 5, method = "cosine", norm = "l2", drop_cue = TRUE){
  cos_sim <- sim2(x = embeds, y = embeds[cue, , drop = FALSE], method = method, norm = norm)
  nn <- cos_sim[order(-cos_sim),]
  if(drop_cue){return(names(nn)[2:(N + 1)])}else{
    return(names(nn)[1:N])}
}

#' Return nearest neighbors based on cosine similarity vector
#'
#' @param cue cue word for which nearest neighbors are required
#' @param sims matrix of similariteis (with cues as rownames)
#' @param N number of nearest neighbors to return
#' @param drop_cue drop nearest neighbor in top N count
#' @return a character vector of nearest neighbors to cue word
#'
#' @note cue is always the nearest neighbor hence can request to drop
#'
#' @export
#'
nn_sims <- function(cue, sims, N = 5, drop_cue = TRUE){
  cos_sim <- sims[cue,]
  nn <- cos_sim <- cos_sim[order(-cos_sim)]
  if(drop_cue){return(names(nn)[2:(N + 1)])}else{
    return(names(nn)[1:N])}
}

# -----------------------
# other
# -----------------------

# this function computes the rank deviation between an embedding model and human ranking of nearest neighbors (Figure 3 in paper and Figure 22 in Appendix)
compute_rank_deviation <- function(cue_i, model, avg_rank_vectors, sscorpus){
  
  # machine ranking
  machine_ranking <- avg_rank_vectors[[model]][cue_i,]
  
  # tally human mentions to get ranking
  human_ranking <- sscorpus$fluency %>% filter(cue == cue_i) %>% group_by(fluency) %>% tally() %>% arrange(-n) %>% mutate(n = 1:nrow(.))
  
  # subset to common vocab
  common_vocab <- intersect(names(machine_ranking), human_ranking$fluency)
  human_ranking <- human_ranking %>% filter(fluency %in% common_vocab)
  machine_ranking <- machine_ranking[common_vocab]
  machine_ranking <- tibble(fluency = names(machine_ranking), n = unname(machine_ranking))
  
  # merge
  ranking <- left_join(human_ranking, machine_ranking, by = 'fluency', suffix = c(".human", ".machine"))
  
  # compute log of rank difference
  ranking <- ranking %>% mutate(log_rank_dev = log((n.machine - n.human)^2 + 1))
  
  # return tibble of summary stats
  return(tibble(model = model, cue = cue_i, mu = mean(ranking$log_rank_dev), std.error = sd(ranking$log_rank_dev)/sqrt(nrow(ranking))))
  
}

