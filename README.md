README
================

# Word Embeddings

This repository contains the replication materials for the article
[“Word Embeddings: What works, what doesn’t and how to tell the
difference for applied
research”](https://github.com/ArthurSpirling/EmbeddingsPaper), to be
published in *The Journal of Politics*, by Pedro L. Rodriguez and Arthur
Spirling.

## Data

The full dataset is 126.75 GB. It includes all 1500 embedding models we
estimated along with all the files (raw and processed) required to
replicate our results. You can access all this data in [this Dropbox
folder](https://www.dropbox.com/sh/p2g0x7u1af0g1hv/AACSyEKbjPfo4sSZqFNGThgwa?dl=0).

## Required Software and Packages

`R (3.6.3)`:  
\- dplyr  
\- text2vec  
\- ggplot2  
\- quanteda  
\- read\_excel  
\- purrr  
\- reticulate  
\- stringr  
\- progress  
\- pbapply  
\- data.table  
\- magrittr  

`Python (3.7)`:  
\- gensim  

In addition to the above packages, we created a series of custom
functions to perform the analyses outlined in the paper. These are all
included in: `./code/functions.R`. We have created two packages based on
these functions that you can access for your own work (not used in the
replication code to avoid package updates from generating errors in
replication). The packages are:

  - [`weeval`](https://github.com/prodriguezsosa/weeval) contains all
    the function necessary to compute cosine similarities, average over
    several initializations of a given mode and compare models in terms
    of how they organize the semantic space.

  - [`turingTT`](https://github.com/prodriguezsosa/turingTT) contains
    the functions necessary to prepare and process the data for the
    Turing-style evaluation. You can check out the Shiny App
    [](https://prodriguezsosa.shinyapps.io/turingTT/). Note, to run the
    App with your own data you will need to fork or copy the code for
    the App. Code for the App is in the package repository under `\app`
    (it will not be downloaded with the package).  

To install them run the following commands in R:  

``` r
devtools::install_github("prodriguezsosa/weeval")
devtools::install_github("prodriguezsosa/turingTT")
```

For code to run the human context word generator task (first component
of the human evaluation) see the repo:
[`shinyGeNNs`](https://github.com/prodriguezsosa/shinyGeNNs). You can
check out the App [](https://prodriguezsosa.shinyapps.io/shinyGeNNs/).

## Corpora

We use the following 5 corpora (raw and processed data included in the
replication file):  

  - `cr` = Congressional Record (hein-bound)
    (<https://data.stanford.edu/congress_text>)
  - `ps` = (UK) Parliamentary Speeches
    (<https://www.english-corpora.org/hansard/>)
  - `sp` = Spanish Legislature
    (<https://github.com/prodriguezsosa/Text-Data>)
  - `gr` = German Legislature
    (<https://github.com/prodriguezsosa/Text-Data>)
  - `sotu` = State of the Union Speeches (R’s `quanteda` package)

## Pre-Trained Embeddings

We use the following 3 pre-trained embedding models (all three included
in the replication file):  

  - glove = English GloVe (<https://nlp.stanford.edu/projects/glove/>)
  - glove\_spanish = Spanish GloVe
    (<https://github.com/dccuchile/spanish-word-embeddings)\>
  - word2vec = English word2vec (Python’s gensim package)

## Estimation

In what follows we lay out the estimation procedure for all results
related to the Congressional Record corpus using GloVe (results included
in the paper). Results using the other corpora or word2vec (all included
in the appendix) follow a very similar procedure. Keep in mind most of
the estimation was done on a high performance cluster given the sheer
number of models that were estimated (1500 embedding models in total).
If you only wish to replicate the figures and tables given our estimated
models, jump to the next section.

1.  **Preprocessing:**
      - input: `./data/cr/raw/`
      - output: `./data/cr/corpus.rds`
      - code: `./code/estimation/preprocess_cr.R`
2.  **Estimation:**
      - input: `./data/cr/corpus.rds`
      - output: `./data/cr/glove/models/`
      - code: `./code/estimation/estimate_local_glove.R`
      - note: `stimate_local_glove.R` estimates a single model for a
        given pair of hyperparameter values (window size and embeddings
        dimensions). For each hyperparameter pair we estimate 10 models
        (10 different initializations). Given 25 hyperparameter pairs,
        we estimate 250 GloVe models. Doing this locally is
        prohibitively expensive as such we used NYU’s HPC to run the
        estimation script 250 times (10 times for each of the 25
        hyperparameter pairs). The same applies to other corpora and
        word2vec. Notice in the `estimate_local_glove.R` file you can
        select the corpus you wish to use.
3.  **Correlations:**
      - input: `./data/cr/glove/models/` and `data/pre-trained/`
      - output: `./data/cr/glove/correlations/`
      - code: `./code/estimation/compute_correlations.R`
      - note: `compute_correlations.R` computes the output necessary for
        Figures 4 - 6. It need only be run once (i.e. it takes in all
        models and computes the required statistics for all
        hyperparameter pairs) but keep in mind it can take a couple of
        hours to run given it goes throught all pairwise comparisons.
4.  **Context words generation (semantic fluency task):**
      - App: <https://prodriguezsosa.shinyapps.io/shinyGeNNs/>
      - App code: <https://github.com/prodriguezsosa/shinyGeNNs>
      - input (App input data):
        `./data/mturk/semantic_fluency_task/input_data/`
      - output-1 (amazon mechanical turk responses):
        `./data/mturk/semantic_fluency_task/output/`
      - output-2 (processed responses):
        `./data/mturk/semantic_fluency_task/processed/`
      - code-1 (prepare input data for App):
        `./code/estimation/prepare_sft_data.R`
      - code-2 (process App output): `./code/estimation/process_sft.R`
      - note: we used the `shinyGeNNs` App to have amazon mechanical
        turk workers generate candidate context words for our cues.
5.  **Turing test (triad task):**
      - App: <https://prodriguezsosa.shinyapps.io/turingTT/>
      - App code: <https://github.com/prodriguezsosa/turingTT>
      - input (App input data): `./data/mturk/triad_task/input_data/`
      - output (amazon mechanical turk responses):
        `./data/mturk/triad_task/output/`
      - code (prepare input data for App):
        `./code/estimation/prepare_triad_data.R`
      - note: we used the `turingTT` App to have amazon mechanical turk
        workers evaluate candidate context words and compare against our
        human basline (generated in step 4).

**Note:** follow the same procedure to replicate results for other
corpora. If a script is specific to a corpus, it will the corpus acronym
will be specified in the file name (e.g. `preprocess_ps.R` etc.). The
following estimation scripts apply to all corpora:
`estimate_local_glove.R` `compute_correlations.R`. Note, we only
estimates `word2vec` models and performed human evaluations for the
Congressional Record corpus so the corresponding scripts only apply to
`cr`.

## Replicate Figures and Tables

In the folder `/code/figures-tables/` you will find an `.R` script for
every figure and table both in the paper and the appendix. The script
names match those of the corresponding figure/table.
